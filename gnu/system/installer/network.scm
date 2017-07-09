;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 John Darrington <jmd@gnu.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu system installer network)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer ping)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (gnu system installer wireless)
  #:use-module (guix build syscalls)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (gurses menu)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (guix store)

  #:export (dhclient)
  #:export (make-network-page))

(include "i18n.scm")


(define (dhclient interface)
  (system* "dhclient" "-r" interface)
  (zero? (system* "dhclient" interface)))


(define (make-network-page parent title)
  (make-page (page-surface parent)
             title
             network-page-refresh
             0
             #:activator network-page-activate-item))

(define (interfaces)
  (map (lambda (ifce)
         `((name .  ,ifce)
           (class . ,(cond
                      ((loopback-network-interface? ifce) 'loopback)
                      ((string-prefix? "wl" ifce) 'wireless)
                      (else 'ethernet)))))
       (all-network-interface-names)))

(define (match->elem m match-number)
  (let ((elem (match:substring m match-number)))
    (if elem
        (string->number (string-drop elem 1))
        0)))

;; Convert a network device name such as "enp0s25" to
;; something more descriptive like
;; "82567LM Gigabit Network Connection"
(define (name->description name)
  (if (string=? name "lo")
      "Loop back interface"
      (let ((m (string-match "^..(P[[:digit:]]+)?(p[[:digit:]]+)(s[[:digit:]]+)(f[[:digit:]]+)?(u[[:digit:]]+)?" name)))
        (if (not m)
            name
            (let ((domain   (match->elem m 1))
                  (bus      (match->elem m 2))
                  (slot     (match->elem m 3))
                  (func     (match->elem m 4))
                  (usb-slot (match->elem m 5)))
              (assoc-ref
                (map
                 (lambda (x)
                  (let ((idx (string-index x #\:)))
                    (cons (substring x 0 idx)
                          (string-trim
                           (substring x (1+ idx))))))
                 (apply slurp*
                        "lspci"
                        (list "-v" "-mm" (format #f "-s~x:~x:~x.~x"
                                                    domain bus slot func))
                ; TODO lsusb -s 2:2 (in decimal); first is bus number.
                ; TODO traverse full port chain.
                ; TODO check /sys/class/net/wlp0s29f7u2/phy80211
                           ))
               "Device"))))))

(define my-buttons `((continue ,(M_ "_Continue") #t)
                     (test     ,(M_ "_Test") #t)))

(define (network-page-activate-item page xitem)
  (match xitem
   (('menu-item-activated item)
    (let ((item-name (and item (assq-ref item 'name)))
          (item-class (and item (assq-ref item 'class))))
      (match item-class
       ('wireless
        (let ((next (make-wireless-page page (M_ "Wireless interface setup")
                                        item-name)))
          (page-enter next)))
       ('ethernet
        (and (zero? (system* "ip" "link" "set" item-name "up"))
             (dhclient item-name)))
       (_ 'x))
      'handled))
   ('test
    (let ((next  (make-page (page-surface page)
                            "Ping"
                             ping-page-refresh
                             0
                             #:activator ping-page-activate-item)))
      (page-enter next)
      'handled))
   ('continue
    ;; Cancel the timer
    (setitimer ITIMER_REAL 0 0 0 0)
    (page-leave)
    'handled)
   (_ #f)))

(define (network-page-refresh page)
  (when (not (page-initialised? page))
    (network-page-init page)
    (page-set-initialised! page #t))
  (let ((text-window (page-datum page 'text-window)))
    (erase text-window)
    (addstr* text-window (format #f
      (gettext "To install GuixSD a connection to one of ~s must be available.  The following network devices exist on the system.  Select one to configure or \"Continue\" to proceeed.") %default-substitute-urls))))

(define (if-flags ifce)
  (network-interface-flags
   (socket SOCK_STREAM AF_INET 0)
   (assq-ref ifce 'name)))

(define (network-page-init p)
  (define prev-flags (map-in-order if-flags (interfaces)))
  (match (create-vbox (page-surface p) 5 (- (getmaxy (page-surface p)) 5 3) 3)
   ((text-window mwin bwin)
    (let ((buttons (make-buttons my-buttons 1))
          (menu (make-menu
                  (filter (lambda (i) (memq
                                     (assq-ref i 'class)
                                     '(ethernet wireless)))
                        (interfaces))
                  #:disp-proc
                  (lambda (datum row)
                     (format #f "~55a (~a) (status: ~a)"
                             (name->description (assq-ref datum 'name))
                             (assq-ref datum 'class)
                             (if (network-interface-running? (assq-ref datum 'name))
                                 (gettext "Running")
                                 (gettext "Down")))))))

      ;; Raise sigalarm every second to refresh the menu
      (sigaction SIGALRM (lambda (_)
                           (let ((flags
                                  (map-in-order
                                   if-flags
                                   (interfaces))))

                             (when (not (equal? prev-flags flags))
                                   (set! prev-flags flags)
                                   (menu-redraw menu)))))
      (setitimer ITIMER_REAL 1 0 1 0)

      (push-cursor (page-cursor-visibility p))
      (page-set-datum! p 'menu menu)
      (page-set-datum! p 'navigation buttons)
      (page-set-datum! p 'text-window text-window)
      (menu-post menu mwin)
      (buttons-post buttons bwin)))))
