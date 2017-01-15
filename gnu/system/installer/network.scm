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

  #:export (make-network-page))

(define-syntax M_
  (syntax-rules ()
    ((M_ str)
     str)))


(define (make-network-page parent  title)
  (make-page (page-surface parent)
	     title
	     network-page-refresh
             0
	     network-page-key-handler))

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
      (let ((m (string-match "^..(P[[:digit:]]+)?(p[[:digit:]]+)(s[[:digit:]]+)(f[[:digit:]]+)?" name)))
        (if (not m)
            name
            (let ((domain  (match->elem m 1))
                  (bus     (match->elem m 2))
                  (slot    (match->elem m 3))
                  (func    (match->elem m 4)))
              (assoc-ref
               (slurp
                (format #f "lspci -v -mm -s~x:~x:~x.~x"
                        domain bus slot func)
                (lambda (x)
                  (let ((idx (string-index x #\:)))
                    (cons (substring x 0 idx)
                          (string-trim
                           (substring x (1+ idx)))))))
               "Device"))))))

(define my-buttons `((continue ,(M_ "_Continue") #t)
		     (test     ,(M_ "_Test") #t)))

(define (network-page-key-handler page ch)
  (let ((menu (page-datum page 'menu))
	(nav  (page-datum page 'navigation)))

    (cond
     ((eq? ch KEY_RIGHT)
      (menu-set-active! menu #f)
      (buttons-select-next nav))

     ((eq? ch #\tab)
      (cond
       ((menu-active menu)
        (menu-set-active! menu #f)
        (buttons-select nav 0))

       ((eqv? (buttons-selected nav) (1- (buttons-n-buttons nav)))
	(menu-set-active! menu #t)
	(buttons-unselect-all nav))

       (else
	(buttons-select-next nav))))

     ((eq? ch KEY_LEFT)
      (menu-set-active! menu #f)
      (buttons-select-prev nav))

     ((eq? ch KEY_UP)
      (buttons-unselect-all nav)
      (menu-set-active! menu #t))

     ((and (select-key? ch)
           (eq? 'wireless (assq-ref (menu-get-current-item menu) 'class)))

      (let ((next (make-wireless-page page (M_ "Wireless interface setup")
                                      (assq-ref (menu-get-current-item menu) 'name))))
        (page-enter next)))

     ((select-key? ch)
      (let ((item (menu-get-current-item menu)))
        (when (eq? (assq-ref item 'class) 'ethernet)
          (and (zero? (system* "ip" "link" "set" (assq-ref item 'name) "up"))
               (zero? (system* "dhclient" (assq-ref item 'name)))))))

     ((buttons-key-matches-symbol? nav ch 'test)
      (let ((next  (make-page (page-surface page)
                              "Ping"
                              ping-page-refresh
                              0
                              ping-page-key-handler)))
        (page-enter next)))

     ((buttons-key-matches-symbol? nav ch 'continue)

      ;; Cancel the timer
      (setitimer ITIMER_REAL 0 0 0 0)

      (page-leave)))

    (std-menu-key-handler menu ch))
  #f)


(define (network-page-refresh page)
  (when (not (page-initialised? page))
    (network-page-init page)
    (page-set-initialised! page #t))
  (touchwin (outer (page-wwin page)))
  (refresh (outer (page-wwin page)))
  (refresh (inner (page-wwin page)))
  (menu-refresh (page-datum page 'menu)))


(define (network-page-init p)
  (let* ((s (page-surface p))
	 (pr (make-boxed-window  #f
                                 (- (getmaxy s) 4) (- (getmaxx s) 2)
                                 2 1
                                 #:title (page-title p)))
	 (text-window (derwin
		       (inner pr)
		       5 (getmaxx (inner pr))
		       0 0
		       #:panel #f))

	 (bwin (derwin (inner pr)
		       3 (getmaxx (inner pr))
		       (- (getmaxy (inner pr)) 3) 0
                       #:panel #f))
	 (buttons (make-buttons my-buttons 1))

	 (mwin (derwin (inner pr)
		       (- (getmaxy (inner pr)) (getmaxy text-window) 3)
		       (- (getmaxx (inner pr)) 0)
		       (getmaxy text-window) 0 #:panel #f))

	 (menu (make-menu
	        (filter (lambda (i) (memq
                                     (assq-ref i 'class)
                                     '(ethernet wireless)))
                        (interfaces))
	        #:disp-proc
	        (lambda (datum row)
                     (format #f "~55a ~a"
                             (name->description (assq-ref datum 'name))
                             (if (zero? (logand IFF_RUNNING
                                         (network-interface-flags
                                          (socket SOCK_STREAM AF_INET 0)
                                          (assq-ref datum 'name))))
                                 (gettext "Down")
                                 (gettext "Running")))))))

    (addstr*   text-window  (format #f
                                    (gettext
                                     "To install GuixSD a connection to one of ~s must be available.  The following network devices exist on the system.  Select one to configure or \"Continue\" to proceeed.") %default-substitute-urls))


    ;; Raise sigalarm every second to refresh the menu
    (sigaction SIGALRM (lambda (_) (menu-redraw menu)))
    (setitimer ITIMER_REAL 1 0 1 0)

    (push-cursor (page-cursor-visibility p))
    (page-set-wwin! p pr)
    (page-set-datum! p 'menu menu)
    (page-set-datum! p 'navigation buttons)
    (menu-post menu mwin)
    (buttons-post buttons bwin)
    (refresh (outer pr))
    (refresh text-window)
    (refresh bwin)))
