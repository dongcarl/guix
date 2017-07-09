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

(define-module (gnu system installer wireless)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer ping)
  #:use-module (gnu system installer passphrase)
  #:use-module (gnu system installer network)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (gurses menu)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (guix store)
  #:use-module (guix utils)

  #:export (wireless-connect)
  #:export (make-wireless-page))

(include "i18n.scm")

(define (make-wireless-page parent title interface)
  (let ((page (make-page (page-surface parent)
                         title
                         wireless-page-refresh
                         0
                         #:activator wireless-page-activate-item)))

    (page-set-datum! page 'ifce interface)
    page))


(define my-buttons `((cancel ,(M_ "Canc_el") #t)))

(define (wireless-page-activate-item page item)
  (match item
   (('menu-item-activated ap)
    (let ((ifce (page-datum page 'ifce)))
      (if (assq-ref ap 'encryption)
          (let ((next (make-passphrase-page
                       page
                       (M_ "Passphrase entry")
                       ifce
                       ap)))
            (page-enter next))
          (begin
            (and (zero? (system* "ip" "link" "set" ifce "up"))
                 (zero? (system* "iw" "dev" ifce "connect" (assq-ref ap 'essid)))
                 (dhclient ifce))
            (page-leave))))
    'handled)
   ('cancel
    (page-leave)
    'handled)
   (_
    'ignored)))

(define (wireless-page-refresh page)
  (when (not (page-initialised? page))
    (wireless-page-init page)
    (page-set-initialised! page #t))
  (let ((text-window (page-datum page 'text-window)))
    (erase text-window)
    (addstr*   text-window  (format #f
                                    (gettext
                                     "Select an access point to connect.")))))

(define (wireless-page-init p)
  (match (create-vbox (page-surface p) 5 (- (getmaxy (page-surface p)) 5 3) 3)
   ((text-window mwin bwin)
    (let* ((buttons (make-buttons my-buttons 1))
           (menu (make-menu
                ;; Present a menu of available Access points in decreasing
                ;; order of signal strength
                (sort
                 (get-wifi
                  (page-datum p 'ifce))
                 (lambda (i j)
                   (<
                    (assq-ref j 'signal)
                    (assq-ref i 'signal))))
                #:disp-proc
                (lambda (d _)
                  (format #f "~30a ~a" (assq-ref d 'essid)
                          (or
                           (and=> (assq-ref d 'encryption)
                                  symbol->string)
                            (M_ "clear")))))))
      (push-cursor (page-cursor-visibility p))
      (page-set-datum! p 'menu menu)
      (page-set-datum! p 'navigation buttons)
      (page-set-datum! p 'text-window text-window)
      (menu-post menu mwin)
      (buttons-post buttons bwin)))))



(use-modules (ice-9 pretty-print))
(use-modules (ice-9 regex))
(use-modules (srfi srfi-1))

(define (drop-quotes s)
  "Drop any double quote characters from S"
  (string-fold
   (lambda (c prev)
     (string-append
      prev
      (if (eq? c #\") "" (make-string 1 c))))
   "" s))

(define (scan-wifi ifce)
  (match (map string-trim-both (slurp* "iwlist" ifce "scan"))
    (#f '())
    ((_ . lines) lines))) ;; Ignore the first line

(define (drop-prefix pfx s)
  "Drop PFX from S if it is the first string"
  (if (string-prefix? pfx s)
      (string-drop s (string-length pfx))
      s))

(define (get-wifi ifce)
  (system* "ip" "link" "set" ifce "up")
  (fold
   (lambda (x prev)
     (let ((mtch (string-match "Cell [0-9][0-9] - " x)))
       (cond (mtch
              (cons
               (list
                `(address . ,
                          (drop-prefix "Address: "
                                       (string-drop x (string-length (match:substring mtch))))))
               prev))

             ((string-prefix? "Encryption key:" x)
              (cons
               (append (car prev)
                       (list `(encryption .
                                          ;; Assume WEP encryption until we know
                                          ;; otherwise.
                                          ,(if (string-suffix? "on" x) 'wep #f))))
               (cdr prev)))

             ((string-match "^IE:  *IEEE 802.11i" x)
              ;; If we find the above string, and encryption is on, then the encrpytion
              ;; type is probably WPA2
              (let ((current-ap (car prev)))
                (if (assq-ref current-ap 'encryption)
                    (cons  (assq-set! current-ap 'encryption 'wpa2)
                           (cdr prev))
                    prev)))

             ((string-prefix? "Quality=" x)
              (let ((lvl (string-match "level=(-?[0-9][0-9]*) dBm" x)))
                (if lvl
                    (cons
                     (append (car prev)
                             (list
                              `(signal . ,(string->number (match:substring lvl 1))))
                             )
                     (cdr prev))
                    prev)))

             ((string-prefix? "ESSID:" x)
              (cons
               (append (car prev)
                       (list
                        `(essid . ,(drop-prefix "ESSID:"
                                                (drop-quotes
                                                 x))))
                       )
               (cdr prev)))

             (else
              prev))))
   '() (scan-wifi ifce)))



(define (start-interface config-file ifce)
  (let ((pid-file (format #f "/wpspid-~a" ifce)))
    (catch #t
           (lambda () (kill (string->number
                             (read-line (open pid-file O_RDONLY))) SIGINT))
           (lambda (key . args) #t))
    (zero? (system* "wpa_supplicant"
                    "-c" config-file
                    "-P" pid-file
                    "-i" ifce
                    "-B"))))

(define (wireless-connect ifce access-point passphrase)
  "Connect the wireless interface IFCE to ACCESS-POINT using the key PASSPHRASE."

  (let ((essid (assq-ref access-point 'essid))
        (encr (assq-ref access-point 'encryption)))

    (call-with-temporary-output-file
     (lambda (filename port)
       (format port
               (if (eq? encr 'wep) "
network={
\tssid=\"~a\"
\tkey_mgmt=NONE
\twep_key0=\"~a\"
}
"
                   "
network={
\tssid=\"~a\"
\tkey_mgmt=WPA-PSK
\tpsk=\"~a\"
}
")
               essid
               passphrase)
       (force-output port)

       (with-output-to-file "/dev/null"
         (lambda ()
           (and (start-interface filename ifce)
                (dhclient ifce))))))))
