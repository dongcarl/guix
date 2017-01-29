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
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (gurses menu)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (guix store)
  #:use-module (guix utils)

  #:export (wireless-connect)
  #:export (make-wireless-page))

(define-syntax M_
  (syntax-rules ()
    ((M_ str)
     str)))


(define (make-wireless-page parent title interface)
  (let ((page (make-page (page-surface parent)
                         title
                         wireless-page-refresh
                         0
                         wireless-page-key-handler)))

    (page-set-datum! page 'ifce interface)
    page))


(define my-buttons `((cancel ,(M_ "Canc_el") #t)))

(define (wireless-page-key-handler page ch)
  (let ((nav  (page-datum page 'navigation))
        (menu  (page-datum page 'menu))
        (test-window  (page-datum page 'test-window)))

    (cond
     ((eq? ch KEY_RIGHT)
      (buttons-select-next nav))

     ((eq? ch #\tab)
      (cond
       ((eqv? (buttons-selected nav) (1- (buttons-n-buttons nav)))
        (buttons-unselect-all nav))

       (else
        (buttons-select-next nav))))

     ((eq? ch KEY_LEFT)
      (buttons-select-prev nav))

     ((eq? ch KEY_UP)
      (buttons-unselect-all nav))

     ((buttons-key-matches-symbol? nav ch 'cancel)
      (page-leave))

     ((select-key? ch)
      (let ((ap (menu-get-current-item menu))
            (ifce (page-datum page 'ifce)))
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
                   (zero? (system* "dhclient" ifce)))
              (page-leave))))))

    (std-menu-key-handler menu ch)

    #f))

(define (wireless-page-refresh page)
  (when (not (page-initialised? page))
    (wireless-page-init page)
    (page-set-initialised! page #t))
  (touchwin (outer (page-wwin page)))
  (refresh* (outer (page-wwin page)))
  (refresh* (inner (page-wwin page)))
  (menu-refresh (page-datum page 'menu)))


(define (wireless-page-init p)
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

    (addstr*   text-window  (format #f
                                    (gettext
                                     "Select an access point to connect.")))

    (push-cursor (page-cursor-visibility p))
    (page-set-wwin! p pr)
    (page-set-datum! p 'menu menu)
    (page-set-datum! p 'navigation buttons)
    (menu-post menu mwin)
    (buttons-post buttons bwin)
    (refresh* (outer pr))
    (refresh* text-window)
    (refresh* bwin)))



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
  (match  (slurp (string-append "iwlist " ifce " scan") string-trim-both)
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
           (and (zero? (system* "wpa_supplicant" "-c" filename "-i" ifce "-B"))
                (zero? (system* "dhclient" ifce)))))))))
