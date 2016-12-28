;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
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
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (gurses menu)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (guix store)
  #:use-module (guix utils)

  #:export (make-essid-page))


(define (make-essid-page parent title interface)
  (let ((page (make-page (page-surface parent)
                         title
                         essid-page-refresh
                         essid-page-key-handler)))

    (page-set-datum! page 'ifce interface)
    page))


(define my-buttons `((continue ,(N_ "_Continue") #t)))

(define (essid-page-key-handler page ch)

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


     ((buttons-key-matches-symbol? nav ch 'continue)

      (with-output-to-file "/tmp/wpa_supplicant.conf"
        (lambda ()
         (format #t "
network={
\tssid=\"~a\"
\tkey_mgmt=WPA-PSK
\tpsk=\"~a\"
}
"
                 (assq-ref (menu-get-current-item menu) 'essid)
                 "Passphrase")))

      (and (zero? (system* "wpa_supplicant" "-c" "/tmp/wpa_supplicant.conf" "-i"
               (page-datum page 'ifce)
               "-B"))
           (zero? (system* "dhclient" (page-datum page 'ifce))))

      (delwin (outer (page-wwin page)))
      (delwin (inner (page-wwin page)))

      (set! page-stack (cdr page-stack))))


    (std-menu-key-handler menu ch)

    #f))

(define (essid-page-refresh page)
  (when (not (page-initialised? page))
    (essid-page-init page)
    (page-set-initialised! page #t))
  (touchwin (outer (page-wwin page)))
  (refresh (outer (page-wwin page)))
  (refresh (inner (page-wwin page)))
  (menu-refresh (page-datum page 'menu)))


(define (essid-page-init p)
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
                (lambda (d _) (assq-ref d 'essid)))
               ))

    (addstr*   text-window  (format #f
	      (gettext
	       "Select an access point to connect.")))

    (page-set-wwin! p pr)
    (page-set-datum! p 'menu menu)
    (page-set-datum! p 'navigation buttons)
    (menu-post menu mwin)
    (buttons-post buttons bwin)
    (refresh (outer pr))
    (refresh text-window)
    (refresh bwin)))
			


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
  (begin (system* "ip" "link" "set" ifce "up")
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
                                                 ,(string-suffix? "on" x))))
                      (cdr prev)))

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
          '() (scan-wifi ifce))))
