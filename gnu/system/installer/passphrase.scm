;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 John Darrington <jmd@gnu.org>
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

(define-module (gnu system installer passphrase)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (gnu system installer wireless)
  #:use-module (gnu system installer levelled-stack)
  #:use-module (gurses form)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)

  #:export (make-passphrase-page))

(include "i18n.scm")

(define max-length ((const 60)))

(define my-fields `((passphrase   ,(M_ "Passphrase") ,max-length)))

(define (make-passphrase-page parent title ifce access-point)
  (let ((page
         (make-page (page-surface parent)
                    title
                    passphrase-refresh
                    1
                    passphrase-key-handler
                    passphrase-mouse-handler)))
    (page-set-datum! page 'access-point access-point)
    (page-set-datum! page 'ifce ifce)
    page))

(define (passphrase-refresh page)
  (when (not (page-initialised? page))
    (passphrase-init page)
    (page-set-initialised! page #t))

  (let ((form  (page-datum page 'form))
        (access-point (page-datum page 'access-point))
        (text-window (page-datum page 'text-window)))
    (erase text-window)
    (addstr*
     text-window
     (gettext
      (format #f "Enter the passphrase for the network ~a."
              (assq-ref access-point 'essid))))))

(define (passphrase-mouse-handler page device-id x y z button-state)
  'ignored)

(define (passphrase-key-handler page ch)
  (let ((form  (page-datum page 'form))
        (nav   (page-datum page 'navigation))
        (access-point (page-datum page 'access-point))
        (dev   (page-datum page 'device)))

    (cond
     ((buttons-key-matches-symbol? nav ch 'cancel)
      (page-leave))

     ((eq? ch #\tab)
      (form-set-enabled! form #f)
      (buttons-select-next nav))

     ((eq? ch KEY_UP)
      (buttons-unselect-all nav)
      (form-set-enabled! form #t))

     ((eq? ch KEY_DOWN)
      (buttons-unselect-all nav)
      (form-set-enabled! form #t))

     ((select-key? ch)
      (wireless-connect
       (page-datum page 'ifce)
       access-point
       (form-get-value form 'passphrase))
      (page-pop)
      (page-leave))

     (else
      (form-enter form ch)))
    #f))

(define my-buttons `((cancel ,(M_ "Cancel") #f)))

(define (passphrase-init p)
  (match (create-vbox (page-surface p) 5 (- (getmaxy (page-surface p)) 5 3) 3)
   ((text-window fw bwin)
    (let ((nav (make-buttons my-buttons))
          (form (make-form my-fields)))

      (push-cursor (page-cursor-visibility p))
      (page-set-datum! p 'navigation nav)
      (page-set-datum! p 'text-window text-window)
      (page-set-datum! p 'form form)

      (form-post form fw)
      (buttons-post nav bwin)))))
