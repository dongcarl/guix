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
  #:use-module (gurses form)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (ice-9 regex)

  #:export (make-passphrase-page))

(define max-length ((const 60)))

(define my-fields `((passphrase   ,(N_ "Passphrase") ,max-length)))

(define (make-passphrase-page parent title ifce network)
  (let ((page
         (make-page (page-surface parent)
                    title
                    passphrase-refresh
                    1
                    passphrase-key-handler)))
    (page-set-datum! page 'network network)
    (page-set-datum! page 'ifce ifce)
    page))

(define (passphrase-refresh page)
  (when (not (page-initialised? page))
    (passphrase-init page)
    (page-set-initialised! page #t))

  (let ((form  (page-datum page 'form))
        (text-window (page-datum page 'text-window)))
    (clear text-window)
    (addstr*
     text-window
     (gettext
      (format #f "Enter the passphrase for the network ~a."
              (page-datum page 'network))))
    (refresh text-window)
    (refresh (outer (page-wwin page)))
    (refresh (form-window form))))

(define (passphrase-key-handler page ch)
  (let ((form  (page-datum page 'form))
        (nav   (page-datum page 'navigation))
        (dev   (page-datum page 'device)))

    (cond
     ((buttons-key-matches-symbol? nav ch 'back)
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
       (page-datum page 'network)
       (form-get-value form 'passphrase))
      (page-leave (cdr (cdr page-stack))))

     (else
      (form-enter form ch)))
    #f))

(define my-buttons `((back ,(N_ "Back") #f)))

(define (passphrase-init p)
  (let* ((s (page-surface p))
         (pr (make-boxed-window
              #f
              (- (getmaxy s) 4) (- (getmaxx s) 2)
              2 1
              #:title (page-title p)))

         (text-window (derwin (inner pr) 5 (getmaxx (inner pr))
                              0 0))

         (bwin (derwin (inner pr)
                       3 (getmaxx (inner pr))
                       (- (getmaxy (inner pr)) 3) 0
                       #:panel #f))

         (nav (make-buttons my-buttons 1))

         (fw (derwin (inner pr)
                     2
                     (getmaxx (inner pr))
                     (getmaxy text-window) 0))


         (form (make-form my-fields)))

    (push-cursor (page-cursor-visibility p))
    (page-set-datum! p 'navigation nav)
    (page-set-datum! p 'text-window text-window)
    (page-set-datum! p 'form form)

    (form-post form fw)
    (buttons-post nav bwin)
    (page-set-wwin! p pr)
    (refresh (outer pr))))
