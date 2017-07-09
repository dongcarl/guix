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

(define-module (gnu system installer locale)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (gnu system locale)
  #:use-module (gurses menu)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (ice-9 format)
  #:use-module (ice-9 poe)
  #:use-module (ice-9 match)
  #:export (make-locale-page))

(include "i18n.scm")

(define my-buttons `((cancel ,(M_ "Canc_el") #t)))

(define (make-locale-page parent  title)
  (make-page (page-surface parent)
             title
             locale-page-refresh
             0
             #:activator locale-page-activate-item))

(define (locale-page-refresh page)
  (when (not (page-initialised? page))
    (locale-page-init page)
    (page-set-initialised! page #t))

  (let ((text-window (page-datum page 'text-window))
        (menu (page-datum page 'menu)))
    (clear text-window)
    (addstr text-window
      (justify* (gettext "The following languages are available.")
                (getmaxx text-window)))))

(define (locale-page-activate-item page item)
  (match item
   (('menu-item-activated locale)
    (setlocale LC_ALL (locale-definition-name locale))
    (page-leave)
    'handled)
   ('cancel
    (page-leave)
    'handled)
   (_
    'ignored)))

(define (locale-descriptionx locale)
  "Return a string describing LOCALE"
  (define loc #f)
  (define lc-all "LC_ALL")
  (dynamic-wind
      (lambda () (set! loc (getenv lc-all))
              (setenv lc-all locale))
      (lambda () (let ((str (assq-ref (key-value-slurp* "locale" "-k" "LC_IDENTIFICATION")
                                      'title)))
                   ;; String enclosing "" if they exist
                   (if (and (eqv? (string-ref str 0) #\")
                            (eqv? (string-ref str (1- (string-length str))) #\"))
                       (substring str 1 (1- (string-length str)))
                       str)))
      (lambda ()
        (if loc
            (setenv lc-all loc)
            (unsetenv lc-all)))))

(define locale-description
  (pure-funcq locale-descriptionx))

(define (locale-page-init p)
  (match (create-vbox (page-surface p) 4 (- (getmaxy (page-surface p)) 4 3) 3)
   ((text-window menu-window button-window)
    (let ((buttons (make-buttons my-buttons 1))
          (menu (make-menu %default-locale-definitions
                          #:disp-proc (lambda (d row)
                                        (format #f "~60a ~10a"
                                        (locale-description
                                         (locale-definition-name d))
                                         (locale-definition-name d))))))
      (push-cursor (page-cursor-visibility p))
      (page-set-datum! p 'text-window text-window)
      (page-set-datum! p 'menu menu)
      (page-set-datum! p 'navigation buttons)
      (menu-post menu menu-window)
      (buttons-post buttons button-window)))))
