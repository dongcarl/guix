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

(define-module (gnu system installer key-map)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer utils)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer levelled-stack)
  #:use-module (gurses menu)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (ice-9 match)

  #:export (make-key-map))

(include "i18n.scm")

(define* (make-key-map parent directory)
  (let ((page (make-page (page-surface parent)
                         (gettext "Keyboard Mapping")
                         key-map-page-refresh
                         0
                         #:activator key-map-page-activate-item)))
    (page-set-datum! page 'directory directory)
    page))


(define my-buttons `((cancel  ,(M_ "Canc_el") #t)))

(define (key-map-page-activate-item page item)
  (match item
   (('menu-item-activated i)
    (let* ((directory (page-datum page 'directory))
           (new-dir (string-append directory i)))
      (if (eq? 'directory (stat:type (stat new-dir)))
        (let ((p (make-key-map page new-dir)))
          (page-pop) ; Don't go back to the current page!
          (page-enter p))
        (begin
          (system* "loadkeys" i)
          (set! key-map i)
          (page-leave)
          'handled))))
   ('cancel
    (page-leave)
    'handled)
   (_ 'ignored)))

(define (key-map-page-refresh page)
  (when (not (page-initialised? page))
    (key-map-page-init page)
    (page-set-initialised! page #t))
  ;auto (menu-refresh (page-datum page 'menu))
  )

(define (key-map-page-init p)
  (match (create-vbox (page-surface p) 4 (- (getmaxy (page-surface p)) 3 4) 3)
   ((text-window menu-window button-window)
    (let ((buttons (make-buttons my-buttons))
          (menu (make-menu
		(let ((dir (page-datum p 'directory)))
                  (filter (lambda (name)
                            (and (not (string=? name "./"))
                                 (not (string=? name "include/"))))
                          (scandir-with-slashes dir))))))
      (menu-post menu menu-window)

      (addstr* text-window (gettext "Select an item most closely matching your keyboard layout:" ))
      (push-cursor (page-cursor-visibility p))
      (page-set-datum! p 'menu menu)
      (page-set-datum! p 'navigation buttons)
      (buttons-post buttons button-window)))))
