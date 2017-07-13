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

(define-module (gnu system installer dialog)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer utils)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (ice-9 match)

  #:export (make-dialog))

(include "i18n.scm")

;; This module creates a single dialog with a simple message and an OK
;; button.

(define* (make-dialog parent message #:key (justify #t))
  (let ((page (make-page (page-surface parent)
			(gettext "Information")
			dialog-page-refresh
                        0
			#:activator dialog-page-activate-item)))
    (page-set-datum! page 'message message)
    (page-set-datum! page 'justify justify)
    page))


(define my-buttons `((ok  ,(M_ "_OK") #t)))

(define (dialog-page-activate-item page item)
  (match item
   ('ok
      (delwin (page-datum page 'text-window))
      (page-leave)
      'handled)))

(define (dialog-page-refresh page)
  (when (not (page-initialised? page))
    (dialog-page-init page)
    (page-set-initialised! page #t))
  (let ((text-window (page-datum page 'text-window))
        (m (page-datum page 'message))
        (justify (page-datum page 'justify)))
    (erase text-window)
    (if justify
        (addstr* text-window (gettext m))
        (addstr text-window (gettext m)))))

(define (dialog-page-init p)
  (match (create-vbox (page-surface p) (- (getmaxy (page-surface p)) 3) 3)
   ((text-window button-window)
    (let ((buttons (make-buttons my-buttons)))
      (push-cursor (page-cursor-visibility p))
      (page-set-datum! p 'text-window text-window)
      (page-set-datum! p 'navigation buttons)
      (buttons-post buttons button-window)
      (buttons-select buttons 0)))))
