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

  #:export (make-dialog))

(include "i18n.scm")

;; This module creates a single dialog with a simple message and an OK
;; button.

(define* (make-dialog parent message #:key (justify #t))
  (let ((page (make-page (page-surface parent)
			(gettext "Information")
			dialog-page-refresh
                        0
			dialog-page-key-handler)))
    (page-set-datum! page 'message message)
    (page-set-datum! page 'justify justify)
    page))


(define my-buttons `((ok  ,(M_ "_OK") #t)))

(define (dialog-page-key-handler page ch)
  (let ((nav  (page-datum page 'navigation)))

    (cond
     ((eq? ch #\tab)
      (cond
       ((eqv? (buttons-selected nav) (1- (buttons-n-buttons nav)))
	(buttons-unselect-all nav))

       (else
	(buttons-select-next nav))))

     ((buttons-key-matches-symbol? nav ch 'ok)

      (delwin (page-datum page 'text-window))
      (page-leave)
      ))
    #f))

(define (dialog-page-refresh page)
  (when (not (page-initialised? page))
    (dialog-page-init page)
    (page-set-initialised! page #t))
  (refresh* (page-datum page 'text-window)))

(define (dialog-page-init p)
  (let* ((s (page-surface p))
	 (frame (make-boxed-window  #f
				    (- (getmaxy s) 5) (- (getmaxx s) 2)
				    2 1
				    #:title (page-title p)))
	 (button-window (derwin (inner frame)
				3 (getmaxx (inner frame))
				(- (getmaxy (inner frame)) 3) 0
				#:panel #f))
	 (buttons (make-buttons my-buttons 1))

	 (text-window (derwin (inner frame)
			      (- (getmaxy (inner frame)) (getmaxy button-window))
			      (getmaxx (inner frame))
			      0 0 #:panel #f)))

    (let ((m (page-datum p 'message))
	  (justify (page-datum p 'justify)))
      (if justify
	  (addstr* text-window (gettext m))
	  (addstr text-window (gettext m))))

  (push-cursor (page-cursor-visibility p))
  (page-set-wwin! p frame)
  (page-set-datum! p 'text-window text-window)
  (page-set-datum! p 'navigation buttons)
  (buttons-post buttons button-window)
  (buttons-select buttons 0)
  (refresh* (outer frame))
  (refresh* (inner frame))
  (refresh* text-window)
  (refresh* button-window)))



