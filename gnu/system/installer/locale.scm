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
  #:export (make-locale-page))

(include "i18n.scm")

(define my-buttons `((cancel ,(M_ "Canc_el") #t)))

(define (make-locale-page parent  title)
  (make-page (page-surface parent)
	     title
	     locale-page-refresh
             0
	     locale-page-key-handler))

(define (locale-page-refresh page)
    (when (not (page-initialised? page))
      (locale-page-init page)
      (page-set-initialised! page #t))

    (let ((win (page-datum page 'text-window))
	  (menu (page-datum page 'menu)))
      (clear win)
      (addstr win
	      (justify* (gettext "The following languages are available.")
			(getmaxx win)))

      (touchwin (outer (page-wwin page)))
      (refresh* (outer (page-wwin page)))
      (refresh* (inner (page-wwin page)))
      (menu-redraw menu)
      (menu-refresh menu)))

(define (locale-page-key-handler page ch)
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

     ((and (eq? ch #\newline)
	   (menu-active menu))
      (let ((locale (menu-get-current-item menu)))
        (setlocale LC_ALL (locale-definition-name locale))
        (page-leave)))

     ((buttons-key-matches-symbol? nav ch 'cancel)
      (page-leave)))

    (std-menu-key-handler menu ch))

  #f
  )

(define (locale-page-init p)
  (let* ((s (page-surface p))
	 (frame (make-boxed-window  #f
	      (- (getmaxy s) 4) (- (getmaxx s) 2)
	      2 1
	      #:title (page-title p)))
	 (button-window (derwin (inner frame)
		       3 (getmaxx (inner frame))
		       (- (getmaxy (inner frame)) 3) 0
			  #:panel #f))
	 (buttons (make-buttons my-buttons 1))

	 (text-window (derwin (inner frame)
			      4
			      (getmaxx (inner frame))
			      0 0 #:panel #f))

	 (menu-window (derwin (inner frame)
		       (- (getmaxy (inner frame)) 3 (getmaxy text-window))
		        (getmaxx (inner frame))
                        (getmaxy text-window) 0 #:panel #f))

	 (menu (make-menu %default-locale-definitions
                          #:disp-proc (lambda (d row)
                                        (locale-definition-name d)))))

    (push-cursor (page-cursor-visibility p))
    (page-set-datum! p 'text-window text-window)
    (page-set-wwin! p frame)
    (page-set-datum! p 'menu menu)
    (page-set-datum! p 'navigation buttons)
    (menu-post menu menu-window)
    (buttons-post buttons button-window)
    (refresh* (outer frame))
    (refresh* button-window)))
