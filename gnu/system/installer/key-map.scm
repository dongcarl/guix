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

  #:export (make-key-map))

(include "i18n.scm")

(define* (make-key-map parent directory)
  (let ((page (make-page (page-surface parent)
			(gettext "Keyboard Mapping")
			key-map-page-refresh
                        0
			key-map-page-key-handler
			key-map-page-mouse-handler)))
    (page-set-datum! page 'directory directory)
    page))


(define my-buttons `((cancel  ,(M_ "Canc_el") #t)))

(define (key-map-page-activate-focused-item page)
  (let* ((menu (page-datum page 'menu))
         (i (menu-get-current-item menu))
         (directory (page-datum page 'directory))
         (new-dir (string-append directory "/" i)))
    (if (eq? 'directory (stat:type (stat new-dir)))
      (let ((p (make-key-map page new-dir)))
        (page-pop) ; Don't go back to the current page!
        (page-enter p))
      (begin
        (system* "loadkeys" i)
        (set! key-map i)
        (page-leave)
        #f))))

(define (key-map-page-mouse-handler page device-id x y z button-state)
  (let* ((menu (page-datum page 'menu))
         (status (std-menu-mouse-handler menu device-id x y z button-state)))
    (if (eq? status 'activated)
      (key-map-page-activate-focused-item page))
    status))

(define (key-map-page-key-handler page ch)
  (let ((nav  (page-datum page 'navigation))
	(menu (page-datum page 'menu))
	(directory (page-datum page 'directory)))
    (cond
     ((eq? ch #\tab)
      (cond
       ((eqv? (buttons-selected nav) (1- (buttons-n-buttons nav)))
	(buttons-unselect-all nav))

       (else
	(buttons-select-next nav))))

     ((buttons-key-matches-symbol? nav ch 'cancel)

      (page-leave))

     ((and (select-key? ch)
           (menu-active menu))
      (key-map-page-activate-focused-item page)))
    (std-menu-key-handler menu ch)
    #f))


(define (key-map-page-refresh page)
  (when (not (page-initialised? page))
    (key-map-page-init page)
    (page-set-initialised! page #t))
  (touchwin (outer (page-wwin page)))
  (refresh* (outer (page-wwin page)))
  (refresh* (inner (page-wwin page)))
  (menu-refresh (page-datum page 'menu)))

(define (key-map-page-init p)
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
			      4
			      (getmaxx (inner frame))
			      0 0 #:panel #f))

	 (menu-window (derwin (inner frame)
			      (- (getmaxy (inner frame)) 3 (getmaxy text-window))
			      (getmaxx (inner frame))
			      (getmaxy text-window) 0 #:panel #f))

	 (menu (make-menu
		(let ((dir (page-datum p 'directory)))
		      (slurp (string-append "ls -1 "
						dir)
			      identity)))))

    (menu-post menu menu-window)

    (addstr* text-window
	     (gettext "Select an item most closely matching your keyboard layout:" ))
    (push-cursor (page-cursor-visibility p))
    (page-set-wwin! p frame)
    (page-set-datum! p 'menu menu)
    (page-set-datum! p 'navigation buttons)
    (buttons-post buttons button-window)
    (refresh* (outer frame))
    (refresh* (inner frame))
    (refresh* text-window)
    (refresh* button-window)))



