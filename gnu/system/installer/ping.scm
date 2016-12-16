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

(define-module (gnu system installer ping)
  #:use-module (gnu system installer partition-reader)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)

  #:export (substitution-servers)
  #:export (ping-page-refresh)
  #:export (ping-page-key-handler))


(define substitution-servers '("mirror.hydra.gnu.org"))

(define my-buttons `((test ,(N_ "_Test") #t)
		     (continue  ,(N_ "_Continue") #t)
		     (back     ,(N_ "Go _Back") #t)))

(define (ping-page-key-handler page ch)
  (let ((nav  (page-datum page 'navigation))
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
      (delwin (cdr (page-wwin page)))
      (delwin (car (page-wwin page)))

      (delwin (page-datum page 'test-window))
      (set! page-stack (cdr page-stack))
      )
     
     ((buttons-key-matches-symbol? nav ch 'test)

      (let* ((windowp (make-window-port test-window)))
	(if (zero?
	     (window-pipe test-window  "ping" "ping" "-c" "3" (car substitution-servers)))
	    (addstr test-window
		    (gettext "Test successful.  Network is working."))
	    (addstr test-window
		    (gettext "Test failed. No servers reached.")))
     
	(refresh test-window)))) #f))

(define (ping-page-refresh page)
  (when (not (page-initialised? page))
    (ping-page-init page)
    (page-set-initialised! page #t))
  (refresh (page-datum page 'test-window)))

(define (ping-page-init p)
  (let* ((s (page-surface p))
	 (frame (make-boxed-window  #f
	      (- (getmaxy s) 5) (- (getmaxx s) 2)
	      2 1
	      #:title (page-title p)))
	 (button-window (derwin (car frame)
		       3 (getmaxx (car frame))
		       (- (getmaxy (car frame)) 3) 0
			  #:panel #f))
	 (buttons (make-buttons my-buttons 1))

	 (text-window (derwin (car frame)
			      4
			      (getmaxx (car frame))
			      0 0 #:panel #f))

	 (test-window (derwin (car frame)
			      (- (getmaxy (car frame)) (getmaxy text-window) (getmaxy button-window))
			      (getmaxx (car frame))
			      (getmaxy text-window) 0 #:panel #f))
	 )

    (box test-window 0 0)
    (addstr* text-window
	    (gettext "Choose \"Test\" to check network connectivity."))
    (page-set-wwin! p frame)
    (page-set-datum! p 'test-window test-window)
    (page-set-datum! p 'navigation buttons)
    (buttons-post buttons button-window)
    (refresh text-window)
    (refresh button-window)))

			      

