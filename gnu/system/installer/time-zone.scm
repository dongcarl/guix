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

(define-module (gnu system installer time-zone)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer utils)
  #:use-module (gnu system installer misc)
  #:use-module (gurses menu)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)

  #:export (make-tz-browser))

(define* (make-tz-browser parent directory #:optional (exit-point #f))
  (let ((page (make-page (page-surface parent)
			(gettext "Time Zone")
			time-zone-page-refresh
			time-zone-page-key-handler)))
    (page-set-datum! page 'directory directory)
    (if exit-point
	(page-set-datum! page 'exit-point exit-point)
	(page-set-datum! page 'exit-point (page-datum parent 'exit-point)))
    page))


(define my-buttons `((back  ,(N_ "_Back") #t)))

(define (time-zone-page-key-handler page ch)
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

     ((buttons-key-matches-symbol? nav ch 'back)
      (page-leave))

     ((and (eqv? ch #\newline)
	   (menu-active menu))
      (let* ((i (menu-get-current-item menu))
	     (new-dir (string-append directory "/" i))
	     (st (lstat new-dir)))
	(if (and (file-exists? new-dir)
		 (eq? 'directory (stat:type st)))
	    (let ((p (make-tz-browser
		      page new-dir)))
	      (page-set-datum! p 'stem
			       (if (page-datum page 'stem)
				   (string-append (page-datum page 'stem) "/" i)
				   i))
              (page-enter p))
	    (begin
	      (set! time-zone
		(if (page-datum page 'stem)
		    (string-append (page-datum page 'stem) "/" i)
		    i))
	      (page-leave (page-datum page 'exit-point))
	      #f)))
      ))
  (std-menu-key-handler menu ch)
  #f))


(define (time-zone-page-refresh page)
  (when (not (page-initialised? page))
    (time-zone-page-init page)
    (page-set-initialised! page #t))
  (touchwin (outer (page-wwin page)))
  (refresh (outer (page-wwin page)))
  (refresh (inner (page-wwin page)))
  (menu-refresh (page-datum page 'menu)))

(define (time-zone-page-init p)
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
	 
	 (menu (make-menu
		(let nn ((ds (opendir (page-datum p 'directory)))
			 (ll '()))
		  (let ((o (readdir ds)))
		    (if (eof-object? o)
			(begin
			  (closedir ds)
			  (sort ll string< ))
			(nn ds
			    (cond 
			     ((equal? "." o) ll)
			     ((equal? ".." o) ll)
			     ((>= (string-suffix-length o ".tab") 4) ll)
			     (else 
			      (cons o ll)))))))))
	 )
    
    (menu-post menu menu-window)
    
    (addstr* text-window
	     (gettext "Select the default time zone for the system:" ))
    (page-set-wwin! p frame)
    (page-set-datum! p 'menu menu)
    (page-set-datum! p 'navigation  buttons)
    (buttons-post buttons button-window)
    (refresh (outer frame))
    (refresh (inner frame))
    (refresh text-window)
    (refresh button-window)))
