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

(define-module (gnu system installer filesystems)
  #:use-module (gnu system installer partition-reader)
  #:use-module (gnu system installer mount-point)
  #:use-module (gnu system installer dialog)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (gurses buttons)
  #:use-module (gurses menu)
  #:use-module (ncurses curses)
  #:use-module (srfi srfi-1)
  
  #:export (filesystem-task-complete?)
  #:export (make-filesystem-page))


(define (filesystem-task-complete?)
  (and (find-mount-device "/" mount-points)
       (>= (sizeof-partition (find-mount-device "/gnu" mount-points)) 12000)))

(define (make-filesystem-page parent  title)
  (make-page (page-surface parent)
	     title
	     filesystem-page-refresh
	     filesystem-page-key-handler))


(define my-buttons `((continue ,(N_ "_Continue") #t)
		     (back     ,(N_ "Go _Back") #t)))



(define (filesystem-page-refresh page)
  (when (not (page-initialised? page))
    (filesystem-page-init page)
    (page-set-initialised! page #t))

  (let ((text-win (page-datum page 'text-window))
	(menu (page-datum page 'menu)))
    (clear text-win)
    (addstr text-win
	    (gettext "Select a partition to change its mount point or filesystem."))

    (menu-set-items! menu (partition-volume-pairs))
    (touchwin (cdr (page-wwin page)))
    (refresh (cdr (page-wwin page)))
    (refresh (car (page-wwin page)))
    (menu-refresh menu)
    (menu-redraw menu)))


(define (sizeof-partition device)
  "Return the size of the partition DEVICE"
  (partition-size
   (car (find  (lambda (x)
		 (equal? (partition-name (car x))
			 device)) (partition-volume-pairs)))))


(define (filesystem-page-key-handler page ch)
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

     ((eq? ch #\newline)
      (let* ((dev (list-ref (menu-items menu) (menu-current-item menu)))
	     (name (partition-name (car dev)))
	     (next  (make-page (page-surface page)
			       (format #f
				       (gettext "Choose the mount point for device ~s") name)
			       mount-point-refresh
			       mount-point-page-key-handler)))

	(page-set-datum! next 'device name)
	(set! page-stack (cons next page-stack))
	((page-refresh next) next)
	))

     
     ((buttons-key-matches-symbol? nav ch 'continue)
      (cond
       ((not (find-mount-device "/" mount-points))
	(let ((next
	       (make-dialog
		page
		(gettext 
		 "You must choose a device on which to mount the root (/) of the operating system's filesystem."))))
	  (set! page-stack (cons next page-stack))
	  ((page-refresh next) next)))

       ((< (sizeof-partition (find-mount-device "/gnu" mount-points)) 12000)
	(let ((next
	       (make-dialog
		page
		(format #f
		(gettext 
		 "The filesystem for ~a needs at least ~a of disk space.") "/gnu" "12GB"))))
	  (set! page-stack (cons next page-stack))
	  ((page-refresh next) next)))
	
       (else
	(delwin (cdr (page-wwin page)))
	(set! page-stack (cdr page-stack))
	((page-refresh (car page-stack)) (car page-stack))
	))))
    (std-menu-key-handler menu ch))
  #f
  )

(define (filesystem-page-init p)
  (let* ((s (page-surface p))
	 (pr (make-boxed-window  #f
	      (- (getmaxy s) 4) (- (getmaxx s) 2)
	      2 1
	      #:title (page-title p)))

	 (text-window (derwin (car pr) 3 (getmaxx (car pr))
			      0 0))
	 
	 (bwin (derwin (car pr)
		       3 (getmaxx (car pr))
		       (- (getmaxy (car pr)) 3) 0
			  #:panel #f))
	 (buttons (make-buttons my-buttons 1))

	 (mwin (derwin (car pr)
		       (- (getmaxy (car pr)) 3 (getmaxy text-window))
		       (- (getmaxx (car pr)) 0)
		       (getmaxy text-window)  0 #:panel #f))
	 
	 (menu (make-menu  (partition-volume-pairs)
			   #:disp-proc
			   (lambda (d row)
			     (let* ((part (car d))
				   (name (partition-name part)))

			       (format "~30a ~7a ~16a ~a"
				       name
				       (number->size (partition-size part))
				       (partition-fs part)
				       (let ((x (assoc-ref mount-points name)))
					     (if x x ""))))))))


    (page-set-wwin! p pr)
    (page-set-datum! p 'menu menu)
    (page-set-datum! p 'navigation buttons)
    (page-set-datum! p 'text-window text-window)
    (menu-post menu mwin)
    (buttons-post buttons bwin)
    (refresh (cdr pr))
    (refresh bwin)))
			      

