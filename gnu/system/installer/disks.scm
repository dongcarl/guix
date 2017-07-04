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

(define-module (gnu system installer disks)
  #:use-module (gnu system installer partition-reader)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (gurses menu)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (ice-9 format)
  #:export (make-disk-page))

(include "i18n.scm")

(define my-buttons `((continue ,(M_ "_Continue") #t)))

(define (make-disk-page parent  title)
  (make-page (page-surface parent)
             title
             disk-page-refresh
             0
             #:activator disk-page-activate-focused-item))

(define (disk-page-refresh page)
    (when (not (page-initialised? page))
      (disk-page-init page)
      (page-set-initialised! page #t))

    (let ((win (page-datum page 'text-window))
	  (menu (page-datum page 'menu)))
      (clear win)
      (addstr win
	      (justify* (gettext "Select a disk to partition (or repartition), or choose \"Continue\" to leave the disk(s) unchanged.")
			(getmaxx win)))

      (menu-set-items! menu (volumes))
      (touchwin (outer (page-wwin page)))
      (refresh* (outer (page-wwin page)))
      (refresh* (inner (page-wwin page)))
      (menu-redraw menu)
      (menu-refresh menu)))

(define (disk-page-activate-focused-item page)
  (let ((menu (page-datum page 'menu)))
    (cond
     ((menu-active menu)
      (let* ((menu (page-datum page 'menu))
             (i (menu-current-item menu)))
        (endwin)
        (system* "cfdisk" (disk-name (list-ref (menu-items menu) i)))
        (system* "partprobe")))
     (else ; "Continue" button activated
      (page-leave)))))

(define (truncate-string ss w)
 (if (> (string-length ss) w)
	  (string-append
	   (string-take ss (- w 3)) "...")
	  ss))

(define (disk-page-init p)
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
	 (menu (make-menu  (volumes)
			   #:disp-proc
			   (lambda (d row)
			     (let ((w 23))
			       (format #f (ngettext "~28a ~? ~6a  (~a partition)"
						    "~28a ~? ~6a  (~a partitions)"
						    (length (disk-partitions d)))
				       (disk-name d)
				       (format #f "~~~aa" (1+ w))
				       (list (truncate-string (disk-vendor d) w))
				       (number->size (disk-size d))
				       (length (disk-partitions d))))))))

    (push-cursor (page-cursor-visibility p))
    (page-set-datum! p 'text-window text-window)
    (page-set-wwin! p frame)
    (page-set-datum! p 'menu menu)
    (page-set-datum! p 'navigation buttons)
    (menu-post menu menu-window)
    (buttons-post buttons button-window)
    (refresh* (outer frame))
    (refresh* button-window)))
