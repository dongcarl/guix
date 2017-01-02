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

(define-module (gnu system installer filesystems)
  #:use-module (gnu system installer partition-reader)
  #:use-module (gnu system installer mount-point)
  #:use-module (gnu system installer dialog)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (guix build utils)
  #:use-module (gurses buttons)
  #:use-module (gurses menu)
  #:use-module (ncurses curses)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)

  #:export (minimum-store-size)
  #:export (filesystem-task-complete?)
  #:export (make-filesystem-page))

(define minimum-store-size 7000)

(define (filesystem-task-complete?)
  (not (filesystem-task-incomplete-reason)))

(define (filesystem-task-incomplete-reason)
  "Returns #f if the task is complete.  Otherwise a string explaining why not."
  (or
   (and (not (find-mount-device "/" mount-points))
        (N_ "You must specify a mount point for the root (/)."))


   (let ((non-absolute-list (fold (lambda (x prev)
                (if (absolute-file-name? (cdr x))
                    prev
                    (cons (cdr x) prev)))
              '()
              mount-points)))
     (and (not (null? non-absolute-list))
          (ngettext
           (format #f
                   (N_ "The mount point ~s is a relative path.  All mount points must be absolute.")
                   (car non-absolute-list))
           (format #f
                   (N_ "The mount points ~s are relative paths.  All mount points must be absolute.")
                   non-absolute-list)
           (length non-absolute-list))))

   (and (< (size-of-partition (find-mount-device (%store-directory) mount-points))
           minimum-store-size)
        (format #f
                (N_ "The filesystem for ~a requires at least ~aGB.")
                (%store-directory) (/ minimum-store-size 1000)))

   (let loop ((ll mount-points)
              (ac '()))
     (match ll
       ('() #f)
       (((_ . directory) . rest)
        (if (member directory ac)
            (format #f
                    (N_ "You have specified the mount point ~a more than once.")
                    directory)
            (loop rest (cons directory ac))))))

   (let ((partitions-without-filesystems
          (fold (lambda (x prev)
                  (if (not (string-prefix? "ext"
                                           (partition-fs (string->partition
                                                          (car x)))))
                      (cons (car x) prev)
                      prev)) '() mount-points)))

     (if (null? partitions-without-filesystems)
         #f
         (ngettext
          (format #f (N_ "The partition ~a does not contain a filesystem.")
                  (car partitions-without-filesystems))
          (format #f (N_ "The partitions ~a do not contain filesystems.")
                  partitions-without-filesystems)
          (length partitions-without-filesystems))))))

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
    (touchwin (outer (page-wwin page)))
    (refresh (outer (page-wwin page)))
    (refresh (inner (page-wwin page)))
    (menu-refresh menu)
    (menu-redraw menu)))


(define (size-of-partition device)
  "Return the size of the partition whose name is DEVICE"
  (partition-size (string->partition device)))


(define (string->partition device)
  (match (find  (lambda (x)
           (equal? (partition-name (car x))
                   device)) (partition-volume-pairs))
    ((p . _)
     (when (not (partition? p))
       (error (format #f "~s is not a partition" p)))
     p)))


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

     ((buttons-key-matches-symbol? nav ch 'back)
      (delwin (outer (page-wwin page)))
      (delwin (inner (page-wwin page)))
      (set! page-stack (cdr page-stack)))


     ((buttons-key-matches-symbol? nav ch 'continue)
      (let ((errstr (filesystem-task-incomplete-reason)))
        (if errstr
            (let ((next (make-dialog page errstr)))
              (set! page-stack (cons next page-stack))
              ((page-refresh next) next))
            (begin
              (delwin (outer (page-wwin page)))
              (set! page-stack (cdr page-stack))
              ((page-refresh (car page-stack)) (car page-stack)))
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

	 (text-window (derwin (inner pr) 3 (getmaxx (inner pr))
			      0 0))

	 (bwin (derwin (inner pr)
		       3 (getmaxx (inner pr))
		       (- (getmaxy (inner pr)) 3) 0
                       #:panel #f))
	 (buttons (make-buttons my-buttons 1))

	 (mwin (derwin (inner pr)
		       (- (getmaxy (inner pr)) 3 (getmaxy text-window))
		       (- (getmaxx (inner pr)) 0)
		       (getmaxy text-window)  0 #:panel #f))

	 (menu (make-menu  (partition-volume-pairs)
			   #:disp-proc
			   (lambda (d row)
			     (let* ((part (car d))
                                    (name (partition-name part)))

			       (format #f "~30a ~7a ~16a ~a"
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
    (refresh (outer pr))
    (refresh bwin)))


