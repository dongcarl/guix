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
  #:use-module (srfi srfi-9)

  #:export (make-file-system-spec)
  #:export (<file-system-spec>)
  #:export (file-system-spec?)
  #:export (file-system-spec-mount-point)
  #:export (file-system-spec-label)
  #:export (file-system-spec-type)
  #:export (file-system-spec-uuid)

  #:export (minimum-store-size)
  #:export (filesystem-task-complete?)
  #:export (make-filesystem-page))


(define-syntax M_
  (syntax-rules ()
    ((M_ str)
     str)))

;; File system spec declaration.
(define-record-type <file-system-spec>
  (make-file-system-spec' mount-point label type uuid)
  file-system-spec?
  (mount-point      file-system-spec-mount-point)
  (label            file-system-spec-label)
  (type             file-system-spec-type)
  (uuid             file-system-spec-uuid))

(define (make-file-system-spec mount-point label type)
  (let ((uuid (slurp "uuidgen" identity)))
    (make-file-system-spec' mount-point label type (car uuid))))



(define minimum-store-size 7000)

(define (filesystem-task-complete?)
  (not (filesystem-task-incomplete-reason)))

(define (filesystem-task-incomplete-reason)
  "Returns #f if the task is complete.  Otherwise a string explaining why not."
  (or
   (and (not (find-mount-device "/" mount-points))
        (M_ "You must specify a mount point for the root (/)."))


   (let ((non-absolute-list
          (fold (lambda (x prev)
                  (match x
                         ((dev . fss)
                          (if (absolute-file-name? (file-system-spec-mount-point fss))
                              prev
                              (cons (file-system-spec-mount-point fss) prev)))))
                '()
                mount-points)))
     (and (not (null? non-absolute-list))
          (ngettext
           (format #f
                   (M_ "The mount point ~s is a relative path.  All mount points must be absolute.")
                   (car non-absolute-list))
           (format #f
                   (M_ "The mount points ~s are relative paths.  All mount points must be absolute.")
                   non-absolute-list)
           (length non-absolute-list))))

   (and (< (size-of-partition (find-mount-device (%store-directory) mount-points))
           minimum-store-size)
        (format #f
                (M_ "The filesystem for ~a requires at least ~aGB.")
                (%store-directory) (/ minimum-store-size 1000)))

   (let loop ((ll mount-points)
              (ac '()))
     (match ll
       ('() #f)
       (((_ . (? file-system-spec? fss)) . rest)
        (if (member fss ac)
            (format #f
                    (M_ "You have specified the mount point ~a more than once.")
                    (file-system-spec-mount-point fss))
            (loop rest (cons fss ac))))))

   (let ((partitions-without-filesystems
          (fold (lambda (x prev)
                  (match x
                         ((dev . (? file-system-spec? fss))
                          (if (not (string-prefix? "ext"
                                                   (file-system-spec-type fss)))
                              (cons dev prev)
                              prev)))) '() mount-points)))

     (if (null? partitions-without-filesystems)
         #f
         (ngettext
          (format #f (M_ "The filesystem type for partition ~a is not valid.")
                  (car partitions-without-filesystems))
          (format #f (M_ "The filesystem type for partitions ~a are not valid.")
                  partitions-without-filesystems)
          (length partitions-without-filesystems))))))

(define (make-filesystem-page parent  title)
  (make-page (page-surface parent)
	     title
	     filesystem-page-refresh
             0
	     filesystem-page-key-handler))


(define my-buttons `((continue ,(M_ "_Continue") #t)
		     (cancel     ,(M_ "Canc_el") #t)))



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
    (menu-redraw menu)
    (menu-refresh menu)))


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
                               1
			       mount-point-page-key-handler)))

	(page-set-datum! next 'device name)
        (page-enter next)))

     ((buttons-key-matches-symbol? nav ch 'cancel)
      (page-leave))


     ((buttons-key-matches-symbol? nav ch 'continue)
      (let ((errstr (filesystem-task-incomplete-reason)))
        (if errstr
            (let ((next (make-dialog page errstr)))
              (page-enter next))
            (begin
              (page-leave))
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

	 (menu (make-menu
                (partition-volume-pairs)
                #:disp-proc
                (lambda (d row)
                  (let* ((part (car d))
                         (name (partition-name part))
                         (fs-spec
                          (assoc-ref mount-points name)))

                    (format #f "~30a ~7a ~16a ~a"
                            name
                            (number->size (partition-size part))
                            (if fs-spec (file-system-spec-type fs-spec) "")
                            (if fs-spec
                                (file-system-spec-mount-point fs-spec) "")))))))

    (push-cursor (page-cursor-visibility p))
    (page-set-wwin! p pr)
    (page-set-datum! p 'menu menu)
    (page-set-datum! p 'navigation buttons)
    (page-set-datum! p 'text-window text-window)
    (menu-post menu mwin)
    (buttons-post buttons bwin)
    (refresh (outer pr))
    (refresh bwin)))


