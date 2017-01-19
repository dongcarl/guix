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

(define-module (gnu system installer mount-point)
  #:use-module (gnu system installer partition-reader)
  #:use-module (gnu system installer filesystems)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (gurses form)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)

  #:export (mount-point-refresh)
  #:export (mount-point-page-key-handler))


(define-syntax M_
  (syntax-rules ()
    ((M_ str)
     str)))

(define my-fields `((mount-point ,(M_ "Mount Point") 40)
                    (fs-type     ,(M_ "File System Type")
                                  ("ext2" "ext3" "ext4" "btrfs" "swap"))
                    (label       ,(M_ "Label") 16)))

(define (mount-point-refresh page)
  (when (not (page-initialised? page))
    (mount-point-page-init page)
    (page-set-initialised! page #t))
  (let ((form  (page-datum page 'form)))
    (refresh* (outer (page-wwin page)))
    (refresh* (form-window form))))

(define (mount-point-page-key-handler page ch)
  (let ((form  (page-datum page 'form))
	(nav   (page-datum page 'navigation))
	(dev   (page-datum page 'device)))

    (cond
     ((buttons-key-matches-symbol? nav ch 'continue)
      (let ((fss
             (make-file-system-spec
              (form-get-value form 'mount-point)
              (form-get-value form 'label)
              (form-get-value form 'fs-type))))
        (when fss
              (set! mount-points
                    (assoc-set! mount-points dev fss))))
      (page-leave))

     ((buttons-key-matches-symbol? nav ch 'cancel)
      ;; Close the menu and return
      (page-leave))

     ((or (eq? ch KEY_RIGHT)
	  (eq? ch #\tab))
      (form-set-enabled! form #f)
      (buttons-select-next nav))

     ((eq? ch KEY_LEFT)
      (form-set-enabled! form #f)
      (buttons-select-prev nav))

     ((eq? ch KEY_UP)
      (buttons-unselect-all nav)
      (form-set-enabled! form #t))

     ((eq? ch KEY_DOWN)
      (buttons-unselect-all nav)
      (form-set-enabled! form #t))
     )

    (form-enter form ch))
  #f)

(define my-buttons `((continue ,(M_ "Continue") #f)
		     (cancel     ,(M_ "Cancel") #f)))

(define (mount-point-page-init p)
  (let* ((s (page-surface p))
	 (pr (make-boxed-window
	      #f
	      (- (getmaxy s) 4) (- (getmaxx s) 2)
	      2 1
	      #:title (page-title p)))
	
	 (text-window (derwin (inner pr) 3 (getmaxx (inner pr))
			      0 0 #:panel #t))
	
	 (bwin (derwin (inner pr)
		       3 (getmaxx (inner pr))
		       (- (getmaxy (inner pr)) 3) 0
		       #:panel #t))
	
	 (nav (make-buttons my-buttons 1))
	
	 (fw (derwin (inner pr)
		     (length my-fields)
		     (getmaxx (inner pr))
		     (getmaxy text-window) 0 #:panel #f))


	 (form (make-form
                my-fields
                (lambda (f)
                  (let ((field (get-current-field f)))
                    (if (eq? (field-symbol field) 'mount-point)
                        (form-set-value! f 'label
                                         (string-append
                                          host-name "-"
                                          (form-get-value f 'mount-point)))))))))

    (page-set-datum! p 'navigation nav)
    (let ((dev (page-datum p 'device)))
      (addstr*
       text-window
       (format #f
	       (gettext
		"The device ~s is currently configured as follows.  You may change the configuration here if desired.")
	       dev))

      (form-post form fw))

    (let* ((dev (page-datum p 'device))
           (fss (assoc-ref mount-points dev)))

      (form-set-value! form 'label
                       (if fss
                           (file-system-spec-label fss)
                           (string-append host-name
                           "-")))
      (when fss
            (form-set-value! form 'mount-point
                             (file-system-spec-mount-point fss))
            (form-set-value! form 'fs-type
                             (symbol->string
                             (file-system-spec-type fss)))))

    (form-set-current-field form 0)

    (push-cursor (page-cursor-visibility p))
    (buttons-post nav bwin)
    (page-set-datum! p 'form form)

    (page-set-wwin! p pr)
    (refresh* (outer pr))))

