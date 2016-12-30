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

(define-module (gnu system installer mount-point)
  #:use-module (gnu system installer partition-reader)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (gurses form)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)

  #:export (mount-point-refresh)
  #:export (mount-point-page-key-handler))

(define (efs-params device)
  (slurp
   (string-append "tune2fs -l " device)
   (lambda (line)
     (let ((sep (string-contains line ":")))
       (if (not sep)
	   ""
	   (cons
		 (string->symbol
		  (string-map (lambda (c) (if (eq? c #\space) #\- c))
			      (string-downcase (substring line 0 sep))))
		 (string-trim-both (substring line (+ sep 2)))))))))

(define my-fields `((label       ,(N_ "Label") 40)
		    (mount-point ,(N_ "Mount Point") 10)))

(define (mount-point-refresh page)
  (when (not (page-initialised? page))
    (mount-point-page-init page)
    (page-set-initialised! page #t))
  (let ((form  (page-datum page 'form)))
    (refresh (outer (page-wwin page)))
    (refresh (form-window form))))

(define (mount-point-page-key-handler page ch)
  (let ((form  (page-datum page 'form))
	(nav   (page-datum page 'navigation))
	(dev   (page-datum page 'device)))

    (if (not (form-enabled? form))
	(if (or
	     (eq? ch #\space)
	     (eq? ch #\nl))
	    (cond
	     ((buttons-key-matches-symbol? nav ch 'continue)
              (let ((mp (form-get-value form 'mount-point)))
                (if (equal? "" mp)
                    (set! mount-points
                      (assoc-remove! mount-points dev))
                    
                    (set! mount-points (assoc-set! mount-points
                                                   dev mp))))

	      (set! page-stack (cdr page-stack))
	      ((page-refresh (car page-stack)) (car page-stack)))

	     ((buttons-key-matches-symbol? nav ch 'check)
	      (window-pipe (page-datum page 'output) "fsck.ext4" "fsck.ext4" "-n" "-v"
			   "-f"
			   dev))

	     ((buttons-key-matches-symbol? nav ch 'write)
	      (window-pipe (page-datum page 'output)
			   "tune2fs" "tune2fs"
			   "-L" (form-get-value form 'label)
			   dev))

	     ((buttons-key-matches-symbol? nav ch 'recreate)
	      (window-pipe (page-datum page 'output)
			   "mkfs.ext4" "mkfs.ext4" "-v" "-F"
			   "-L" (form-get-value form 'label)
			   dev))
	     )))
    
    (cond
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

    (curs-set 1)
    (form-enter form ch))
  #f)

(define my-buttons `((continue ,(N_ "Continue") #f)
		     (check    ,(N_ "Check") #f)
		     (write    ,(N_ "Write") #f)
		     (recreate ,(N_ "(re)Create") #f)
		     (back     ,(N_ "Go Back") #f)))

(define (mount-point-page-init p)
  (let* ((s (page-surface p))
	 (pr (make-boxed-window
	      #f
	      (- (getmaxy s) 4) (- (getmaxx s) 2)
	      2 1
	      #:title (page-title p)))
	 
	 (text-window (derwin (inner pr) 3 (getmaxx (inner pr))
			      0 0))
	 
	 (bwin (derwin (inner pr)
		       3 (getmaxx (inner pr))
		       (- (getmaxy (inner pr)) 3) 0
		       #:panel #f))
	 
	 (nav (make-buttons my-buttons 1))
	 
	 (fw (derwin (inner pr)
		     2
		     (getmaxx (inner pr))
		     (getmaxy text-window) 0))


	 (out (derwin (inner pr)
		     (- (getmaxy (inner pr)) (getmaxy bwin) (getmaxy text-window) (getmaxy fw))
		     (getmaxx (inner pr))
		     (+ (getmaxy text-window) (getmaxy fw))
		     0))
	 
	 (form (make-form my-fields)))

    (box out 0 0)
    (page-set-datum! p 'output out)
    (page-set-datum! p 'navigation nav)    
    (let* ((dev (page-datum p 'device))
	   (efsp (efs-params dev)))
      (addstr*
       text-window
       (format #f
	       (gettext
		"The device ~s is currently configured as follows.  You may change the configuration here if desired.")
	       dev))

      (form-post form fw)
      (if efsp
	  (form-set-value! form 'label
			   (assq-ref efsp
				     'filesystem-volume-name)))

      (form-set-value! form 'mount-point
		       (or (assoc-ref mount-points dev)
			   "")))

    (buttons-post nav bwin)
    (page-set-datum! p 'form form)

    (page-set-wwin! p pr)
    (refresh (outer pr))))

