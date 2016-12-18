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

(define-module (gnu system installer hostname)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (gurses form)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)

  #:export (make-host-name-page))

(define my-fields `((name   ,(N_ "Host Name") 64)))

(define (make-host-name-page parent  title)
  (make-page (page-surface parent)
	     title
	     host-name-refresh
	     host-name-key-handler))

(define (host-name-refresh page)
  (when (not (page-initialised? page))
    (host-name-init page)
    (page-set-initialised! page #t))

  (let ((form  (page-datum page 'form))
	(text-window (page-datum page 'text-window)))
    (clear text-window)
    (addstr*
     text-window
     (gettext "Enter the host name for the new system.  Only letters, digits and hyphens are allowed. The first character may not be a hyphen.  A maximum of 64 characters are allowed."))
    (refresh text-window)
    (refresh (cdr (page-wwin page)))
    (refresh (form-window form))))

(define (host-name-key-handler page ch)
  (let ((form  (page-datum page 'form))
	(nav   (page-datum page 'navigation))
	(dev   (page-datum page 'device)))

    (cond
     ((buttons-key-matches-symbol? nav ch 'continue)
      (set! host-name (form-get-value form 0))
      (set! page-stack (cdr page-stack))
      ((page-refresh (car page-stack)) (car page-stack)))

     ((eq? ch #\tab)
      (form-set-enabled! form #f)
      (buttons-select-next nav))

     ((eq? ch KEY_UP)
      (buttons-unselect-all nav)
      (form-set-enabled! form #t))

     ((eq? ch KEY_DOWN)
      (buttons-unselect-all nav)
      (form-set-enabled! form #t))

     ;; Do not allow more than 64 characters
     ((and (char? ch)
           (char-set-contains? char-set:printing ch)
           (>= (field-cursor-position (get-current-field form)) 64)))

     ;; The first character may not be  a hyphen
     ((and (char? ch)
           (eq? ch #\-)
           (zero? (field-cursor-position (get-current-field form)))))

     ;; Subsequent characters must be [-A-Za-z0-9]
     ((and (char? ch)
           (char-set-contains? char-set:printing ch)
           (not (char-set-contains?
                 (char-set-adjoin char-set:letter+digit #\-) ch))
           (positive? (field-cursor-position (get-current-field form)))))

     (else
      (curs-set 1)
      (form-enter form ch)))
    #f))

(define my-buttons `((continue ,(N_ "Continue") #f)))

(define (host-name-init p)
  (let* ((s (page-surface p))
	 (pr (make-boxed-window
	      #f
	      (- (getmaxy s) 4) (- (getmaxx s) 2)
	      2 1
	      #:title (page-title p)))
	 
	 (text-window (derwin (car pr) 5 (getmaxx (car pr))
			      0 0))
	 
	 (bwin (derwin (car pr)
		       3 (getmaxx (car pr))
		       (- (getmaxy (car pr)) 3) 0
		       #:panel #f))
	 
	 (nav (make-buttons my-buttons 1))
	 
	 (fw (derwin (car pr)
		     2
		     (getmaxx (car pr))
		     (getmaxy text-window) 0))


	 (form (make-form my-fields)))

    (page-set-datum! p 'navigation nav)    
    (page-set-datum! p 'text-window text-window)
    (page-set-datum! p 'form form)
    
    (form-post form fw)
    (buttons-post nav bwin)
    (page-set-wwin! p pr)
    (refresh (cdr pr))))

