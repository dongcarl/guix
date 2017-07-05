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

(define-module (gnu system installer user-edit)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (gnu system shadow)
  #:use-module (gurses form)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:export (make-user-edit-page)
  )

(include "i18n.scm")

(define (my-fields) `((comment     ,(M_ "Real Name") 40)
                      (name        ,(M_ "User Name") 40)
                      (home        ,(M_ "Home Directory") 16)))

(define (make-user-edit-page parent title account)
  (let ((page (make-page (page-surface parent)
                       title
                       user-edit-refresh
                       1
                       #:activator user-edit-page-activate-focused-item)))
    (page-set-datum! page 'account account)
    (page-set-datum! page 'parent parent)
    page))


(define (user-edit-refresh page)
  (when (not (page-initialised? page))
    (user-edit-page-init page)
    (page-set-initialised! page #t))
  (let ((form  (page-datum page 'form)))
    (refresh* (outer (page-wwin page)))
    (refresh* (form-window form))))

(define (user-edit-page-activate-focused-item page)
  (let ((form  (page-datum page 'form))
	(nav   (page-datum page 'navigation))
        (parent   (page-datum page 'parent))
	(dev   (page-datum page 'device)))
    (match (if (form-enabled? form)
               'save
               (buttons-selected-symbol nav))
     ('save
      (set! users
            (cons
             (user-account
              (name    (form-get-value form 'name))
              (supplementary-groups '("video" "audio" "floppy" "dialout"))
              (group   "users")
              (comment (form-get-value form 'comment))
              (home-directory (form-get-value form 'home)))
             (remove (lambda (user)
                       (equal? user (page-datum page 'account)))
                     users)))
      (page-set-initialised! parent #f)
      (page-leave)
      'handled)

     ('cancel
      (page-leave)
      'handled)
     (_ 'ignored))))

(define my-buttons `((save ,(M_ "Save") #f)
		     (cancel     ,(M_ "Cancel") #f)))

(define (user-edit-page-init p)
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
                     (-
                      (getmaxy (inner pr))
                      (getmaxy text-window)
                      (getmaxy bwin))
		     (getmaxx (inner pr))
		     (getmaxy text-window) 0 #:panel #f))

	 (form (make-form (my-fields)
                          (lambda (frm)
                            ;; Infer the most likely desired values of the
                            ;; name and home fields from the other field values
                            (let* ((f (get-current-field frm))
                                   (fv (form-get-value frm (field-symbol f)))
                                   (brk (string-index fv #\space))
                                   (first
                                    (and brk
                                         (string-map char-downcase
                                                     (string-take fv brk)))))
                              (cond ((eq? (field-symbol f) 'comment)
                                     (when first
                                           (form-set-value! frm 'name first)
                                           (form-set-value! frm 'home
                                                            (string-append "/home/"
                                                                           first))))
                                    ((eq? (field-symbol f) 'name)
                                     (form-set-value! frm 'home
                                                      (string-append
                                                       "/home/"
                                                       (form-get-value
                                                        frm 'name))))))))))
    (page-set-datum! p 'navigation nav)

    (let ((acc (page-datum p 'account)))
      (addstr*
       text-window
       (if acc
       (format #f (M_ "This user account currently has the following details.  You may change any details here as required."))
       (format #f (M_ "Enter the details of the new user below."))))

      (form-post form fw)

      (when acc
            (form-set-value! form 'name (user-account-name acc))
            (form-set-value! form 'comment (user-account-comment acc))
            (form-set-value! form 'home (user-account-home-directory acc))))

    (push-cursor (page-cursor-visibility p))
    (buttons-post nav bwin)
    (page-set-datum! p 'form form)

    (page-set-wwin! p pr)
    (refresh* (outer pr))))
