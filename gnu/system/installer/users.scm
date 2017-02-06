;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 John Darrington <jmd@gnu.org>
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

(define-module (gnu system installer users)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (gnu system shadow)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (gurses menu)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)

  #:export (make-users-page))

(include "i18n.scm")

(define (make-users-page parent  title)
  (make-page (page-surface parent)
	     title
	     users-page-refresh
             0
	     users-page-key-handler))


(define my-buttons `((delete ,(M_ "_Delete") #t)
                     (add ,(M_ "_Add") #t)
                     (cancel ,(M_ "Canc_el") #t)))

(define (users-page-key-handler page ch)
  (let ((menu (page-datum page 'menu))
	(nav  (page-datum page 'navigation)))

    (cond
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

     ((eq? ch KEY_RIGHT)
      (menu-set-active! menu #f)
      (buttons-select-next nav))

     ((eq? ch KEY_LEFT)
      (menu-set-active! menu #f)
      (buttons-select-prev nav))

     ((eq? ch KEY_UP)
      (buttons-unselect-all nav)
      (menu-set-active! menu #t))


     ((select-key? ch)
      (page-leave))

     ((buttons-key-matches-symbol? nav ch 'cancel)
      (page-leave))

     ((buttons-key-matches-symbol? nav ch 'delete)
      (set! users (remove (lambda (user)
                             (equal? user (menu-get-current-item menu)))
                           users))
      (page-set-initialised! page #f)))

    (std-menu-key-handler menu ch))
  #f)


(define (users-page-refresh page)
  (when (not (page-initialised? page))
    (users-page-init page)
    (page-set-initialised! page #t))
  (touchwin (outer (page-wwin page)))
  (refresh* (outer (page-wwin page)))
  (refresh* (inner (page-wwin page)))
  (menu-refresh (page-datum page 'menu)))



(define (users-page-init p)
  (let* ((s (page-surface p))
	 (pr (make-boxed-window  #f
	      (- (getmaxy s) 4) (- (getmaxx s) 2)
	      2 1
	      #:title (page-title p)))
	 (text-window (derwin
		       (inner pr)
		       5 (getmaxx (inner pr))
		       0 0
		       #:panel #f))

	 (bwin (derwin (inner pr)
		       3 (getmaxx (inner pr))
		       (- (getmaxy (inner pr)) 3) 0
			  #:panel #f))
	 (buttons (make-buttons my-buttons 1))

	 (mwin (derwin (inner pr)
		       (- (getmaxy (inner pr)) (getmaxy text-window) 3)
		       (- (getmaxx (inner pr)) 0)
		       (getmaxy text-window) 0 #:panel #f))

	 (menu (make-menu users
                          #:disp-proc (lambda (x r)
                                        (format #f "~16a ~40a"
                                                (user-account-name x)
                                                (user-account-comment x))))))

    (addstr*
     text-window
     (if (null? users)
         (format #f
                 (M_ "Currently there are no users in the system configuration.  You can add some users now, or you can ignore this step and add them after the system has been installed.  The root user will be automatically created regardless."))
         (format #f (M_
                     "The following user accounts are currently configured.  You can edit the account details here and add or remove them as desired."))))

    (push-cursor (page-cursor-visibility p))

    (page-set-wwin! p pr)
    (page-set-datum! p 'menu menu)
    (page-set-datum! p 'navigation buttons)
    (menu-post menu mwin)
    (buttons-post buttons bwin)
    (refresh* (outer pr))
    (refresh* text-window)
    (refresh* bwin)))
