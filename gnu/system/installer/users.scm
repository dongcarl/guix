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
  #:use-module (gnu system installer user-edit)
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

(define (make-users-page parent title)
  (make-page (page-surface parent)
             title
             users-page-refresh
             0
             #:activator users-page-activate-item))

(define my-buttons `((add ,(M_ "_Add") #t)
                     (delete ,(M_ "_Delete") #t)
                     (continue ,(M_ "_Continue") #t)))

(define (users-page-activate-item page item)
  (let ((menu (page-datum page 'menu)))
    (match item
     (('menu-item-activated account)
      (if account
        (page-enter  (make-user-edit-page page  "Edit User" account)))
      'handled)

     ('add
      (let* ((next  (make-user-edit-page page  "Add New User" #f)))
        (page-enter next)
        'handled))
     ('continue
      (page-leave)
      'handled)
     ('delete
      (set! users (remove (lambda (user)
                            (equal? user (menu-get-current-item menu)))
                          users))
      (page-set-initialised! page #f)
      'handled)
     (_
      'ignored))))


(define header-format "~16a ~40a")

(define (users-page-refresh page)
  (when (not (page-initialised? page))
    (users-page-init page)
    (page-set-initialised! page #t))
  (let ((text-window (page-datum page 'text-window))
        (header-window (page-datum page 'header-window))
        (header (format #f header-format (gettext "Username")
                                           (gettext "Real name"))))
    (erase text-window)
    (addstr*
     text-window
     (if (null? users)
         (format #f
                 (M_ "Currently there are no users in the system configuration.  You can add some users now, or you can ignore this step and add them after the system has been installed.  The root user will be automatically created regardless."))
         (format #f (M_
                     "The following user accounts are currently configured.  You can edit the account details here and add or remove them as desired."))))

    (erase header-window)
    (addstr header-window header)
    (addstr header-window "
")
    (hline header-window (acs-hline) (string-length header))))

(define (users-page-init p)
  (match (create-vbox (page-surface p) 3 2 (- (getmaxy (page-surface p)) 3 2 3) 3)
   ((text-window header-window mwin bwin)
    (let* ((buttons (make-buttons my-buttons 1))
           (menu (make-menu users
                          #:disp-proc (lambda (x r)
                                        (format #f header-format
                                                (user-account-name x)
                                                (user-account-comment x))))))
    (push-cursor (page-cursor-visibility p))

    (page-set-datum! p 'menu menu)
    (page-set-datum! p 'navigation buttons)
    (page-set-datum! p 'text-window text-window)
    (page-set-datum! p 'header-window header-window)
    (menu-post menu mwin)
    (buttons-post buttons bwin)))))
