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

(define-module (gnu system installer page)
  #:export (make-page)
  #:export (page-surface)
  #:export (page-refresh)
  #:export (page-initialised?)
  #:export (page-set-initialised!)
  #:export (page-enter)
  #:export (page-leave)
  #:export (page-set-wwin!)
  #:export (page-wwin)
  #:export (page-cursor-visibility)
  #:export (page-title)
  #:export (page-datum)
  #:export (page-set-datum!)
  #:export (page-key-handler)
  #:export (page-mouse-handler)
  #:export (page-default-key-handler)
  #:export (page-default-mouse-handler)

  #:use-module (gurses buttons)
  #:use-module (gurses menu)
  #:use-module (gurses form)
  #:use-module (ncurses curses)
  #:use-module (gnu system installer utils)
  #:use-module (gnu system installer levelled-stack)
  #:use-module (srfi srfi-9))

(define-record-type <page>
  (make-page' surface title inited refresh cursor-visibility key-handler mouse-handler data)
  page?
  (title page-title)
  (surface page-surface)
  (inited  page-initialised? page-set-initialised!)
  (refresh page-refresh)
  (cursor-visibility page-cursor-visibility)
  (key-handler page-key-handler)
  (mouse-handler page-mouse-handler)
  (wwin page-wwin page-set-wwin!)
  (data page-data page-set-data!))

(define (page-activate-focused-item page)
  ((page-datum page 'activator) page))

(define (page-default-mouse-handler page device-id x y z button-state)
  (let* ((menu (page-datum page 'menu))
         (status (if menu
                     (std-menu-mouse-handler menu device-id x y z button-state)
                     'ignored))
         (buttons (page-datum page 'navigation))
         (status (if (and (eq? status 'ignored) buttons)
                     (let ((button-status (buttons-mouse-handler buttons
                                                                 device-id
                                                                 x y z
                                                                 button-state)))
                       (if (and menu (eq? button-status 'activated))
                         (menu-set-active! menu #f))
                       button-status)
                     status)))
    (if (eq? status 'activated)
      (page-activate-focused-item page))
    status))

(define (page-default-key-handler page ch)
  "Handle keypresses in a commonly-used page.
The page is assumed to have only at most a menu, a form and a navigation.
If a form is used it's assumed that the menu is not used and vice versa."
  (let ((menu (page-datum page 'menu))
        (nav  (page-datum page 'navigation))
        (form (page-datum page 'form)))
    (cond
     ((and form (form-enabled? form) (not (eq? 'ignored (form-enter form ch))))
     'handled)

     ((eq? ch KEY_RIGHT)
      (if menu
        (menu-set-active! menu #f))
      (if form
        (form-set-enabled! form #f))
      (if nav
        (buttons-select-next nav))
      'handled)

     ((eq? ch KEY_LEFT)
      (if menu
        (menu-set-active! menu #f))
      (if form
        (form-set-enabled! form #f))
      (if nav
        (buttons-select-prev nav))
      'handled)

     ((eq? ch #\tab)
      (cond
       ((and menu (menu-active menu))
        (menu-set-active! menu #f)
        (if nav
            (buttons-select nav 0))
        'handled)

       ((and form (form-enabled? form))
        (form-set-enabled! form #f)
        (if nav
            (buttons-select nav 0))
        'handled)

       ((and nav (eqv? (buttons-selected nav) (1- (buttons-n-buttons nav))))
        (if menu
          (menu-set-active! menu #t)
          (if form
            (form-set-enabled! form #t)))
        (buttons-unselect-all nav)
        'handled)

       (else
        (if nav
            (buttons-select-next nav))
        'handled)))

     ((select-key? ch)
      (page-activate-focused-item page))

     ((and menu (menu-active menu) (not (eq? 'ignored (std-menu-key-handler menu ch))))
      'handled)

     ((eq? ch KEY_UP)
      (if nav
          (buttons-unselect-all nav))
      (if menu
        (menu-set-active! menu #t)
        (if form
          (form-set-enabled! form #t)))
      'handled)

     ((eq? ch KEY_DOWN)
      (if nav
          (buttons-unselect-all nav))
      (if menu
        (menu-set-active! menu #t)
        (if form
          (form-set-enabled! form #t)))
      'handled)

     ((and nav (buttons-fetch-by-key nav ch))
      (buttons-select-by-symbol nav (buttons-fetch-by-key nav ch))
      (page-activate-focused-item page))

     (else
       'ignored))))


(define* (make-page surface title refresh cursor-visibility
                    #:optional
                    (key-handler page-default-key-handler)
                    (mouse-handler page-default-mouse-handler)
                    #:key
                    activator)
  (let ((result (make-page' surface title #f refresh cursor-visibility key-handler mouse-handler '())))
    (if activator
      (page-set-datum! result 'activator activator))
    result))

(define (page-set-datum! page key value)
  (page-set-data! page (acons key value (page-data page))))

(define (page-datum page key)
  (assq-ref (page-data page) key))

(define (page-leave)
  (pop-cursor)
  (page-pop))

(define (page-enter p)
  (page-push p)
  ((page-refresh p) p))

