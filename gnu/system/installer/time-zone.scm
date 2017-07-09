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

(define-module (gnu system installer time-zone)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer utils)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer levelled-stack)
  #:use-module (gurses menu)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (ice-9 match)

  #:export (make-tz-browser))

(include "i18n.scm")

(define (make-tz-browser parent directory)
  (let ((page (make-page (page-surface parent)
			(gettext "Time Zone")
			time-zone-page-refresh
                        0
			#:activator time-zone-page-activate-item)))
    (page-set-datum! page 'directory directory)
    page))


(define my-buttons `((cancel  ,(M_ "Canc_el") #t)))

(define (time-zone-page-activate-item page item)
  (match item
   (('menu-item-activated i)
      (time-zone-page-refresh page) ; FIXME remove
      (let* ((directory (page-datum page 'directory))
             (new-dir (string-append directory "/" i))
             (st (lstat new-dir)))
        (if (and (file-exists? new-dir)
                 (eq? 'directory (stat:type st)))
          (let ((p (make-tz-browser page new-dir)))
            (page-set-datum! p 'stem
               (if (page-datum page 'stem)
                 (string-append (page-datum page 'stem) "/" i)
                 i))
            (page-pop)  ; Don't go back to the current page!
            (page-enter p))
          (begin
            (set! time-zone
              (if (page-datum page 'stem)
                (string-append (page-datum page 'stem) "/" i)
                i))
            (page-leave)
            #f))))
     ('cancel
      (page-leave)
      'cancelled)
     (_
      'ignored)))

(define (time-zone-page-refresh page)
  (when (not (page-initialised? page))
    (time-zone-page-init page)
    (page-set-initialised! page #t))
  (let ((text-window (page-datum page 'text-window)))
    (addstr* text-window (gettext "Select the default time zone for the system:" ))))

(define (time-zone-page-init p)
  (match (create-vbox (page-surface p) 4 (- (getmaxy (page-surface p)) 4 3) 3)
   ((text-window menu-window button-window)
    (let ((buttons (make-buttons my-buttons 1))
          (menu (make-menu
                (let* ((dir (page-datum p 'directory))
                       (all-names (scandir-with-slashes dir))
                       (useful-names (filter (lambda (name)
                                               (and
                                                 (not (string=? "./" name))
                                                 (not (string-suffix? ".tab" name))))
                                             all-names)))
                  (sort useful-names string<)))))

      (menu-post menu menu-window)

      (push-cursor (page-cursor-visibility p))

      (page-set-datum! p 'menu menu)
      (page-set-datum! p 'text-window text-window)
      (page-set-datum! p 'navigation  buttons)
      (buttons-post buttons button-window)))))
