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

(define-module (gnu system installer ping)
  #:use-module (gnu system installer partition-reader)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:export (substitute-is-reachable?)
  #:export (ping-page-refresh)
  #:export (ping-page-activate-item))

(include "i18n.scm")

(define (substitute-is-reachable?)
  "Return #t if at least one substitute URL responds to pings"
  (with-output-to-file "/dev/null"
    (lambda ()
      (with-error-to-file "/dev/null"
        (lambda ()
          (fold (lambda (x prev)
                  (or prev
                      (zero? (system*
                              "ping" "-q" "-c" "1"
                              (uri-host (string->uri x))))))
                #f %default-substitute-urls))))))

(define my-buttons `((test ,(M_ "_Test") #t)
		     (continue  ,(M_ "_Continue") #t)
		     (cancel     ,(M_ "Canc_el") #t)))

(define (ping-page-activate-item page item)
  (let ((test-window  (page-datum page 'test-window)))
    (match item
     ('cancel
      ;; Close the menu and return
      (page-leave)
      'handled)

     ('continue
      (delwin (page-datum page 'test-window))
      (page-leave)
      'handled)

     ('test
      (let* ()
        (if (zero? (window-pipe test-window  "ping" "ping" "-c" "3"
                          (uri-host
                           (string->uri
                            (car %default-substitute-urls)))))
            (addstr test-window (G_ "Test successful.  Network is working."))
            (addstr test-window (G_ "Test failed. No servers reached.")))
	'handled))
     (_
       'ignored))))

(define (ping-page-refresh page)
  (when (not (page-initialised? page))
    (ping-page-init page)
    (page-set-initialised! page #t))

  (let ((text-window (page-datum page 'text-window)))
        (erase text-window)
        (addstr* text-window
                 (G_ "Choose \"Test\" to check network connectivity."))))

(define (ping-page-init p)
  (match (create-vbox (page-surface p) 4 (- (getmaxy (page-surface p)) 4 3) 3)
   ((text-window test-window button-window)
    (let ((buttons (make-buttons my-buttons 1)))
      (box test-window 0 0)
      (page-set-datum! p 'test-window test-window)
      (page-set-datum! p 'text-window text-window)
      (page-set-datum! p 'navigation buttons)
      (buttons-post buttons button-window)))))
