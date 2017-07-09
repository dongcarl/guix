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
  #:use-module (ice-9 match)

  #:export (mount-point-refresh)
  #:export (mount-point-page-activate-item))

(include "i18n.scm")

(define validator
  (let ((regexp (make-regexp "^/")))
    (lambda (text)
      (regexp-exec regexp text))))

(define (my-fields) `((mount-point ,(M_ "Mount Point") 40 ,validator)
                      (fs-type     ,(M_ "File System Type")
                                   ,(append valid-file-system-types (list "<unused>")))
                      (label       ,(M_ "Label") 16)))

(define (mount-point-refresh page)
  (when (not (page-initialised? page))
    (mount-point-page-init page)
    (page-set-initialised! page #t))

  (let ((dev (page-datum page 'device))
        (text-window (page-datum page 'text-window)))
    (erase text-window)
    (addstr*
     text-window
     (format #f
             (gettext
             "The device ~s is currently configured as follows.  You may change the configuration here if desired.")
             dev))))

(define (mount-point-page-activate-item page item)
  (let ((form  (page-datum page 'form))
	(nav   (page-datum page 'navigation))
	(dev   (page-datum page 'device)))
    (match (if (eq? item 'default)
               'continue
               item)
     ('continue
      (let ((fss
             (make-file-system-spec
              (form-get-value form 'mount-point)
              (form-get-value form 'label)
              (form-get-value form 'fs-type))))
        (set! mount-points
              (if fss
                  (assoc-set! mount-points dev fss)
                  (assoc-remove! mount-points dev)))
        (page-leave)
        'handled))

     ('cancel
      ;; Close the menu and return
      (page-leave)
      'handled)

     (_ 'ignored))))

(define my-buttons `((continue ,(M_ "Continue") #f)
                     (cancel     ,(M_ "Cancel") #f)))

(define (mount-point-page-init p)
  (match (create-vbox (page-surface p) 3 (- (getmaxy (page-surface p)) 3 3) 3)
    ((text-window fw bwin)
     (let ((nav (make-buttons my-buttons 1))
           (form (make-form
                (my-fields)
                (lambda (f)
                  (let ((field (get-current-field f)))
                    (case (field-symbol field)
                      ((mount-point)
                       (form-set-value! f 'label
                                        (string-append
                                         host-name "-"
                                         (form-get-value f 'mount-point))))
                      ((fs-type)
                       (cond
                        ((equal? "swap" (form-get-value f 'fs-type))
                         (form-set-value! f 'label "swap-space")
                         (form-set-value! f 'mount-point ""))
                        ((equal? "<unused>" (form-get-value f 'fs-type))
                         (form-set-value! f 'label "")
                         (form-set-value! f 'mount-point ""))))))))))

      (page-set-datum! p 'text-window text-window)
      (page-set-datum! p 'navigation nav)
      (form-post form fw)

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
      (page-set-datum! p 'form form)))))

