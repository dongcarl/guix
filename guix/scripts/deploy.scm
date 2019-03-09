;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 David Thompson <davet@gnu.org>
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

(define-module (guix scripts deploy)
  #:use-module (gnu machine)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix scripts build)
  #:use-module (guix store)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:export (guix-deploy))

(define (show-help)
  (display (G_ "Usage: guix deploy WHATEVER\n")))

(define %options
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         %standard-build-options))

(define %default-options
  '((system . ,(%current-system))
    (substitutes? . #t)
    (build-hook? . #t)
    (graft? . #t)
    (print-build-trace? . #t)
    (print-extended-build-trace? . #t)
    (multiplexed-build-output? . #t)
    (debug . 0)
    (verbosity . 2)))

(define (load-source-file file)
  (let ((module (make-user-module '())))
    (load* file module)))

(define (guix-deploy . args)
  (define (handle-argument arg result)
    (alist-cons 'file arg result))
  (let* ((opts (parse-command-line args %options (list %default-options)
                                   #:argument-handler handle-argument))
         (file (assq-ref opts 'file))
         (machines (load-source-file file)))
    (with-store store
      (set-build-options-from-command-line store opts)
      ;; Build all the OSes and create a mapping from machine to OS derivation
      ;; for use in the deploy step.
      (let ((osdrvs (map (lambda (machine)
                           (format #t "building ~a... " (display-name machine))
                           (let ((osdrv (build-os machine store)))
                             (display "done\n")
                             (cons machine osdrv)))
                         machines)))
        (for-each (lambda (machine)
                    (format #t "deploying to ~a... " (display-name machine))
                    (deploy-os machine store (assq-ref osdrvs machine))
                    (display "done\n"))
                  machines)))))
