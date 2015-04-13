;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
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
  #:use-module (guix ui)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix utils)
  #:use-module (guix monads)
  #:use-module (guix build utils)
  #:use-module (guix scripts)
  #:use-module (guix scripts build)
  #:use-module (gnu packages)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu machines)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-98)
  #:export (guix-deploy))

(define (show-help)
  (display (_ "Usage: guix deploy [OPTION] ACTION FILE
Manage your data beans without disturbing Terry the data goblin.\n"))
  (newline)
  (display (_ "The valid values for ACTION are:\n"))
  (display (_ "\
  - 'build', build all of the operating systems without deploying\n"))
  (display (_ "\
  - 'init', provision and install the operating systems\n"))
  (display (_ "\
  - 'reconfigure', update an existing deployment\n"))
  (display (_ "\
  - 'destroy', unprovision the deployed operating systems\n"))
  (display (_ "
  -e, --expression=EXPR  create environment for the package that EXPR
                         evaluates to"))
  (newline)
  (show-build-options-help)
  (newline)
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %default-options
  `((substitutes? . #t)
    (max-silent-time . 3600)
    (verbosity . 0)))

(define %options
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix deploy")))
         %standard-build-options))

(define-syntax-rule (return* body ...)
  "Generate the monadic form of BODY, an expression evaluated for its
side-effects.  The result is always #t."
  (return (begin body ... #t)))

(define (deployment-derivations deployment)
  (map (lambda (machine)
         (operating-system-derivation
          (machine-os-for-platform machine)))
       (deployment-machines deployment)))

(define (build-deployment deployment)
  (mlet* %store-monad
      ((drvs (sequence %store-monad (deployment-derivations deployment))))
    (mbegin %store-monad
      (show-what-to-build* drvs)
      (built-derivations drvs)
      (return*
       (for-each (lambda (drv)
                   (display (derivation->output-path drv))
                   (newline))
                 drvs)))))

(define (provision-deployment deployment)
  (sequence %store-monad
            (map (lambda (machine)
                   (mlet %store-monad
                       ((state (provision-machine machine)))
                     (return (list machine state))))
                 (deployment-machines deployment))))

(define (spawn-deployment deployment)
  (mlet %store-monad
      ((states (provision-deployment deployment)))
    (sequence %store-monad
              (map (match-lambda
                    ((machine state)
                     (return* (boot-machine machine state))))
                   states))))

(define (perform-action action deployment)
  (case action
    ((build) (build-deployment deployment))
    ((provision) (provision-deployment deployment))
    ((spawn) (spawn-deployment deployment))))

(define (guix-deploy . args)
  (define (parse-sub-command-or-config arg result)
    (cond
     ((assoc-ref result 'config)
      (leave (_ "~a: extraneous argument~%") arg))
     ((assoc-ref result 'action)
      (alist-cons 'config arg result))
     (else
      (let ((action (string->symbol arg)))
        (case action
          ((build provision spawn)
           (alist-cons 'action action result))
          (else (leave (_ "~a: unknown action~%") action)))))))

  (with-error-handling
    (let* ((opts (args-fold* args %options
                             (lambda (opt name arg result)
                               (leave (_ "~A: unrecognized option~%") name))
                             parse-sub-command-or-config %default-options))
           (action (assoc-ref opts 'action))
           (deployment (primitive-load (assoc-ref opts 'config))))
      (with-store store
        (run-with-store store
          (mbegin %store-monad
            (set-build-options-from-command-line* opts)
            (perform-action action deployment)))))))
