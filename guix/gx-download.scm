;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
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

(define-module (guix gx-download)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix modules)
  ;; #:autoload   (guix build-system gnu) (standard-packages)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:export (gx-reference
            gx-reference?
            gx-reference-hash

            gx-fetch
            gx-version
            gx-file-name))

;;; Commentary:
;;;
;;; An <origin> method that uses gx to fetch a specific hash over IPFS.
;;; See https://github.com/whyrusleeping/gx.
;;; The hash is specified with a <gx-reference> object.
;;;
;;; Code:

(define-record-type* <gx-reference>
  gx-reference make-gx-reference
  gx-reference?
  (hash gx-reference-hash))

(define (gx-package)
  "Return the default gx package."
  (let ((distro (resolve-interface '(gnu packages ipfs))))
    (module-ref distro 'gx)))

(define* (gx-fetch ref hash-algo hash
                    #:optional name
                    #:key (system (%current-system)) (guile (default-guile))
                    (gx (gx-package)))
  "Return a fixed-output derivation that fetches REF, a <gx-reference>
object.  The output is expected to have recursive hash HASH of type
HASH-ALGO (a symbol).  Use NAME as the file name, or a generic name if #f."
  ;; (define inputs
  ;;   ;; When doing 'git clone --recursive', we need sed, grep, etc. to be
  ;;   ;; available so that 'git submodule' works.
  ;;   ;; (if (git-reference-recursive? ref)
  ;;   ;;     (standard-packages)
  ;;   ;;     '())
  ;;   )

  ;; (define zlib
  ;;   (module-ref (resolve-interface '(gnu packages compression)) 'zlib))

  ;; (define config.scm
  ;;   (scheme-file "config.scm"
  ;;                #~(begin
  ;;                    (define-module (guix config)
  ;;                      #:export (%libz))

  ;;                    (define %libz
  ;;                      #+(file-append zlib "/lib/libz")))))

  ;; (define modules
  ;;   (cons `((guix config) => ,config.scm)
  ;;         (delete '(guix config)
  ;;                 (source-module-closure '((guix build git)
  ;;                                          (guix build utils)
  ;;                                          (guix build download-nar))))))

  (define build
    (with-imported-modules '((guix build gx)
                             (guix build utils))
      #~(begin
          (use-modules (guix build gx)
                       ;; (guix build utils)
                       ;; (guix build download-nar)
                       ;; (ice-9 match)
                       )

          ;; The 'git submodule' commands expects Coreutils, sed,
          ;; grep, etc. to be in $PATH.
          ;; (set-path-environment-variable "PATH" '("bin")
          ;;                                (match '#+inputs
          ;;                                  (((names dirs outputs ...) ...)
          ;;                                   dirs)))

          (or (gx-fetch '#$(gx-reference-hash ref)
                         #$output
                         #:gx-command (string-append #+gx "/bin/gx"))
              ;; (download-nar #$output)
              ))))

  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation (or name "gx-checkout") build
                      #:system system
                      #:local-build? #t
                      #:hash-algo hash-algo
                      #:hash hash
                      #:recursive? #t
                      #:guile-for-build guile)))

(define (gx-version version revision hash)
  "Return the version string for packages using gx-download."
  (string-append version "-" revision "." (string-take hash 7)))

(define (gx-file-name name version)
  "Return the file-name for packages using gx-download."
  (string-append name "-" version "-checkout"))

;;; gx-download.scm ends here
