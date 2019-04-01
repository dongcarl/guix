;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2019 Caleb Ristvedt <caleb.ristvedt@cune.org>
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

(define-module (guix store files)
  #:use-module (ice-9 regex)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-26)
  #:use-module (gcrypt hash)
  #:use-module (guix base32)
  #:use-module (guix base16)
  #:use-module (guix config)
  #:use-module (guix memoization)
  #:export (%store-prefix
            store-path
            output-path
            fixed-output-path
            store-path?
            direct-store-path?
            derivation-path?
            store-path-package-name
            store-path-hash-part
            direct-store-path
            derivation-log-file
            log-file))

;;;
;;; Store paths.
;;;

(define %store-prefix
  ;; Absolute path to the Nix store.
  (make-parameter %store-directory))

(define (compressed-hash bv size)                 ; `compressHash'
  "Given the hash stored in BV, return a compressed version thereof that fits
in SIZE bytes."
  (define new (make-bytevector size 0))
  (define old-size (bytevector-length bv))
  (let loop ((i 0))
    (if (= i old-size)
        new
        (let* ((j (modulo i size))
               (o (bytevector-u8-ref new j)))
          (bytevector-u8-set! new j
                              (logxor o (bytevector-u8-ref bv i)))
          (loop (+ 1 i))))))

(define (store-path type hash name)               ; makeStorePath
  "Return the store path for NAME/HASH/TYPE."
  (let* ((s (string-append type ":sha256:"
                           (bytevector->base16-string hash) ":"
                           (%store-prefix) ":" name))
         (h (sha256 (string->utf8 s)))
         (c (compressed-hash h 20)))
    (string-append (%store-prefix) "/"
                   (bytevector->nix-base32-string c) "-"
                   name)))

(define (output-path output hash name)            ; makeOutputPath
  "Return an output path for OUTPUT (the name of the output as a string) of
the derivation called NAME with hash HASH."
  (store-path (string-append "output:" output) hash
              (if (string=? output "out")
                  name
                  (string-append name "-" output))))

(define* (fixed-output-path name hash
                            #:key
                            (output "out")
                            (hash-algo 'sha256)
                            (recursive? #t))
  "Return an output path for the fixed output OUTPUT defined by HASH of type
HASH-ALGO, of the derivation NAME.  RECURSIVE? has the same meaning as for
'add-to-store'."
  (if (and recursive? (eq? hash-algo 'sha256))
      (store-path "source" hash name)
      (let ((tag (string-append "fixed:" output ":"
                                (if recursive? "r:" "")
                                (symbol->string hash-algo) ":"
                                (bytevector->base16-string hash) ":")))
        (store-path (string-append "output:" output)
                    (sha256 (string->utf8 tag))
                    name))))

(define (store-path? path)
  "Return #t if PATH is a store path."
  ;; This is a lightweight check, compared to using a regexp, but this has to
  ;; be fast as it's called often in `derivation', for instance.
  ;; `isStorePath' in Nix does something similar.
  (string-prefix? (%store-prefix) path))

(define (direct-store-path? path)
  "Return #t if PATH is a store path, and not a sub-directory of a store path.
This predicate is sometimes needed because files *under* a store path are not
valid inputs."
  (and (store-path? path)
       (not (string=? path (%store-prefix)))
       (let ((len (+ 1 (string-length (%store-prefix)))))
         (not (string-index (substring path len) #\/)))))

(define (direct-store-path path)
  "Return the direct store path part of PATH, stripping components after
'/gnu/store/xxxx-foo'."
  (let ((prefix-length (+ (string-length (%store-prefix)) 35)))
    (if (> (string-length path) prefix-length)
        (let ((slash (string-index path #\/ prefix-length)))
          (if slash (string-take path slash) path))
        path)))

(define (derivation-path? path)
  "Return #t if PATH is a derivation path."
  (and (store-path? path) (string-suffix? ".drv" path)))

(define store-regexp*
  ;; The substituter makes repeated calls to 'store-path-hash-part', hence
  ;; this optimization.
  (mlambda (store)
    "Return a regexp matching a file in STORE."
    (make-regexp (string-append "^" (regexp-quote store)
                                "/([0-9a-df-np-sv-z]{32})-([^/]+)$"))))

(define (store-path-package-name path)
  "Return the package name part of PATH, a file name in the store."
  (let ((path-rx (store-regexp* (%store-prefix))))
    (and=> (regexp-exec path-rx path)
           (cut match:substring <> 2))))

(define (store-path-hash-part path)
  "Return the hash part of PATH as a base32 string, or #f if PATH is not a
syntactically valid store path."
  (and (string-prefix? (%store-prefix) path)
       (let ((base (string-drop path (+ 1 (string-length (%store-prefix))))))
         (and (> (string-length base) 33)
              (let ((hash (string-take base 32)))
                (and (string-every %nix-base32-charset hash)
                     hash))))))

(define (derivation-log-file drv)
  "Return the build log file for DRV, a derivation file name, or #f if it
could not be found."
  (let* ((base    (basename drv))
         (log     (string-append (or (getenv "GUIX_LOG_DIRECTORY")
                                     (string-append %localstatedir "/log/guix"))
                                 "/drvs/"
                                 (string-take base 2) "/"
                                 (string-drop base 2)))
         (log.gz  (string-append log ".gz"))
         (log.bz2 (string-append log ".bz2")))
    (cond ((file-exists? log.gz) log.gz)
          ((file-exists? log.bz2) log.bz2)
          ((file-exists? log) log)
          (else #f))))


