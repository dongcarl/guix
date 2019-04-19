;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
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


(define-module (guix store derivations)
  #:use-module (ice-9 match)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (gcrypt hash)
  #:use-module (guix base16)
  #:use-module (guix combinators)
  #:use-module (guix memoization)
  #:use-module (guix sets)
  #:use-module (guix store files)
  #:export (&derivation-error
            derivation-error?
            derivation-error-derivation

            &derivation-missing-output-error
            derivation-missing-output-error?
            derivation-missing-output

            <derivation>
            make-derivation
            derivation?
            derivation-outputs
            derivation-inputs
            derivation-sources
            derivation-system
            derivation-builder
            derivation-builder-arguments
            derivation-builder-environment-vars
            derivation-file-name

            <derivation-output>
            make-derivation-output
            derivation-output?
            derivation-output-path
            derivation-output-hash-algo
            derivation-output-hash
            derivation-output-recursive?

            <derivation-input>
            make-derivation-input
            derivation-input?
            derivation-input-path
            derivation-input-sub-derivations
            coalesce-duplicate-inputs

            derivation-name
            derivation-path->base16-hash
            derivation-output-names
            derivation-hash
            derivation-properties
            fixed-output-derivation?
            offloadable-derivation?
            substitutable-derivation?

            derivation-input<?
            derivation-input-output-paths
            derivation-output-paths
            derivation->output-path
            derivation->output-paths
            derivation-path->output-path
            derivation-path->output-paths

            derivation-prerequisites

            derivation/masked-inputs
            read-derivation
            read-derivation-from-file
            derivation->bytevector
            %derivation-cache
            write-derivation
            invalidate-derivation-caches!))

;;;
;;; Nix derivations, as implemented in Nix's `derivations.cc'.
;;;

(define-immutable-record-type <derivation>
  (make-derivation outputs inputs sources system builder args env-vars
                   file-name)
  derivation?
  (outputs  derivation-outputs)      ; list of name/<derivation-output> pairs
  (inputs   derivation-inputs)       ; list of <derivation-input>
  (sources  derivation-sources)      ; list of store paths
  (system   derivation-system)       ; string
  (builder  derivation-builder)      ; store path
  (args     derivation-builder-arguments)         ; list of strings
  (env-vars derivation-builder-environment-vars)  ; list of name/value pairs
  (file-name derivation-file-name))               ; the .drv file name

(define-immutable-record-type <derivation-output>
  (make-derivation-output path hash-algo hash recursive?)
  derivation-output?
  (path       derivation-output-path)             ; store path
  (hash-algo  derivation-output-hash-algo)        ; symbol | #f
  (hash       derivation-output-hash)             ; bytevector | #f
  (recursive? derivation-output-recursive?))      ; Boolean

(define-immutable-record-type <derivation-input>
  (make-derivation-input path sub-derivations)
  derivation-input?
  (path            derivation-input-path)             ; store path
  (sub-derivations derivation-input-sub-derivations)) ; list of strings

(set-record-type-printer! <derivation>
                          (lambda (drv port)
                            (format port "#<derivation ~a => ~a ~a>"
                                    (derivation-file-name drv)
                                    (string-join
                                     (map (match-lambda
                                           ((_ . output)
                                            (derivation-output-path output)))
                                          (derivation-outputs drv)))
                                    (number->string (object-address drv) 16))))

;;;
;;; Error conditions.
;;;

(define-condition-type &derivation-error &store-error
  derivation-error?
  (derivation derivation-error-derivation))

(define-condition-type &derivation-missing-output-error &derivation-error
  derivation-missing-output-error?
  (output derivation-missing-output))


(define (derivation-name drv)
  "Return the base name of DRV."
  (let ((base (store-path-package-name (derivation-file-name drv))))
    (string-drop-right base 4)))

(define (derivation-output-names drv)
  "Return the names of the outputs of DRV."
  (match (derivation-outputs drv)
    (((names . _) ...)
     names)))

(define (fixed-output-derivation? drv)
  "Return #t if DRV is a fixed-output derivation, such as the result of a
download with a fixed hash (aka. `fetchurl')."
  (match drv
    (($ <derivation>
        (("out" . ($ <derivation-output> _ (? symbol?) (? bytevector?)))))
     #t)
    (_ #f)))

(define (derivation-input<? input1 input2)
  "Compare INPUT1 and INPUT2, two <derivation-input>."
  (string<? (derivation-input-path input1)
            (derivation-input-path input2)))

(define (coalesce-duplicate-inputs inputs)
  "Return a list of inputs, such that when INPUTS contains the same DRV twice,
they are coalesced, with their sub-derivations merged.  This is needed because
Nix itself keeps only one of them."
  (fold (lambda (input result)
          (match input
            (($ <derivation-input> path sub-drvs)
             ;; XXX: quadratic
             (match (find (match-lambda
                            (($ <derivation-input> p s)
                             (string=? p path)))
                          result)
               (#f
                (cons input result))
               ((and dup ($ <derivation-input> _ sub-drvs2))
                ;; Merge DUP with INPUT.
                (let ((sub-drvs (delete-duplicates
                                 (append sub-drvs sub-drvs2))))
                  (cons (make-derivation-input path
                                               (sort sub-drvs string<?))
                        (delq dup result))))))))
        '()
        inputs))

(define* (derivation-prerequisites drv #:optional (cut? (const #f)))
  "Return the list of derivation-inputs required to build DRV, recursively.

CUT? is a predicate that is passed a derivation-input and returns true to
eliminate the given input and its dependencies from the search.  An example of
such a predicate is 'valid-derivation-input?'; when it is used as CUT?, the
result is the set of prerequisites of DRV not already in valid."
  (let loop ((drv       drv)
             (result    '())
             (input-set (set)))
    (let ((inputs (remove (lambda (input)
                            (or (set-contains? input-set input)
                                (cut? input)))
                          (derivation-inputs drv))))
      (fold2 loop
             (append inputs result)
             (fold set-insert input-set inputs)
             (map (lambda (i)
                    (read-derivation-from-file (derivation-input-path i)))
                  inputs)))))

(define (offloadable-derivation? drv)
  "Return true if DRV can be offloaded, false otherwise."
  (match (assoc "preferLocalBuild"
                (derivation-builder-environment-vars drv))
    (("preferLocalBuild" . "1") #f)
    (_ #t)))

(define (substitutable-derivation? drv)
  "Return #t if DRV can be substituted."
  (match (assoc "allowSubstitutes"
                (derivation-builder-environment-vars drv))
    (("allowSubstitutes" . value)
     (string=? value "1"))
    (_ #t)))

(define (derivation-output-paths drv sub-drvs)
  "Return the output paths of outputs SUB-DRVS of DRV."
  (match drv
    (($ <derivation> outputs)
     (map (lambda (sub-drv)
            (derivation-output-path (assoc-ref outputs sub-drv)))
          sub-drvs))))

(define derivation-path->base16-hash
  (mlambda (file)
    "Return a string containing the base16 representation of the hash of the
derivation at FILE."
    (bytevector->base16-string
     (derivation-hash (read-derivation-from-file file)))))

(define (derivation/masked-inputs drv)
  "Assuming DRV is a regular derivation (not fixed-output), replace the file
name of each input with that input's hash."
  (match drv
    (($ <derivation> outputs inputs sources
                     system builder args env-vars)
     (let ((inputs (map (match-lambda
                          (($ <derivation-input> path sub-drvs)
                           (let ((hash (derivation-path->base16-hash path)))
                             (make-derivation-input hash sub-drvs))))
                        inputs)))
       (make-derivation outputs
                        (sort (coalesce-duplicate-inputs inputs)
                              derivation-input<?)
                        sources
                        system builder args env-vars
                        #f)))))

(define derivation-hash            ; `hashDerivationModulo' in derivations.cc
  (lambda (drv)
    "Return the hash of DRV, modulo its fixed-output inputs, as a bytevector."
    (match drv
      (($ <derivation> ((_ . ($ <derivation-output> path
                                                    (? symbol? hash-algo) (? bytevector? hash)
                                                    (? boolean? recursive?)))))
       ;; A fixed-output derivation.
       (sha256
        (string->utf8
         (string-append "fixed:out:"
                        (if recursive? "r:" "")
                        (symbol->string hash-algo)
                        ":" (bytevector->base16-string hash)
                        ":" path))))
      (_

       ;; XXX: At this point this remains faster than `port-sha256', because
       ;; the SHA256 port's `write' method gets called for every single
       ;; character.
       (sha256 (derivation->bytevector (derivation/masked-inputs drv)))))))

(define (invalidate-derivation-caches!)
  "Invalidate internal derivation caches.  This is mostly useful for
long-running processes that know what they're doing.  Use with care!"
  ;; Typically this is meant to be used by Cuirass and Hydra, which can clear
  ;; caches when they start evaluating packages for another architecture.
  (invalidate-memoization! derivation->bytevector)
  (invalidate-memoization! derivation-path->base16-hash)
  (hash-clear! %derivation-cache))

(define derivation-properties
  (mlambdaq (drv)
    "Return the property alist associated with DRV."
    (match (assoc "guix properties"
                  (derivation-builder-environment-vars drv))
      ((_ . str) (call-with-input-string str read))
      (#f        '()))))

(define (derivation-input-output-paths input)
  "Return the list of output paths corresponding to INPUT, a
<derivation-input>."
  (match input
    (($ <derivation-input> path sub-drvs)
     (map (cut derivation-path->output-path path <>)
          sub-drvs))))

(define* (derivation->output-path drv #:optional (output "out"))
  "Return the store path of its output OUTPUT.  Raise a
'&derivation-missing-output-error' condition if OUTPUT is not an output of
DRV."
  (let ((output* (assoc-ref (derivation-outputs drv) output)))
    (if output*
        (derivation-output-path output*)
        (raise (condition (&derivation-missing-output-error
                           (derivation drv)
                           (output output)))))))

(define (derivation->output-paths drv)
  "Return the list of name/path pairs of the outputs of DRV."
  (map (match-lambda
        ((name . output)
         (cons name (derivation-output-path output))))
       (derivation-outputs drv)))

(define derivation-path->output-path
  ;; This procedure is called frequently, so memoize it.
  (let ((memoized (mlambda (path output)
                    (derivation->output-path (read-derivation-from-file path)
                                             output))))
    (lambda* (path #:optional (output "out"))
      "Read the derivation from PATH (`/gnu/store/xxx.drv'), and return the store
path of its output OUTPUT."
      (memoized path output))))

(define (derivation-path->output-paths path)
  "Read the derivation from PATH (`/gnu/store/xxx.drv'), and return the
list of name/path pairs of its outputs."
  (derivation->output-paths (read-derivation-from-file path)))

(define (read-derivation drv-port)
  "Read the derivation from DRV-PORT and return the corresponding <derivation>
object.  Most of the time you'll want to use 'read-derivation-from-file',
which caches things as appropriate and is thus more efficient."

  (define comma (string->symbol ","))

  (define (ununquote x)
    (match x
      (('unquote x) (ununquote x))
      ((x ...)      (map ununquote x))
      (_            x)))

  (define (outputs->alist x)
    (fold-right (lambda (output result)
                  (match output
                    ((name path "" "")
                     (alist-cons name
                                 (make-derivation-output path #f #f #f)
                                 result))
                    ((name path hash-algo hash)
                     ;; fixed-output
                     (let* ((rec? (string-prefix? "r:" hash-algo))
                            (algo (string->symbol
                                   (if rec?
                                       (string-drop hash-algo 2)
                                       hash-algo)))
                            (hash (base16-string->bytevector hash)))
                       (alist-cons name
                                   (make-derivation-output path algo
                                                           hash rec?)
                                   result)))))
                '()
                x))

  (define (make-input-drvs x)
    (fold-right (lambda (input result)
                  (match input
                    ((path (sub-drvs ...))
                     (cons (make-derivation-input path sub-drvs)
                           result))))
                '()
                x))

  ;; The contents of a derivation are typically ASCII, but choosing
  ;; UTF-8 allows us to take the fast path for Guile's `scm_getc'.
  (set-port-encoding! drv-port "UTF-8")

  (let loop ((exp    (read drv-port))
             (result '()))
    (match exp
      ((? eof-object?)
       (let ((result (reverse result)))
         (match result
           (('Derive ((outputs ...) (input-drvs ...)
                      (input-srcs ...)
                      (? string? system)
                      (? string? builder)
                      ((? string? args) ...)
                      ((var value) ...)))
            (make-derivation (outputs->alist outputs)
                             (make-input-drvs input-drvs)
                             input-srcs
                             system builder args
                             (fold-right alist-cons '() var value)
                             (port-filename drv-port)))
           (_
            (error "failed to parse derivation" drv-port result)))))
      ((? (cut eq? <> comma))
       (loop (read drv-port) result))
      (_
       (loop (read drv-port)
             (cons (ununquote exp) result))))))

(define %derivation-cache
  ;; Maps derivation file names to <derivation> objects.
  ;; XXX: This is redundant with 'atts-cache' in the store.
  (make-weak-value-hash-table 200))

(define (read-derivation-from-file file)
  "Read the derivation in FILE, a '.drv' file, and return the corresponding
<derivation> object."
  ;; Memoize that operation because 'read-derivation' is quite expensive,
  ;; and because the same argument is read more than 15 times on average
  ;; during something like (package-derivation s gdb).
  (or (and file (hash-ref %derivation-cache file))
      (let ((drv (call-with-input-file file read-derivation)))
        (hash-set! %derivation-cache file drv)
        drv)))

(define-inlinable (write-sequence lst write-item port)
  ;; Write each element of LST with WRITE-ITEM to PORT, separating them with a
  ;; comma.
  (match lst
    (()
     #t)
    ((prefix (... ...) last)
     (for-each (lambda (item)
                 (write-item item port)
                 (display "," port))
               prefix)
     (write-item last port))))

(define-inlinable (write-list lst write-item port)
  ;; Write LST as a derivation list to PORT, using WRITE-ITEM to write each
  ;; element.
  (display "[" port)
  (write-sequence lst write-item port)
  (display "]" port))

(define-inlinable (write-tuple lst write-item port)
  ;; Same, but write LST as a tuple.
  (display "(" port)
  (write-sequence lst write-item port)
  (display ")" port))

(define (write-derivation drv port)
  "Write the ATerm-like serialization of DRV to PORT.  See Section 2.4 of
Eelco Dolstra's PhD dissertation for an overview of a previous version of
that form."

  ;; Make sure we're using the faster implementation.
  (define format simple-format)

  (define (write-string-list lst)
    (write-list lst write port))

  (define (write-output output port)
    (match output
     ((name . ($ <derivation-output> path hash-algo hash recursive?))
      (write-tuple (list name path
                         (if hash-algo
                             (string-append (if recursive? "r:" "")
                                            (symbol->string hash-algo))
                             "")
                         (or (and=> hash bytevector->base16-string)
                             ""))
                   write
                   port))))

  (define (write-input input port)
    (match input
      (($ <derivation-input> path sub-drvs)
       (display "(\"" port)
       (display path port)
       (display "\"," port)
       (write-string-list sub-drvs)
       (display ")" port))))

  (define (write-env-var env-var port)
    (match env-var
      ((name . value)
       (display "(" port)
       (write name port)
       (display "," port)
       (write value port)
       (display ")" port))))

  ;; Assume all the lists we are writing are already sorted.
  (match drv
    (($ <derivation> outputs inputs sources
        system builder args env-vars)
     (display "Derive(" port)
     (write-list outputs write-output port)
     (display "," port)
     (write-list inputs write-input port)
     (display "," port)
     (write-string-list sources)
     (simple-format port ",\"~a\",\"~a\"," system builder)
     (write-string-list args)
     (display "," port)
     (write-list env-vars write-env-var port)
     (display ")" port))))

(define derivation->bytevector
  (mlambda (drv)
    "Return the external representation of DRV as a UTF-8-encoded string."
    (with-fluids ((%default-port-encoding "UTF-8"))
      (call-with-values open-bytevector-output-port
        (lambda (port get-bytevector)
          (write-derivation drv port)
          (get-bytevector))))))

