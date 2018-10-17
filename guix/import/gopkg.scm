;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
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

(define-module (guix import gopkg)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module ((ice-9 rdelim) #:select (read-line))
  #:use-module (srfi srfi-11)
  #:use-module (texinfo string-utils) ; transform-string
  #:use-module (gcrypt hash)
  ;; #:use-module (guix hash)
  #:use-module (guix base32)
  #:use-module (guix serialization)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (gopkg->guix-package))

(define (vcs-file? file stat)
  ;; TODO: Factorize
  (case (stat:type stat)
    ((directory)
     (member (basename file) '(".bzr" ".git" ".hg" ".svn" "CVS")))
    ((regular)
     ;; Git sub-modules have a '.git' file that is a regular text file.
     (string=? (basename file) ".git"))
    (else
     #f)))

(define (file->hash-base32 file)
  "Return hash of FILE in nix base32 sha256 format.  If FILE is a directory,
exclude vcs files."
  (let-values (((port get-hash) (open-sha256-port)))
    (write-file file port #:select? (negate vcs-file?))
    (force-output port)
    (bytevector->nix-base32-string (get-hash))))

(define (git->hash url commit file)
  "Clone git repository and return FILE hash in nix base32 sha256 format."
  (if (not (file-exists? (string-append file "/.git")))
      (git-fetch url commit file #:recursive? #f))
  (file->hash-base32 file))

(define (git-ref->commit path tag)
  "Return commit number coresponding to git TAG.  Return \"XXX\" if tag is not
found."
  (define (loop port)
    (let ((line (read-line port)))
      (cond
       ((eof-object? line)              ; EOF
        (begin
          (close-port port)
          "XXX"))
       ((string-match tag line)         ; Match tag
        (let ((commit (car (string-split (transform-string line #\tab " ")
                                         #\ ))))
          commit))
       (else                            ; Else
        (loop port)))))

  (let ((file (if (file-exists? (string-append path "/.git/packed-refs"))
                  (string-append path "/.git/packed-refs")
                  (string-append path "/.git/FETCH_HEAD"))))
    (loop (open-input-file file))))

(define* (git-fetch url commit directory
                    #:key (git-command "git") recursive?)
  "Fetch COMMIT from URL into DIRECTORY.  COMMIT must be a valid Git commit
identifier.  When RECURSIVE? is true, all the sub-modules of URL are fetched,
recursively.  Return #t on success, #f otherwise."
  (mkdir-p directory)
  
  (with-directory-excursion directory
    (invoke git-command "init")
    (invoke git-command "remote" "add" "origin" url)
    (if (zero? (system* git-command "fetch" "--depth" "1" "origin" commit))
        (invoke git-command "checkout" "FETCH_HEAD")
        (begin
          (invoke git-command "fetch" "origin")
          (if (not (zero? (system* git-command "checkout" commit)))
              (let ((commit-hash (git-ref->commit directory commit)))
                (invoke git-command "checkout" "master")
                (if (not (equal? "XXX" commit-hash)) ;HACK else stay on master
                    (zero? (system* git-command "checkout" commit-hash))))
              #t)))))

;;
;; Append attributes.
;;

(define (append-inputs inputs name)
  "Return list with new input corresponding to package NAME."
  (let ((unquote-name (list 'unquote (string->symbol name))))
    (append inputs (list (list name unquote-name)))))

;;
;; Parse attributes.
;;

(define (url->package-name url)
  "Compute URL and return package name."
  (let* ((url-no-slash (string-replace-substring url "/" "-"))
         (url-no-slash-no-dot (string-replace-substring url-no-slash
                                                        "." "-")))
    (string-downcase (string-append "go-" url-no-slash-no-dot))))

(define (cut-url url)
  "Return URL without protocol prefix and git file extension."
  (string-replace-substring
   (cond
    ((string-match "http://"  url)
     (string-replace-substring url "http://" ""))
    ((string-match "https://" url)
     (string-replace-substring url "https://" ""))
    ((string-match "git://"   url)
     (string-replace-substring url "git://" ""))
    (else
     url))
   ".git" ""))

(define (url->dn url)
  "Return the web site DN form url 'gnu.org/software/guix' --> 'gnu.org'"
  (car (string-split url #\/)))

(define (url->git-url url)
  (string-append "https://" url ".git"))

(define (comment? line)
  "Return #t if LINE start with comment delimiter, else return #f."
  (eq? (string-ref (string-trim line) 0) #\#))

(define (empty-line? line)
  "Return #t if LINE is empty, else #f."
  (string-null? (string-trim line)))

(define (attribute? line attribute)
  "Return #t if LINE contain ATTRIBUTE."
  (equal? (string-trim-right
           (string-trim
            (car (string-split line #\=)))) attribute))

(define (attribute-by-name line name)
  "Return attribute value corresponding to NAME."
  (let* ((line-no-attribut-name (string-replace-substring
                                 line
                                 (string-append name " = ") ""))
         (value-no-double-quote (string-replace-substring
                                 line-no-attribut-name
                                 "\"" "")))
    (string-trim value-no-double-quote)))

;;
;; Packages functions.
;;

(define (make-go-sexp->package packages dependencies
                               name url version revision
                               commit str-license home-page
                               git-url is-dep? hash)
  "Create Guix sexp package for Go software NAME. Return new package sexp."
  (define (package-inputs)
    (if (not is-dep?)
        `((native-inputs ,(list 'quasiquote dependencies)))
        '()))

  (values
   `(define-public ,(string->symbol name)
      (let ((commit ,commit)
            (revision ,revision))
        (package
          (name ,name)
          (version (git-version ,version revision commit))
          (source (origin
                    (method git-fetch)
                    (uri (git-reference
                          (url ,git-url)
                          (commit commit)))
                    (file-name (git-file-name name version))
                    (sha256
                     (base32
                      ,hash))))
          (build-system go-build-system)
          (arguments
           '(#:import-path ,url))
          ,@(package-inputs)
          (home-page ,home-page)
          (synopsis "XXX")
          (description "XXX")
          (license #f))))))

(define (create-package->packages+dependencies packages dependencies
                                               url version directory
                                               revision commit
                                               constraint? is-dep?)
  "Return packages and dependencies with new package sexp corresponding to
URL."
  (call-with-temporary-directory
   (lambda (dir)
     (let ((name      (url->package-name url))
           (home-page (string-append "https://" url))
           (git-url   (url->git-url url))
           (synopsis    "XXX")
           (description "XXX")
           (license     "XXX"))
       (let ((hash (git->hash (url->git-url url)
                              commit
                              dir))
             (commit-hash (if (< (string-length commit) 40)
                              (git-ref->commit dir
                                               commit)
                              commit)))
         (values
          (append packages
                  (list
                   (make-go-sexp->package packages dependencies
                                          name url version
                                          revision commit-hash
                                          license home-page
                                          git-url is-dep? hash)))
          (if constraint?
              (append-inputs dependencies name)
              dependencies)))))))

(define (parse-dependencies->packages+dependencies port constraint?
                                                   packages dependencies)
  "Parse one dependencies in PORT, and return packages and dependencies list."
  (let ((url "XXX")
        (version "0.0.0")
        (revision "0")
        (commit "XXX"))
    (define (loop port url commit packages dependencies)
      (let ((line (read-line port)))
        (cond
         ((eof-object? line)            ; EOF
          (values packages dependencies))
         ((empty-line? line)                               ; Empty line
          (if (not (or (equal? "k8s.io" (url->dn url))     ; HACK bypass k8s
                       (equal? "golang.org" (url->dn url)) ; HACK bypass golang
                       (equal? "cloud.google.com" (url->dn url)))) ; HACK bypass cloud.google
              (create-package->packages+dependencies packages dependencies
                                                     url version port revision
                                                     commit
                                                     constraint? #t)
              (values packages dependencies)))
         ((comment? line)               ; Comment
          (loop port url commit
                packages dependencies))
         ((attribute? line "name")      ; Name
          (loop port
                (attribute-by-name line "name")
                commit
                packages dependencies))
         ((attribute? line "revision")  ; Revision
          (loop port
                url
                (attribute-by-name line "revision")
                packages dependencies))
         ((attribute? line "version")   ; Version
          (loop port
                url
                (attribute-by-name line "version")
                packages dependencies))
         ((attribute? line "branch")    ; Branch
          (loop port
                url
                (attribute-by-name line "branch")
                packages dependencies))
         ((string-match "=" line)       ; Other options
          (loop port url commit
                packages dependencies))
         (else (loop port url commit
                     packages dependencies)))))
    (loop port url commit
          packages dependencies)))

(define (parse-toml->packages+dependencies port packages dependencies)
  "Read toml file on PORT and return all dependencies packages sexp and list
of constraint dependencies."
  (define (loop port packages dependencies)
    (let ((line (read-line port)))
      (cond
       ((eof-object? line)              ; EOF
        (values packages dependencies))
       ((empty-line? line)              ; Empty line
        (loop port packages dependencies))
       ((comment? line)                 ; Comment
        (loop port packages dependencies))
       ((equal? line "[prune]")         ; Ignored
        (loop port packages dependencies))
       ((equal? "[[constraint]]" line)  ; Direct dependencies
        (let-values (((packages dependencies)
                      (parse-dependencies->packages+dependencies port #t
                                                                 packages
                                                                 dependencies)))
          (loop port packages dependencies)))
       ((equal? "[[override]]" line)    ; Dependencies of dependencies
        (let-values (((packages dependencies)
                      (parse-dependencies->packages+dependencies port #f
                                                                 packages
                                                                 dependencies)))
          (loop port packages dependencies)))
       (else (loop port packages dependencies)))))
  (loop port packages dependencies))

(define (gopkg-dep->packages+dependencies path)
  "Open toml file if exist and parse it and return packages sexp and
dependencies list. Or return two empty list if file not found."
  (if (file-exists? path)
      (let ((port (open-input-file path)))
        (let-values (((packages dependencies)
                      (parse-toml->packages+dependencies port
                                                         '() '())))
          (close-port port)
          (values packages dependencies)))
      (values '() '())))

;;
;; Entry point.
;;

(define (gopkg->guix-package url branch)
  "Create package for git repository dans branch verison and all dependencies
sexp packages with Gopkg.toml file."
  (let ((name (url->package-name (cut-url url)))
        (version "0.0.0")
        (revision "0"))
    (call-with-temporary-directory
     (lambda (directory)
       (git-fetch url branch directory #:recursive? #f)

       (let-values (((packages dependencies)
                     (gopkg-dep->packages+dependencies
                      (string-append directory
                                     "/Gopkg.toml"))))
         (let-values (((packages dependencies)
                       (create-package->packages+dependencies packages dependencies
                                                              (cut-url url) version
                                                              directory
                                                              revision branch
                                                              #f #f)))
           (values packages)))))))
