;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Andy Wingo <wingo@pobox.com>
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

(define-module (guix git)
  #:use-module (guix utils)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (&git-condition
            git-condition?
            git-condition-argv
            git-condition-output
            git-condition-status

            false-if-git-error

            git-check-ref-format
            git-rev-parse
            git-config
            git-describe
            git-fetch
            git-push
            git-clone
            git-reset
            git-add
            git-commit))

;;; Commentary:
;;;
;;; A simple collection of Scheme wrappers for Git functionality.
;;;
;;; Code:

(define-condition-type &git-condition &condition git-condition?
  (argv git-condition-argv)
  (output git-condition-output)
  (status git-condition-status))

(define-syntax-rule (false-if-git-error body0 body ...)
  (guard (c ((git-condition? c) #f))
    body0 body ...))

(define (shell:quote str)
  (with-output-to-string
    (lambda ()
      (display #\')
      (string-for-each (lambda (ch)
                         (if (eqv? ch #\')
                             (begin (display #\\) (display #\'))
                             (display ch)))
                       str)
      (display #\'))))

(define (run env input-file args)
  (define (prepend-env args)
    (if (null? env)
        args
        (cons "env" (append env args))))
  (define (redirect-input args)
    (if input-file
        (list "sh" "-c"
              (string-append (string-join (map shell:quote args) " ")
                             "<" input-file))
        args))
  (let* ((real-args (redirect-input (prepend-env args)))
         (pipe (apply open-pipe* OPEN_READ real-args))
         (output (read-string pipe))
         (ret (close-pipe pipe)))
    (case (status:exit-val ret)
      ((0) output)
      (else (raise (condition (&git-condition
                               (argv real-args)
                               (output output)
                               (status ret))))))))

(define* (git* args #:key (input #f) (env '()))
  (if input
      (call-with-temporary-output-file
       (lambda (file-name file-port)
         (display input file-port)
         (close-port file-port)
         (run env file-name (cons* "git" args))))
      (run env #f (cons* "git" args))))

(define (git . args)
  (git* args))

(define* (git-check-ref-format str #:key allow-onelevel?)
  "Raise an exception if @var{str} is not a valid Git ref."
  (when (string-prefix? "-" str)
    (error "bad ref" str))
  (git "check-ref-format"
       (if allow-onelevel? "--allow-onelevel" "--no-allow-onelevel")
       str))

(define (git-rev-parse rev)
  "Parse the string @var{rev} and return a Git commit hash, as a string."
  (string-trim-both (git "rev-parse" rev)))

(define (git-config key)
  "Return the configuration value for @var{key}, as a string."
  (string-trim-both (git "config" key)))

(define* (git-describe #:optional (ref "HEAD"))
  "Run @command{git describe} on the given @var{ref}, defaulting to
@code{HEAD}, and return the resulting string."
  (string-trim-both (git "describe")))

(define (git-fetch)
  "Run @command{git fetch} in the current working directory."
  (git "fetch"))

(define (git-push)
  "Run @command{git push} in the current working directory."
  (git "push"))

(define (git-clone repo dir)
  "Check out @var{repo} into @var{dir}."
  (git "clone" "--" repo dir))

(define* (git-reset #:key (ref "HEAD") (mode 'hard))
  ;; Can't let the ref be mistaken for a command-line argument.
  "Reset the current working directory to @var{ref}.  Available values for
@var{mode} are the symbols @code{hard}, @code{soft}, and @code{mixed}."
  (when (string-prefix? "-" ref)
    (error "bad ref" ref))
  (git "reset"
       (case mode
         ((hard) "--hard")
         ((mixed) "--mixed")
         ((soft) "--soft")
         (else (error "unknown mode" mode)))
       ref))

(define (git-add file)
  "Add @var{file} to the index in the current working directory."
  (git "add" "--" file))

(define* (git-commit #:key message author-name author-email)
  "Commit the changes in the current working directory, with the message
@var{message}.  The commit will be attributed to the author with the name and
email address @var{author-name} and @var{author-email}, respectively."
  (git* (list "commit" (string-append "--message=" message))
        #:env (list (string-append "GIT_COMMITTER_NAME=" author-name)
                    (string-append "GIT_COMMITTER_EMAIL=" author-email)
                    (string-append "GIT_AUTHOR_NAME=" author-name)
                    (string-append "GIT_AUTHOR_EMAIL=" author-email))))
