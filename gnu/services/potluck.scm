;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Andy Wingo <wingo@igalia.com>
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

(define-module (gnu services potluck)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (potluck-configuration
            potluck-configuration?
            potluck-service-type))

;;; Commentary:
;;;
;;; Guix potluck services.
;;;
;;; Code:

;; FIXME: Replace with guix version that has potluck.
(define guix/potluck guix)

(define-record-type* <potluck-configuration> potluck-configuration
  make-potluck-configuration
  potluck-configuration?
  (package       potluck-configuration-package ;<package>
                 (default guix/potluck))
  (scratch       potluck-configuration-scratch
                 (default "/var/cache/potluck/scratch"))
  (source        potluck-configuration-source
                 (default "/var/cache/potluck/source"))
  (source-repo   potluck-configuration-source-repo
                 (default "/srv/git/source.git"))
  (target        potluck-configuration-target
                 (default "/var/cache/potluck/target"))
  (target-repo   potluck-configuration-target-repo
                 (default "/srv/git/target.git"))
  (user          potluck-configuration-user
                 (default "potluck"))
  (group         potluck-configuration-group
                 (default "potluck")))

(define potluck-accounts
  (match-lambda
    (($ <potluck-configuration>
        package scratch source source-repo target target-repo user group)
     (filter identity
             (list
              (and (equal? group "potluck")
                   (user-group
                    (name "potluck")
                    (system? #t)))
              (and (equal? user "potluck")
                   (user-account
                    (name "potluck")
                    (group group)
                    (system? #t)
                    (comment "Potluck Service")
                    (home-directory "/var/empty")
                    (shell (file-append shadow "/sbin/nologin")))))))))

(define potluck-activation-service
  (match-lambda
    (($ <potluck-configuration>
        package scratch source source-repo target target-repo user group)
     (with-imported-modules '((guix build utils))
       #~(begin
	   (use-modules (guix build utils) (ice-9 ftw))
	   (define (chown-r dir user group)
             (let ((uid (passwd:uid (getpwnam user)))
		   (gid (group:gid (getgrnam group))))
	       (for-each (lambda (f) (chown f uid gid))
			 (find-files dir #:directories? #t))))
	   (define (ensure-git-repo dir)
	     (unless (file-exists? dir)
	       (mkdir-p dir)
	       (unless (zero? (system* (string-append #$git "/bin/git")
				       "init" "--bare" dir))
		 (error "failed to create repository" dir))
	       (chown-r dir #$user #$group)))
	   (define (ensure-checkout repo dir)
	     (unless (file-exists? dir)
	       (mkdir-p (dirname dir))
	       (unless (zero? (system* (string-append #$git "/bin/git")
				       "clone" repo dir))
	         (error "failed to check out repository" repo dir))
	       (chown-r dir #$user #$group)))
	   (mkdir-p #$scratch)
	   (chown-r #$scratch #$user #$group)
	   (ensure-git-repo #$source-repo)
	   (ensure-git-repo #$target-repo)
	   (ensure-checkout #$source-repo #$source)
	   (ensure-checkout #$target-repo #$target))))))

(define potluck-shepherd-service
  (match-lambda
    (($ <potluck-configuration>
        package scratch source source-repo target target-repo user group)
     (list (shepherd-service
            (provision '(potluck))
            (documentation "Run the potluck daemon.")
            (requirement '(networking))

            (start #~(make-forkexec-constructor
                      '(#$(file-append package "/bin/guix") "host-channel"
                        #$(format #f "--scratch=~a" scratch)
			#$(format #f "--source=~a" source)
			#$(format #f "--target=~a" target))
		      #:user #$user #:group #$group))
            (stop #~(make-kill-destructor)))))))

(define potluck-service-type
  (service-type (name 'potluck)
                (extensions
                 (list (service-extension activation-service-type
                                          potluck-activation-service)
		       (service-extension shepherd-root-service-type
                                          potluck-shepherd-service)
		       (service-extension account-service-type
					  potluck-accounts)))
                (default-value (potluck-configuration))))
