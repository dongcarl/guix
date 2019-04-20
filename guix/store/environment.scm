;;; GNU Guix --- Functional package management for GNU
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

;;; Code for setting up environments, especially build environments.  Builds
;;; on top of (gnu build linux-container).

(define-module (guix store environment)
  #:use-module (guix records)
  #:use-module (guix config)
  #:use-module (gnu build linux-container)
  #:use-module (gnu system file-systems)
  #:use-module ((guix build utils) #:select (delete-file-recursively
                                             mkdir-p
                                             copy-recursively))
  #:use-module (guix derivations)
  #:use-module (guix store)
  #:use-module (guix build syscalls)
  #:use-module (guix store database)
  #:use-module (guix store files)
  #:use-module (gcrypt hash)
  #:use-module (guix base32)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-98)

  #:export (<environment>
            environment
            environment-namespaces
            environment-variables
            environment-temp-dirs
            environment-filesystems
            environment-new-session?
            environment-new-pgroup?
            environment-setup-i/o-thunk
            environment-preserved-fds
            environment-chroot
            environment-personality
            environment-user
            environment-group
            environment-hostname
            environment-domainname
            build-environment-vars
            delete-environment
            run-in-environment
            bind-mount
            standard-i/o-setup
            %standard-preserved-fds
            nonchroot-build-environment
            chroot-build-environment
            builtin-builder-environment
            run-standard
            run-standard-build
            wait-for-build))

(define %standard-preserved-fds '(0 1 2))

(define-record-type* <environment> environment
  ;; The defaults are set to be as close to the "current environment" as
  ;; possible.
  make-environment
  environment?
  (namespaces environment-namespaces (default '())) ; list of symbols
  ; list of (key . val) pairs
  (variables environment-variables (default (get-environment-variables)))
  ; list of (symbol . filename) pairs.
  (temp-dirs environment-temp-dirs (default '()))
  ;; list of <file-system> objects. Only used when MNT is in NAMESPACES.
  (filesystems environment-filesystems (default '()))
  ; boolean (implies NEW-PGROUP?)
  (new-session? environment-new-session? (default #f))
  (new-pgroup? environment-new-pgroup? (default #f)) ; boolean
  (setup-i/o environment-setup-i/o-thunk) ; a thunk or #f
  ; #f or list of integers (in case of #f, all are preserved)
  (preserved-fds environment-preserved-fds (default #f))
  ;; either the chroot directory or #f, must not be #f if MNT is in
  ;; NAMESPACES! Will be recursively deleted when the environment is
  ;; destroyed. Ignored if MNT is not in NAMESPACES.
  (chroot environment-chroot (default #f))
  (initial-directory environment-initial-directory (default #f)) ; string or #f
  (personality environment-personality (default #f)) ; integer or #f
  ;; These are currently naively handled in the case of user namespaces.
  (user environment-user (default #f))             ; integer or #f
  (group environment-group (default #f))           ; integer or #f
  (hostname environment-hostname (default #f))         ; string or #f
  (domainname environment-domainname (default #f)))    ; string or #f

(define (delete-environment env)
  "Delete all temporary directories used in ENV."
  (for-each (match-lambda
              ((id . filename)
               (delete-file-recursively filename)))
            (environment-temp-dirs env))
  (when (environment-chroot env)
    (delete-file-recursively (environment-chroot env))))

(define (format-file file-name . args)
  (call-with-output-file file-name
    (lambda (port)
      (apply simple-format port args))))

(define* (mkdir-p* dir #:optional permissions)
  (mkdir-p dir)
  (when permissions
    (chmod dir permissions)))

(define (add-core-files environment fixed-output?)
  "Populate container with miscellaneous files and directories that shouldn't
be bind-mounted."
  (let ((uid (environment-user environment))
        (gid (environment-group environment)))
    (mkdir-p* "/tmp" #o1777)
    (mkdir-p* "/etc")

    (unless (or (file-exists? "/etc/passwd")
                (file-exists? "/etc/group"))
      (format-file "/etc/passwd"
                   (string-append "nixbld:x:~a:~a:Nix build user:/:/noshell~%"
                                  "nobody:x:65534:65534:Nobody:/:/noshell~%")
                   uid gid)
      (format-file "/etc/group" "nixbld:!:~a:~%" gid))

    (unless (or fixed-output? (file-exists? "/etc/hosts"))
      (format-file "/etc/hosts" "127.0.0.1 localhost~%"))
    (when (file-exists? "/dev/pts/ptmx")
      (symlink "/dev/pts/ptmx" "/dev/ptmx")
      (chmod "/dev/pts/ptmx" #o0666))))

(define (run-in-environment env thunk . i/o-args)
  "Run THUNK in ENV with I/O-ARGS passed to the SETUP-I/O procedure of
ENV.  Return the pid of the process THUNK is run in."
  (match env
    (($ <environment> namespaces variables temp-dirs
                      filesystems new-session? new-pgroup? setup-i/o
                      preserved-fds chroot current-directory new-personality
                      user group hostname domainname)
     (when (and new-session? (not new-pgroup?))
       (throw 'invalid-environment "NEW-SESSION? implies NEW-PGROUP?."))
     (let ((fixed-output? (not (memq 'net namespaces))))
       (run-container chroot filesystems namespaces (and user (1+ user))
                      (lambda ()
                        (when hostname (sethostname hostname))
                        (when domainname (setdomainname domainname))
                        ;; setsid / setpgrp as necessary
                        (if new-session?
                            (setsid)
                            (when new-pgroup?
                              (setpgid 0 0)))
                        (when chroot
                          (add-core-files env fixed-output?))
                        ;; set environment variables
                        (when variables
                          (environ (map (match-lambda
                                          ((key . val)
                                           (string-append key "=" val)))
                                        variables)))
                        (when setup-i/o (apply setup-i/o i/o-args))
                        ;; set UID and GID
                        (when current-directory (chdir current-directory))
                        (when group (setgid group))
                        (when user (setuid user))
                        ;; Close unpreserved fds
                        (when preserved-fds
                          (let close-next ((n 0))
                            (when (< n 20) ;; XXX: don't hardcode.
                              (unless (memq n preserved-fds)
                                (false-if-exception (close-fdes n)))
                              (close-next (1+ n)))))

                        ;; enact personality
                        (when new-personality (personality new-personality))
                        (thunk)))))))

(define (bind-mount src dest)
  "Return a <file-system> denoting the bind-mounting of SRC to DEST. Note that
if this is part of a chroot <environment>, DEST will be the name *inside of*
the chroot, i.e.

(bind-mount \"/foo/x\" \"/bar/x\")

in an environment with chroot \"/chrootdir\" will bind-mount \"/foo/x\" to
\"/chrootdir/bar/x\"."
  (file-system
    (device src)
    (mount-point dest)
    (type "none")
    (flags '(bind-mount))
    (check? #f)))

(define input->mount
  (match-lambda
    ((source . dest)
     (bind-mount source dest))
    (source
     (bind-mount source source))))

(define (default-files drv)
  "Return a list of the files to be bind-mounted that aren't store items or
already added by call-with-container."
  `(,@(if (file-exists? "/dev/kvm")
          '("/dev/kvm")
          '())
    ,@(if (fixed-output-derivation? drv)
          '("/etc/resolv.conf"
            "/etc/nsswitch.conf"
            "/etc/services"
            "/etc/hosts")
          '())))

(define (build-environment-vars drv build-dir)
  "Return an alist of environment variable / value pairs for every environment
variable that should be set during the build execution."
  (let ((leaked-vars (and
                      (fixed-output-derivation? drv)
                      (let ((leak-string
                             (assoc-ref (derivation-builder-environment-vars drv)
                                        "impureEnvVars")))
                        (and leak-string
                             (string-tokenize leak-string
                                              (char-set-complement
                                               (char-set #\space))))))))
    (append `(("PATH"             .  "/path-not-set")
              ("HOME"             .  "/homeless-shelter")
              ("NIX_STORE"        .  ,%store-directory)
              ;; XXX: make this configurable
              ("NIX_BUILD_CORES"  .  "0")
              ("NIX_BUILD_TOP"    .  ,build-dir)
              ("TMPDIR"           .  ,build-dir)
              ("TEMPDIR"          .  ,build-dir)
              ("TMP"              .  ,build-dir)
              ("TEMP"             .  ,build-dir)
              ("PWD"              .  ,build-dir))
            (if (fixed-output-derivation? drv)
                '(("NIX_OUTPUT_CHECKED" . "1"))
                '())
            (if leaked-vars
                ;; leaked vars might be #f
                (filter cdr
                        (map (lambda (leaked-var)
                               (cons leaked-var (getenv leaked-var)))
                             leaked-vars))
                '())
            (derivation-builder-environment-vars drv))))

(define* (temp-directory name #:optional permissions user group
                         #:key (tmpdir %temp-directory))
  "Create a temporary directory under TMPDIR with permissions PERMISSIONS if
specified, otherwise default permissions as specified by umask, and belonging
to user USER and group GROUP (defaulting to current user if not specified or
#f).  Return the full filename of the form <tmpdir>/<name>-<number>."
  (let try-again ((attempt-number 0))
    (catch 'system-error
      (lambda ()
        (let ((attempt-name (string-append tmpdir "/" name "-"
                                           (number->string
                                            attempt-number 10))))
          (mkdir attempt-name permissions)
          (when permissions
            (chmod attempt-name permissions))
          ;; -1 means "unchanged"
          (chown attempt-name (or user -1) (or group -1))
          attempt-name))
      (lambda args
        (if (= (system-error-errno args) EEXIST)
            (try-again (+ attempt-number 1))
            (apply throw args))))))

(define (path-already-assigned? path paths)
  "Determines whether something is already going to be bind-mounted to PATH
based on what is in PATHS, which should be a list of paths or path pairs."
  (find (match-lambda
          ((source . target)
           (string= target path))
          (target
           (string= target path)))
        paths))


(define (special-filesystems input-paths)
  "Return whatever new filesystems need to be created in the container, which
depends on whether they're already set to be bind-mounted.  INPUT-PATHS must
be a list of paths or pairs of paths."
  ;; procfs is already taken care of by call-with-container
  `(,@(if (file-exists? "/dev/shm")
          (list (file-system
                  (device "none")
                  (mount-point "/dev/shm")
                  (type "tmpfs")
                  (check? #f)))
          '())

    ;; Indicates CONFIG_DEVPTS_MULTIPLE_INSTANCES=y in the kernel.
    ,@(if (and (file-exists? "/dev/pts/ptmx")
               ;; This check is fishy
               (not (path-already-assigned? "/dev/ptmx"
                                            input-paths))
               (not (path-already-assigned? "/dev/pts"
                                            input-paths)))
          (list (file-system
                  (device "none")
                  (mount-point "/dev/pts")
                  (type "devpts")
                  (options "newinstance,mode=0620")
                  (check? #f)))
          '())))

(define (standard-i/o-setup output-port)
  "Redirect output and error streams to OUTPUT-FD, get input from /dev/null."
  (define output-fd (port->fdes output-port))
  (define stdout (fdopen 1 "w"))
  ;; Useful in case an error happens between here and an exec and it needs to
  ;; get reported.
  (set-current-output-port stdout)
  (set-current-error-port stdout)
  (dup2 output-fd 1)
  (dup2 output-fd 2)
  (call-with-input-file "/dev/null"
    (lambda (null-port)
      (dup2 (port->fdes null-port) 0))))



(define (derivation-tempname drv)
  (string-append "guix-build-"
                 (store-path-package-name (derivation-file-name drv))))

;; We might want to add to this sometime.
(define %default-chroot-dirs
  '())

(define (default-personality drv)
  (let ((current-personality (personality #xffffffff)))
    (logior current-personality ADDR_NO_RANDOMIZE
            (match (cons %system (derivation-system drv))
              ((or ("x86_64-linux" . "i686-linux")
                   ("aarch64-linux" . "armhf-linux"))
               PER_LINUX32)
              (_ 0))
            (match (cons (derivation-system drv) %impersonate-linux-2.6?)
              (((or "x86_64-linux" "i686-linux") . #t)
               UNAME26)
              (_ 0)))))

(define* (nonchroot-build-environment drv #:key gid uid)
  "Create and return an <environment> for building DRV outside of a chroot, as
well as the store inputs the build requires."
  (let* ((fixed-output? (fixed-output-derivation? drv))
         (tempname (derivation-tempname drv))
         (build-directory (temp-directory tempname #o0700)))
    (values
     (environment
      (temp-dirs `((build-directory . ,build-directory)))
      (initial-directory build-directory)
      (new-session? #t)
      (new-pgroup? #t)
      (variables (build-environment-vars drv build-directory))
      (preserved-fds %standard-preserved-fds)
      (setup-i/o standard-i/o-setup)
      (personality (default-personality drv))
      (user uid)
      (group gid))
     (all-transitive-inputs drv))))


(define* (builtin-builder-environment drv #:key gid uid)
  "Create and return an <environment> for builtin builders, as well as the
store inputs the build requires."
  ;; It's just the same as non-chroot-build-environment, but without any
  ;; environment variables being changed.
  (let*-values (((env inputs) (nonchroot-build-environment drv
                                                           #:gid gid
                                                           #:uid uid)))
    (values
     (environment (inherit env)
                  (variables (get-environment-variables)))
     inputs)))

(define* (chroot-build-environment drv #:key gid uid
                                   (extra-chroot-dirs '())
                                   build-chroot-dirs )
  "Create an <environment> for building DRV with standard in-chroot
settings (as used by nix daemon).  Return said environment as well as the
store paths that are included in it (useful for reference scanning)."
  (let* ((tempname (derivation-tempname drv))
         (store-directory (temp-directory (string-append tempname ".store")
                                          #o1775 0 gid))
         (build-directory (temp-directory tempname #o0700 uid gid))
         (inside-build-dir (string-append %temp-directory "/" tempname "-0"))
         (fixed-output? (fixed-output-derivation? drv))
         (store-inputs (all-transitive-inputs drv))
         (input-paths (append store-inputs
                              (default-files drv)
                              (or build-chroot-dirs
                                  %default-chroot-dirs)
                              extra-chroot-dirs)))
    (values
     (environment
      (namespaces `(mnt pid ipc uts ,@(if fixed-output? '() '(net))))
      (filesystems
       (cons* (bind-mount build-directory inside-build-dir)
              (bind-mount store-directory %store-directory)
              (append (special-filesystems input-paths)
                      (map input->mount input-paths))))
      (temp-dirs `((store-directory . ,store-directory)
                   (build-directory . ,build-directory)))
      (initial-directory inside-build-dir)
      (new-session? #t)
      (new-pgroup? #t)
      (setup-i/o (lambda (output-fd)
                   (unless fixed-output?
                     (initialize-loopback))
                   (standard-i/o-setup output-fd)))
      (variables (build-environment-vars drv inside-build-dir))
      (preserved-fds %standard-preserved-fds)
      (chroot (temp-directory (string-append tempname ".chroot") #o750 0 gid))
      (user uid)
      (group gid)
      (personality (default-personality drv))
      (hostname "localhost")
      (domainname "(none)"))
     store-inputs)))

(define (redirected-path drv output)
  (let* ((original (assoc-ref (derivation-outputs drv) output))
         (hash
          (bytevector->nix-base32-string
           (compressed-hash (sha256 (string-append "rewrite:"
                                                   (derivation-file-name drv)
                                                   ":"
                                                   original))
                            20))))
    (string-append (%store-prefix) "/" hash "-"
                   (store-path-package-name original))))

(define (redirect-outputs env drv output-names)
  "Create a new <environment> based on ENV but modified so that for each
output-name in OUTPUT-NAMES, the environment variable corresponding to that
output is set to a newly-generated output path."
  (environment (inherit env)
   (variables (append (map (lambda (output)
                             (cons output (redirected-path drv output)))
                           output-names)
                      (remove (lambda (var)
                                (member (car var) output-names))
                              (environment-variables env))))))

(define (run-standard environment thunk)
  "Run THUNK in ENVIRONMENT.  Return the PID it is being run in and the read
end of the pipe its i/o has been set up with."
  (match (pipe)
    ((read . write)
     (let ((pid (run-in-environment environment
                                    (lambda ()
                                      (catch #t
                                        (lambda ()
                                          (thunk)
                                          (primitive-exit 0))
                                        (lambda args
                                          (format #t "Error: ~A~%" args)
                                          (primitive-exit 1))))
                                    write)))
       (close-fdes (port->fdes write))
       (values pid read)))))

(define (run-standard-build drv environment)
  "Run the builder of DRV in ENVIRONMENT.  Return the PID it is being run in
and the read end of the pipe its i/o has been set up with."
  (run-standard environment
                (lambda ()
                  (let ((prog (derivation-builder drv))
                        (args (derivation-builder-arguments drv)))
                    (apply execl prog prog args)))))

(define (dump-port port)
  (unless (port-eof? port)
    (put-bytevector (current-output-port)
                    (get-bytevector-some port))
    (force-output (current-output-port))
    (dump-port port)))

(define (wait-for-build pid read-port)
  "Dump all input from READ-PORT to (current-output-port), then wait for PID
to terminate."
  (dump-port read-port)
  (close-fdes (port->fdes read-port))
  ;; Should we wait specifically for PID to die, or just for any state change?
  (cdr (waitpid pid)))



