;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages commencement)
  #:use-module ((guix licenses)
                #:select (gpl3+ lgpl2.0+ public-domain))
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages c)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages code)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages mes)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix memoization)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex))

;;; Commentary:
;;;
;;; This is the commencement, this is where things start.  Before the
;;; commencement, of course, there's the 'bootstrap' module, which provides us
;;; with the initial binaries.  This module uses those bootstrap binaries to
;;; actually build up the whole tool chain that make up the implicit inputs of
;;; 'gnu-build-system'.
;;;
;;; To avoid circular dependencies, this module should not be imported
;;; directly from anywhere.
;;;
;;; Below, we frequently use "inherit" to create modified packages.  The
;;; reason why we use "inherit" instead of "package/inherit" is because we do
;;; not want these commencement packages to inherit grafts.  By definition,
;;; these packages are not depended on at run time by any of the packages we
;;; use.  Thus it does not make sense to inherit grafts.  Furthermore, those
;;; grafts would often lead to extra overhead for users who would end up
;;; downloading those "-boot0" packages just to build package replacements
;;; that are in fact not going to be used.
;;;
;;; Code:

(define %bootstrap-gash+coreutils
  (package-with-bootstrap-guile
   (package
     (inherit guile-gash)
     (name "bootstrap-gash+coreutils")
     (source #f)
     (native-inputs `(("gash" ,%bootstrap-gash)))
     (inputs '())
     (propagated-inputs '())
     (outputs '("out"))
     (build-system trivial-build-system)
     (arguments
      `(#:guile ,%bootstrap-guile
        #:modules ((guix build utils))
        #:builder (begin
                    (use-modules (guix build utils))
                    (let* ((gash (assoc-ref %build-inputs "gash"))
                           (gash-bin (string-append gash "/bin"))
                           (gash-lib (string-append gash "/lib"))
                           (gash-libexec (string-append gash "/libexec/gash"))
                           (out (assoc-ref %outputs "out"))
                           (bin (string-append out "/bin"))
                           (lib (string-append out "/lib")))

                      (define (rewire-script script)
                        (substitute* script
                          ((gash) out)))

                      (mkdir-p bin)
                      (for-each (lambda (file) (install-file file bin))
                                (append (find-files gash-bin)
                                        (find-files gash-libexec)))
                      (copy-recursively gash-lib lib)
                      (for-each rewire-script (find-files bin))
                      #t)))))))

(define mes-boot0
  (package
    (inherit mes)
    (name "mes-boot0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://lilypond.org/janneke/mes/"
                    "mes-0.19-46-g4b921d1b.tar"))
              (sha256
               (base32
                "1ar217kzl59cx9bk0jw9xw0ql3jsrmps3f0qqbqidr9k0vaa7jmp"))))
    (native-inputs '())
    (propagated-inputs '())))

(define nyacc-boot
  (package
    (inherit nyacc)
    (name "nyacc-boot")
    (version "0.86.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://lilypond.org/janneke/mes/"
                    "nyacc-" version ".tar"))
              (sha256
               (base32
                "1912yx1vr9l97gr0wddm9j7f0p3wvfc410dmvdmg11jvjssvmdjp"))))))

(define-public mes-boot
  (package-with-bootstrap-guile
   (package
     (inherit mes-boot0)
     (name "mes-boot")
     (inputs '())
     (propagated-inputs '())
     (native-inputs
      `(("mescc-tools" ,%bootstrap-mescc-tools)
        ("nyacc-source" ,(package-source nyacc-boot))

        ("guile" ,%bootstrap-guile)
        ("gash" , %bootstrap-gash+coreutils)
        ("bootstrap-mes" ,%bootstrap-mes)))
     (arguments
      `(#:implicit-inputs? #f
        #:tests? #f
        #:guile ,%bootstrap-guile
        #:strip-binaries? #f   ; binutil's strip b0rkes MesCC/M1/hex2 binaries
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'unpack-seeds
            (lambda _
              (let ((nyacc-source (assoc-ref %build-inputs "nyacc-source"))
                    (bootstrap-mes (assoc-ref %build-inputs "bootstrap-mes")))
                (with-directory-excursion ".."
                  (and
                   ;; FIXME: tar --strip broken?
                   ;;(mkdir-p "nyacc-source")
                   ;;(invoke "tar" "--strip=1" "-C" "nyacc-source" "-xvf" nyacc-source)
                   (invoke "tar" "-xvf" nyacc-source)
                   (rename-file "nyacc-0.86.0" "nyacc-source")
                   (symlink (string-append bootstrap-mes "/share/mes/lib") "mes-seed")
                   #t)))))
          (add-before 'configure 'remove-bash
            (lambda _
              (substitute* "build-aux/check.sh.in"
                (("(./pre-inst-env bash .*check-boot.sh)" all) (string-append "# " all))
                (("(./pre-inst-env sh .*check-mescc.sh)" all) (string-append "# " all)))
              #t))
          (replace 'configure
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref %outputs "out")))
                (setenv "GUILE" "mes")
                (setenv "GUILE_EFFECTIVE_VERSION" "2.2")
                (setenv "GUILE_LOAD_PATH" "nyacc")
                (symlink (string-append "../nyacc-source/module") "nyacc")
                (invoke "bash" "configure.sh"
                        (string-append "--prefix=" out)))))
          (replace 'build
            (lambda _
              ;; show some progress
              (substitute* "bootstrap.sh"
                (("guile build-aux/mes-snarf.scm (.*)" all cmd)
                 (string-append "echo guile -e '(mes-snarf)' build-aux/mes-snarf.scm " cmd "\n"
                                "guile -e '(mes-snarf)' build-aux/mes-snarf.scm " cmd "\n"
                                "ls -l src\n"))
                (("guile (.*)" all cmd)
                 (string-append "echo guile " cmd "\n"
                                "guile " cmd "\n"
                                "ls -l lib lib/x86-mes src\n")))
              (let ((mes (assoc-ref %build-inputs "bootstrap-mes")))
                (setenv "MES_PREFIX" (string-append mes "/share/mes"))
                (setenv "MES_ARENA" "100000000")
                (setenv "MES_MAX_ARENA" "100000000")
                (setenv "MES_STACK" "10000000")
                (invoke "sh" "bootstrap.sh"))))
          (replace 'check
            (lambda _
              (and
               (setenv "DIFF" "sh scripts/diff.scm")
               ;; fail fast tests
               ;; (invoke "sh" "-x" "build-aux/test.sh" "scaffold/tests/t")
               ;; (invoke "sh" "-x" "build-aux/test.sh" "scaffold/tests/63-struct-cell")
               (setenv "V" "0")
               (setenv "MES_PREFIX" "mes")
               (setenv "MES_DEBUG" "1")
               (invoke "sh" "check.sh"))))
          (replace 'install
            (lambda _
              (substitute* "install.sh"
                ((" --exclude=[^ ]*") ""))

              ;; show some progress
              (substitute* "install.sh"
                ((" -xf") " -xvf"))

              ;; show some progress
              (substitute* "install.sh"
                (("^( *)((cp|mkdir|tar) [^']*[^\\])\n" all space cmd)
                 (string-append space "echo '" cmd "'\n"
                                space cmd "\n")))
              (format #t "INVOKE\n")
              (invoke "sh" "-x" "install.sh"))))))
     (native-search-paths
      ;; Use the language-specific variables rather than 'CPATH' because they
      ;; are equivalent to '-isystem' whereas 'CPATH' is equivalent to '-I'.
      ;; The intent is to allow headers that are in the search path to be
      ;; treated as "system headers" (headers exempt from warnings) just like
      ;; the typical /usr/include headers on an FHS system.
      (list (search-path-specification
             (variable "C_INCLUDE_PATH")
             (files '("share/mes/include")))
            (search-path-specification
             (variable "LIBRARY_PATH")
             (files '("share/mes/lib"))))))))

(define-public tcc-boot0
  ;; Pristine tcc cannot be built by MesCC, we are keeping a delta of 11
  ;; patches.  In a very early and rough form they were presented to the
  ;; TinyCC developers, who at the time showed no interest in supporting the
  ;; bootstrappable effort; we will try again later.  These patches have been
  ;; ported to 0.9.27, alas the resulting tcc is buggy.  Once MesCC is more
  ;; mature, this package should use the 0.9.27 sources (or later).
  (let ((version "0.9.26")
        (revision "6")
        (commit "337acca2b8666375f97c0522ebc318bdce8bfe81"))
    (package-with-bootstrap-guile
     (package
       (inherit tcc)
       (name "tcc-boot0")
       (version (string-append version "-" revision "." (string-take commit 7)))
       ;; (source (origin
       ;;           (method url-fetch)
       ;;           (uri (string-append "https://gitlab.com/janneke/tinycc"
       ;;                               "/-/archive/" commit
       ;;                               "/tinycc-" commit ".tar.gz"))
       ;;           (sha256
       ;;            (base32
       ;;             "1hmzn1pq0x22ppd80hyrn5qzqq94mxd0ychzj6vrr2vnj2frjv5b"))))
       (source (origin
                 (method url-fetch)
                 (uri (string-append
                       "http://lilypond.org/janneke/mes/"
                       "tinycc-" commit ".tar"))
                 (sha256
                  (base32
                   "1336cbnpq6g8gyhznrn8hlicn63g2xn7znzdmm298jiyrxca0gmd"))))
       (build-system gnu-build-system)
       (supported-systems '("i686-linux" "x86_64-linux"))
       (inputs '())
       (propagated-inputs '())
       (native-inputs
        `(("mes" ,mes-boot)
          ("mescc-tools" ,%bootstrap-mescc-tools)
          ("nyacc-source" ,(package-source nyacc-boot))

          ("guile" ,%bootstrap-guile)
          ("gash" , %bootstrap-gash+coreutils)
          ("bootstrap-mes" ,%bootstrap-mes)))
       (arguments
        `(#:implicit-inputs? #f
          #:guile ,%bootstrap-guile
          ;;/gnu/store/ang6n8d9yz86zfiy0arsydh6pp9fwk97-tcc-boot0-0.9.26-6.337acca/lib/tests/string/70-strchr: error: offset + size of segment 2 (type 1) exceeds total size
          #:validate-runpath? #f ; no dynamic executables
          #:strip-binaries? #f   ; no strip yet
          ;;#:system "i686-linux"
          #:phases
          (modify-phases %standard-phases
            (add-after 'unpack 'unpack-seeds
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((nyacc-source (assoc-ref %build-inputs "nyacc-source"))
                       (bootstrap-mes (assoc-ref %build-inputs "bootstrap-mes")))
                  (with-directory-excursion ".."
                    (and
                     ;; FIXME: tar --strip broken?
                     ;; (mkdir-p "nyacc-source")
                     ;; (invoke "tar" "--strip=1" "-C" "nyacc-source"
                     ;;         "-xvf" nyacc-source)
                     (invoke "tar" "-xvf" nyacc-source)
                     (rename-file "nyacc-0.86.0" "nyacc-source")
                     (symlink (string-append bootstrap-mes "/share/mes/lib") "mes-seed")
                     #t)))))
            (replace 'configure
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref %outputs "out"))
                       (dir (with-directory-excursion ".." (getcwd)))
                       (guile (assoc-ref %build-inputs "guile"))
                       (gash (assoc-ref %build-inputs "gash"))
                       (mes (assoc-ref %build-inputs "mes"))
                       (mescc-tools (assoc-ref %build-inputs "mescc-tools"))
                       (libc (assoc-ref %build-inputs "libc"))
                       (interpreter (if libc
                                        ;; also for x86_64-linux, we are still on i686-linux
                                        (string-append libc ,(glibc-dynamic-linker "i686-linux"))
                                        (string-append mes "/lib/mes-loader"))))
                  (setenv "PATH" (string-append
                                  gash "/bin"
                                  ":" mes "/bin"
                                  ":" mescc-tools "/bin"))
                  (format (current-error-port) "PATH=~s\n" (getenv "PATH"))

                  (setenv "PREFIX" out)
                  (symlink (string-append mes "/share/mes") "mes")
                  (symlink (string-append "../nyacc-source/module") "nyacc")
                  (setenv "MES_PREFIX" "mes")
                  (setenv "MES_ARENA" "100000000")
                  (setenv "MES_MAX_ARENA" "100000000")
                  (setenv "MES_STACK" "10000000")
                  (setenv "MES" "mes")
                  (setenv "GUILE_LOAD_PATH" "nyacc")

                  (if #f ;; needs at least Gash' 100-test-false.sh to pass.
                      (invoke "sh" "-x" "configure"
                              "--prefix=$PREFIX"
                              (string-append "--elfinterp=" interpreter)
                              "--crtprefix=."
                              "--tccdir=.")

                      (with-output-to-file "config.h"
                        (lambda _
                          (display (string-append "
#ifndef CONFIG_TCCDIR
#define CONFIG_TCCDIR \"" out "/lib/tcc\"
#endif
#define GCC_MAJOR 0
#define GCC_MINOR 0
#define TCC_VERSION \"0.9.26\"
")))))
                  )))
            (replace 'build
              (lambda _
                (substitute* "bootstrap.sh"
                  (("^( *)cmp" all indent) (string-append indent "#cmp")))

              (let ((bootstrap.sh (open-file "bootstrap.sh" "a")))
                (display "
./tcc -c -I $MES_PREFIX/include -I $MES_PREFIX/lib $MES_PREFIX/lib/libc+gnu.c
./tcc -ar rc libc-new.a libc+gnu.o
"
                         bootstrap.sh)
                (close bootstrap.sh))

;;               (let ((boot.sh (open-file "boot.sh" "a")))
;;                 (display "
;; rm -f libc.a libc+gnu.o
;; ./${PROGRAM_PREFIX}tcc -c -I $MES_PREFIX/include -I $MES_PREFIX/lib $MES_PREFIX/lib/libc+gnu.c
;; ./${PROGRAM_PREFIX}tcc -ar rc libc.a libc+gnu.o
;; "
;;                          boot.sh)
;;                 (close boot.sh))

              ;; Show some progress
              (substitute* "bootstrap.sh"
                (("^( *)((cp|ls|mkdir|rm|[.]/tcc|[.]/[$][{PROGRAM_PREFIX[}]tcc) [^\"]*[^\\])\n" all space cmd)
                 (string-append space "echo \"" cmd "\"\n"
                                space cmd "\n")))

                (invoke "sh"  "bootstrap.sh")))
            (replace 'check
              (lambda _
                (setenv "DIFF" "diff.scm")
                (setenv "OBJDUMP" "true")
                ;; fail fast tests
                (system* "./tcc" "--help") ; --help exits 1
                ;;(invoke "sh" "test.sh" "mes/scaffold/tests/30-strlen")
                ;;(invoke "sh" "-x" "test.sh" "mes/scaffold/tinycc/00_assignment")
                ;;(invoke "sh" "check.sh")
                #t))
            (replace 'install
              (lambda _

              (let ((install.sh (open-file "install.sh" "a")))
                (display "
chmod +w $PREFIX/lib
cp $PREFIX/lib/libc.a $PREFIX/lib/libc-boot.a
chmod +w $PREFIX/lib/libc.a
rm -f $PREFIX/lib/libc.a
cp libc-new.a $PREFIX/lib/libc-new.a
cp libc-new.a $PREFIX/lib/libc.a
"
                         install.sh)
                (close install.sh))

                ;; Show some progress
                (substitute* "install.sh"
                  (("^( *)((cp|ls|mkdir|rm|tar|./[$][{PROGRAM_PREFIX[}]tcc) [^\"]*[^\\])\n" all space cmd)
                   (string-append space "echo \"" cmd "\"\n"
                                  space cmd "\n")))

                (invoke "sh" "install.sh"))))))
       (native-search-paths
        ;; Use the language-specific variables rather than 'CPATH' because they
        ;; are equivalent to '-isystem' whereas 'CPATH' is equivalent to '-I'.
        ;; The intent is to allow headers that are in the search path to be
        ;; treated as "system headers" (headers exempt from warnings) just like
        ;; the typical /usr/include headers on an FHS system.
        (list (search-path-specification
               (variable "C_INCLUDE_PATH")
               (files '("include")))
              (search-path-specification
               (variable "LIBRARY_PATH")
               (files '("lib")))))))))

(define-public make-mesboot0
  (package-with-bootstrap-guile
   (package
     (inherit gnu-make)
     (name "make-mesboot0")
     (version "3.80")
     (source (origin
               (method url-fetch)
               (uri (string-append "http://lilypond.org/janneke/mes/make-"
                                   version ".tar"))
               (sha256
                (base32
                 "0x1jwszfq9gv798flh6mj99vrp3l57wc366xw84x14vb0n5i80cg"))))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("tcc" ,tcc-boot0)

                      ("guile" ,%bootstrap-guile)
                      ("gash" ,%bootstrap-gash+coreutils)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:configure-flags `("CC=tcc"
                            "CPP=tcc -E"
                            "LD=tcc"
                            "--build=i686-unknown-linux-gnu"
                            "--host=i686-unknown-linux-gnu"
                            "--disable-nls")
        #:modules ((guix build gnu-build-system)
                   (guix build utils)
                   (srfi srfi-1)
                   (srfi srfi-26))
        #:strip-binaries? #f            ; no strip yet
        #:phases
        (modify-phases %standard-phases
          (delete 'patch-generated-file-shebangs) ; no perl
          (add-after 'unpack 'patch-configure
            (lambda _
              (substitute* "configure"
                ;; Gash temporarily lacks job control support
                (("[(] sleep [)] &" all)
                 (string-append "# " all)))
              (substitute* "build.sh.in"
                (("@REMOTE@") "stub"))
              (setenv "CPPFLAGS" "-D__alloca=alloca")
              (setenv "make_cv_union_wait" "no")
              ;; avoid Gash(?) bug
              ;; tcc: error: file './getloadavg$U.c' not found
              (setenv "ac_cv_objext" ".o")
              #t))
          (add-after 'configure 'configure-fixups
            (lambda _
              (let* ((out (assoc-ref %outputs "out"))
                     (localedir (string-append out "/share/locale")))
                (substitute* "config.h"
                  (("^/[*] #undef (HAVE_(ALLOCA_H|DIRENT_H|FCNTL_H|STRING_H|UNISTD_H)) [*]/" all have)
                   (string-append "#define " have " 1"))
                  ;; ... so we also need these
                  ;; (("^[*] #undef (HAVE_(ALLOCA_H|DIRENT_H|FCNTL_H|INTTYPES_H|LIMITS_H|LOCALE_H|MEMORY_H|STDINT_H|STDLIB_H|STRINGS_H|STRING_H|SYS_PARAM_H|SYS_STAT_H|SYS_TIMEB_H|SYS_TIME_H|SYS_TYPES_H|SYS_WAIT_H|UNISTD_H)) [*]/"
                  ;;   all have) (string-append "#define " have " 1"))
                  ;; (("^#undef (HAVE_(ALLOCA|DUP2|FDOPEN|FORK|GETCWD|GETTIMEOFDAY|MEMCPY|MEMMOVE|MKTEMP|PIPE|SA_RESTART|SETLOCALE|SETVBUF|SIGACTION|SIGSETMASK|STRCHR|STRDUP|STRERROR|VPRINTF|WAITPID|WORKING_FORK)) [*]/")
                  ;;  (string-append "#define " have " 1"))
                  )
                (let ((config.h (open-file "config.h" "a")))
                  (display (string-append "
#define LOCALEDIR \"" localedir"\"
#define PROTOTYPES 1
#define __STDC__ 1
#include <fcntl.h>
")
                           config.h)
                  (close config.h))
                (substitute* "build.sh"
                  (("^LOADLIBES=.*$") "LOADLIBES=\n")
                  (("[.][$]U[.][.]o") ".o"))
                (substitute* "make.h"
                  (("^extern long int lseek.*" all) (string-append "// " all)))
                #t)))
          (replace 'build
            (lambda _
              (invoke "sh" "-x" "./build.sh")))
          ;; proper check needs awk
          (replace 'check
            (lambda _
              (invoke "./make" "--version")
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
                (install-file "make" bin)
                #t)))))))))

;; Only used for development -- fast check
(define-public make-mesboot0-scripted
  (package-with-bootstrap-guile
   (package
     (inherit gnu-make)
     (name "make-mesboot0-scripted")
     (version "3.80")
     (source (origin
               (method url-fetch)
               (uri (string-append "http://lilypond.org/janneke/mes/make-"
                                   version ".tar"))
               (sha256
                (base32
                 "0x1jwszfq9gv798flh6mj99vrp3l57wc366xw84x14vb0n5i80cg"))))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("tcc" ,tcc-boot0)

                      ("guile" ,%bootstrap-guile)
                      ("gash" ,%bootstrap-gash+coreutils)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:configure-flags `("CC=tcc -DO_RDONLY=0"
                            "LD=tcc"
                            "--disable-nls")
        #:modules ((guix build gnu-build-system)
                   (guix build utils)
                   (srfi srfi-1)
                   (srfi srfi-26))
        #:strip-binaries? #f            ; no strip yet
        #:phases
        (modify-phases %standard-phases
          (replace 'configure
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (includedir (string-append out "/include"))
                     (libdir (string-append out "/lib"))
                     (localedir (string-append out "/share/locale"))
                     (host "i686-unknown-linux-gnu")
                     (version ,(package-version make-mesboot0)))
                (with-output-to-file "config.h"
                  (lambda _ (display (string-append "
#define ALIASPATH \""localedir"\":.
#define INCLUDEDIR \""includedir"\"
#define LIBDIR \""libdir"\"
#define LOCALEDIR \""localedir"\"

#define C_GETLOADAVG 1
#define FILE_TIMESTAMP_HI_RES 0
#define HAVE_ALLOCA 1
#define HAVE_ALLOCA_H 1
#define HAVE_DIRENT_H 1
#define HAVE_DUP2 1
#define HAVE_FCNTL_H 1
#define HAVE_FDOPEN 1
#define HAVE_FORK 1
#define HAVE_GETCWD 1
#define HAVE_GETTIMEOFDAY 1
#define HAVE_INTTYPES_H 1
#define HAVE_LIMITS_H 1
#define HAVE_LOCALE_H 1
#define HAVE_MEMCPY 1
#define HAVE_MEMMOVE 1
#define HAVE_MEMORY_H 1
#define HAVE_MKTEMP 1
#define HAVE_PIPE 1
#define HAVE_SA_RESTART 1
#define HAVE_SETLOCALE 1
#define HAVE_SETVBUF 1
#define HAVE_SIGACTION 1
#define HAVE_SIGSETMASK 1
#define HAVE_STDINT_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRCHR 1
#define HAVE_STRDUP 1
#define HAVE_STRERROR 1
#define HAVE_STRINGS_H 1
#define HAVE_STRING_H 1
#define HAVE_SYS_PARAM_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TIMEB_H 1
#define HAVE_SYS_TIME_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_SYS_WAIT_H 1
#define HAVE_UNISTD_H 1
#define HAVE_VPRINTF 1
#define HAVE_WAITPID 1
#define HAVE_WORKING_FORK 1
#define MAKE_HOST \"" host "\"
#define MAKE_JOBSERVER 1
#define PACKAGE \"make\"
#define PACKAGE_BUGREPORT \"bug-make@gnu.org\"
#define PACKAGE_NAME \"GNU make\"
#define PACKAGE_STRING \"GNU make "version"\"
#define PACKAGE_TARNAME \"make\"
#define PACKAGE_VERSION \""version"\"
#define PROTOTYPES 1
#define RETSIGTYPE void
#define SCCS_GET \"get\"
#define STDC_HEADERS 1
#define TIME_WITH_SYS_TIME 1
#define VERSION \"" version" \"
#define __PROTOTYPES 1
#define vfork fork
")))))
              #t))
          (delete 'patch-generated-file-shebangs) ; no perl
          (add-after 'configure 'configure-fixup
            (lambda _
              (substitute* "make.h"
                (("^extern long int lseek.*" all) (string-append "// " all)))
              #t))
          (replace 'build
            (lambda _
              (let ((files '(
                             "ar"
                             "arscan"
                             "commands"
                             "default"
                             "dir"
                             "expand"
                             "file"
                             "function"
                             "getopt"
                             "getopt1"
                             "implicit"
                             "job"
                             "main"
                             "misc"
                             "read"
                             "remake"
                             "rule"
                             "signame"
                             "variable"
                             "version"
                             "vpath"
                             "hash"
                             "remote-stub"
                             "getloadavg"
                             "glob/fnmatch"
                             "glob/glob"
                             )))
                (and (fold-right
                      (lambda (file status)
                        (and status (invoke "tcc"
                                            "-D" "HAVE_CONFIG_H"
                                            "-I" "."
                                            "-I" "glob"
                                            "-c" (string-append file ".c")
                                            "-o" (string-append file ".o"))))
                      #t
                      files)
                     (apply invoke `("tcc" "-o" "make"
                                     ,@(map (cut string-append <> ".o")
                                            files)))))))
          (replace 'check
            (lambda _
              (invoke "./make" "--version")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
                (install-file "make" bin)
                #t)))))))))

(define-public gzip-mesboot0
  (package-with-bootstrap-guile
   (package
     (inherit gzip)
     (version "1.2.4")
     (name "gzip-mesboot0")
     (source (origin
               (method url-fetch)
               (uri (string-append
                     "http://lilypond.org/janneke/mes/"
                     "gzip-1.2.4.tar"))
               (sha256
                (base32
                 "0bk71r26ixlkvmb7wrmxsafiqhf4c4hmczbp9mi55814psiv1q23"))))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("make" ,make-mesboot0)
                      ("tcc" ,tcc-boot0)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:parallel-build? #f
        #:tests? #f           ; check is naive, also checks non-built PROGRAMS
        #:strip-binaries? #f  ; no strip yet
        #:configure-flags '("TIME_T_32_BIT_OK=yes"
                            "gl_cv_func_gettimeofday_clobber=y")
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'scripted-patch
            (lambda _
              (substitute* "Makefile.in"
                (("\tmkdir") "	mkdir -p ")
                ((" mkdir") " mkdir -p "))
              (substitute* "util.c"
                (("^char [*]strlwr" all) (string-append all "_tcc_cannot_handle_dupe")))
              #t))
          (add-before 'configure 'setenv
            (lambda _
              (let* ((out (assoc-ref %outputs "out"))
                     (gash (assoc-ref %build-inputs "gash"))
                     (shell (string-append gash "/bin/gash")))
                (setenv "CONFIG_SHELL" shell)
                (setenv "SHELL" shell)
                (setenv "CC" "tcc")
                (setenv "LD" "tcc"))
              #t))
          ;; grep-1.2.4 needs a more traditional configure
          (replace 'configure
            (lambda* (#:key configure-flags #:allow-other-keys)
              (let* ((out (assoc-ref %outputs "out"))
                     (gash (assoc-ref %build-inputs "gash"))
                     (configure-flags `(,@configure-flags
                                        ,(string-append "--prefix=" out))))
                (format (current-error-port)
                        "running ./configure ~a\n" (string-join configure-flags))
                (apply system* "./configure" configure-flags)
                (invoke "sh" "-x" "./config.status"))))
          ;; no gzip yet
          (delete 'compress-documentation)))))))

(define-public tar-mesboot0
  (package-with-bootstrap-guile
   (package
     (inherit tar)
     (name "tar-mesboot0")
     (version  "1.12")
     ;; (source (origin
     ;;           (method url-fetch)
     ;;           (uri (string-append "mirror://gnu/tar/tar-"
     ;;                               version
     ;;                               ".tar.gz"))
     ;;           (sha256
     ;;            (base32
     ;;             "02m6gajm647n8l9a5bnld6fnbgdpyi4i3i83p7xcwv0kif47xhy6"))))
     (source (origin
               (method url-fetch)
               (uri (string-append
                     "http://lilypond.org/janneke/mes/"
                     "tar" "-" version ".tar"))
               (sha256
                (base32
                 "0hhjsxzvj4hb1pwigsybgr76x01xwa8vdkih132na8mm3mn3lpnw"))))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("make" ,make-mesboot0)
                      ("mes" ,mes-boot)
                      ("tcc" ,tcc-boot0)

                      ("gash" ,%bootstrap-gash+coreutils)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:parallel-build? #f
        #:tests? #f           ; check is naive, also checks non-built PROGRAMS
        #:strip-binaries? #f  ; no strip yet
        #:configure-flags '("--build=i686-unknown-linux-gnu"
                            "--host=i686-unknown-linux-gnu"
                            "--disable-nls")
        #:phases
        (modify-phases %standard-phases
          (replace 'configure
            (lambda* (#:key configure-flags #:allow-other-keys)
              (let* ((out (assoc-ref %outputs "out"))
                     (gash (assoc-ref %build-inputs "gash"))
                     (shell (string-append gash "/bin/gash")))
                (setenv "CONFIG_SHELL" shell)
                (setenv "SHELL" shell)
                (setenv "CC" "tcc")
                (setenv "LD" "tcc")
                (substitute* "configure"
                  ((" /bin/sh") shell))
                (substitute* "Makefile.in"
                  ((" lib intl ") " lib "))
                (substitute* "lib/Makefile.in"
                  (("\\$\\(AR\\) cru ") "tcc -ar cr "))
                (format (current-error-port)
                        "running ./configure ~a\n" (string-join configure-flags))
                (apply invoke (cons "./configure" configure-flags)))))
          (add-after 'configure 'fixup-configure
            (lambda _
              (substitute* "config.h"
                (("/[*] #undef HAVE_DIRENT_H [*]/") "#define HAVE_DIRENT_H 1")
                (("/[*] #undef HAVE_FCNTL_H [*]/") "#define HAVE_FCNTL_H 1")
                (("/[*] #undef HAVE_UNISTD_H [*]/") "#define HAVE_UNISTD_H 1")

                (("/[*] #undef HAVE_GETCWD [*]/") "#define HAVE_GETCWD 1")
                (("/[*] #undef HAVE_MKTIME [*]/") "#define HAVE_MKTIME 1")

                (("#undef HAVE_DIRENT_H") "#define HAVE_DIRENT_H 1")
                (("#undef HAVE_FCNTL_H") "#define HAVE_FCNTL_H 1")
                (("#undef HAVE_UNISTD_H") "#define HAVE_UNISTD_H 1"))

              (let ((config.h (open-file "config.h" "a")))
                (display (string-append "
#include \"dirent.h\"
")
                         config.h)
                (close config.h))
              #t))
          (replace 'install
            (lambda _
              (let* ((out (assoc-ref %outputs "out"))
                     (bin (string-append out "/bin")))
                (install-file "src/tar" bin)
                #t)))))))))

(define-public gawk-mesboot0
  (package-with-bootstrap-guile
   (package
     (inherit gawk)
     (name "gawk-mesboot0")
     (version "3.0.0")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/gawk/gawk-"
                                   version
                                   ".tar.gz"))
               (sha256
                (base32
                 "087s7vpc8zawn3l7bwv9f44bf59rc398hvaiid63klw6fkbvabr3"))))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("gzip" ,gzip-mesboot0)
                      ("make" ,make-mesboot0)
                      ("tcc" ,tcc-boot0)
                      ("tar" ,tar-mesboot0)

                      ("guile" ,%bootstrap-guile)
                      ("gash" ,%bootstrap-gash+coreutils)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:configure-flags '("--build=i686-unknown-linux-gnu"
                            "--host=i686-unknown-linux-gnu"
                            "--disable-nls")
        #:make-flags '("gawk")
        #:strip-binaries? #f            ; no strip yet
        #:phases
        (modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "zxvf" source)
              ;;(chdir (first-subdirectory "."))
              (chdir (string-append "gawk-" ,version))
              #t))
          (add-after 'unpack 'scripted-patch
            (lambda _
              (substitute* "Makefile.in"
                (("date ") "echo today ")
                ((" autoheader") "true")
                ((" -lm ") " "))
              (substitute* "test/Makefile.in"
                (("^bigtest:.*") "bigtest: basic\n")
                (("( |\t)(childin|convfmt|fflush|longwrds|math|negexp)" all sep) sep))))
          (add-before 'configure 'setenv
            (lambda _
              (let* ((out (assoc-ref %outputs "out"))
                     (gash (assoc-ref %build-inputs "gash"))
                     (shell (string-append gash "/bin/gash")))
                (setenv "CONFIG_SHELL" shell)
                (setenv "SHELL" shell)
                (setenv "CC" "tcc")
                (setenv "CPP" "tcc -E")
                (setenv "LD" "tcc"))
              #t))
          ;; gawk-3.0.0 needs a more traditional configure
          (replace 'configure
            (lambda* (#:key configure-flags #:allow-other-keys)
              (let* ((out (assoc-ref %outputs "out"))
                     (configure-flags
                      `(,@configure-flags
                        ,(string-append "--prefix=" out))))
                (format (current-error-port) "running ./configure ~a\n" (string-join configure-flags))
                (apply invoke (cons "./configure" configure-flags)))))
          (add-after 'configure 'configure-fixups
            (lambda _
              (let ((config.h (open-file "config.h" "a")))
                (display (string-append "
#define __STDC__ 1
#define HAVE_MKTIME 1
#define HAVE_TZSET 1
#define tzset() 0
#define GETPGRP_VOID 1

#define atan2(x,y) (x/y)
#define ceil(x) ((int)(x+1))
#define cos(x) x
#define exp(x) (3*(int)x)
#define floor(x) ((int)x)
#define log(x) ((int)x)
#define modf(x,y) 0
#define pow(x,y) 0
#define sin(x) 0
#define sqrt(x) 0
")
                         config.h)
                (close config.h))
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
                (install-file "gawk" bin)
                (symlink "gawk" (string-append bin "/awk"))
                #t)))))))))

(define-public bash-mesboot0
  ;; The initial Bash
  (package-with-bootstrap-guile
   (package
     (inherit static-bash)
     (name "bash-mesboot0")
     (version "2.05b")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/bash/bash-"
                                   version
                                   ".tar.gz"))
               (sha256
                (base32
                 "1r1z2qdw3rz668nxrzwa14vk2zcn00hw7mpjn384picck49d80xs"))))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("gzip" ,gzip-mesboot0)
                      ("make" ,make-mesboot0)
                      ("tar" ,tar-mesboot0)
                      ("tcc" ,tcc-boot0)

                      ("guile" ,%bootstrap-guile)
                      ("gash" , %bootstrap-gash+coreutils)))
     (outputs '("out"))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:parallel-build? #f
        #:strip-binaries? #f   ; no strip yet
        #:configure-flags
        '("--build=i686-unknown-linux-gnu"
          "--host=i686-unknown-linux-gnu"

          "--without-bash-malloc"
          "--disable-readline"
          "--disable-history"
          "--disable-help-builtin"
          "--disable-progcomp"
          "--disable-net-redirections"
          "--disable-nls"

          ;; Pretend 'dlopen' is missing so we don't build loadable
          ;; modules and related code.
          "ac_cv_func_dlopen=no")
        #:make-flags '("bash")
        #:phases
        (modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "zxvf" source)
              ;;(chdir (first-subdirectory "."))
              (chdir (string-append "bash-" ,version))
              #t))
          (add-before 'configure 'setenv
            (lambda _
              (let* ((gash (assoc-ref %build-inputs "gash"))
                     (shell (string-append gash "/bin/gash")))
                (setenv "CONFIG_SHELL" shell)
                (setenv "SHELL" shell)
                (setenv "CC" "tcc")
                (setenv "LD" "tcc")
                (setenv "AR" "tcc -ar")
                (setenv "CFLAGS" "-D _POSIX_VERSION=1")
                #t)))
          (add-after 'unpack 'scripted-patch
            (lambda _
              (substitute* "Makefile.in"
                (("mksyntax\\.c\n") "mksyntax.c -lgetopt\n")
                (("buildversion[.]o\n") "buildversion.o -lgetopt\n")
                ;; No size in Gash
                (("\tsize ") "#\tsize"))
              (substitute* "lib/sh/oslib.c"
                (("int name, namelen;") "char *name; int namelen;"))
              (substitute* "lib/sh/snprintf.c"
                (("^#if (defined [(]HAVE_LOCALE_H[)])" all define) (string-append "#if 0 //" define)))
              #t))
          (replace 'configure
            (lambda* (#:key configure-flags #:allow-other-keys)
              (let ((configure-flags (filter (lambda (x)
                                               (and (not (string-prefix? "CONFIG_SHELL=" x))
                                                    (not (string-prefix? "SHELL=" x))))
                                             configure-flags)))
                (format (current-error-port)
                        "running ./configure ~a\n" (string-join configure-flags)))
              (apply invoke (cons "./configure" configure-flags))))
          (add-after 'configure 'configure-fixups
            (lambda _
              (substitute* "config.h"
                (("#define GETCWD_BROKEN 1") "#undef GETCWD_BROKEN"))
              (let ((config.h (open-file "config.h" "a")))
                (display (string-append "
// tcc: error: undefined symbol 'enable_hostname_completion'
#define enable_hostname_completion(on_or_off) 0

// /gnu/store/cq0cmv35s9dhilx14zaghlc08gpc0hwr-tcc-boot0-0.9.26-6.c004e9a/lib/libc.a: error: 'sigprocmask' defined twice
#define HAVE_POSIX_SIGNALS 1
#define endpwent(x) 0
")
                         config.h)
                (close config.h))
              #t))
          (replace 'check
            (lambda _
              (invoke "./bash" "--version")))
          (replace 'install
            (lambda _
              (let* ((out (assoc-ref %outputs "out"))
                     (bin (string-append out "/bin")))
                (mkdir-p bin)
                (copy-file "bash" (string-append bin "/bash"))
                (copy-file "bash" (string-append bin "/sh"))
                #t)))))))))

(define-public bash-mesboot1
  ;; The initial Bash
  (package-with-bootstrap-guile
   (package
     (inherit static-bash)
     (version "4.4")
     (name "bash-mesboot1")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/bash/bash-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "1jyz6snd63xjn6skk7za6psgidsd53k05cr3lksqybi0q6936syq"))))
     ;; (source (origin
     ;;           (method url-fetch)
     ;;           (uri (string-append
     ;;                 "http://lilypond.org/janneke/mes/"
     ;;                 "bash-4.4.tar"))
     ;;           (sha256
     ;;            (base32
     ;;             "1p0ixbf04bbbdrzsd1y1ibzvk3iwwihmrkfh6irwqcaz93xi7rxb"))))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("gawk" ,gawk-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("make" ,make-mesboot0)
                      ("tar" ,tar-mesboot0)
                      ("tcc" ,tcc-boot0)

                      ("guile" ,%bootstrap-guile)
                      ("gash" , %bootstrap-gash+coreutils)))
     (outputs '("out"))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:parallel-build? #f
        #:strip-binaries? #f   ; no strip yet
        #:configure-flags
        '("--build=i686-unknown-linux-gnu"
          "--host=i686-unknown-linux-gnu"

          "--without-bash-malloc"
          "--disable-readline"
          "--disable-history"
          "--disable-help-builtin"
          "--disable-progcomp"
          "--disable-net-redirections"
          "--disable-nls"

          "CC=tcc"
          "AWK=gawk"
          "CC=tcc"
          "LD=tcc"
          "AR=tcc -ar"
          "CFLAGS=-D _POSIX_VERSION=1"

          ;; Pretend 'dlopen' is missing so we don't build loadable
          ;; modules and related code.
          "ac_cv_func_dlopen=no"
          "acl_cv_prog_gnu_ld=no" ; this often hangs; short-circuit
          "acl_cv_rpath=no")
        #:make-flags '("bash")
        #:phases
        (modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "zxvf" source)
              ;;(chdir (first-subdirectory "."))
              (chdir (string-append "bash-" ,version))
              #t))
          (add-before 'configure 'subst
            (lambda _
              (let ((tcc (assoc-ref %build-inputs "tcc")))
                (setenv "LIBS_FOR_BUILD" (string-append tcc "/lib/x86-mes-gcc/libgetopt.o"))
                (substitute* "Makefile.in"
                  (("^LIBS_FOR_BUILD = ") (string-append "LIBS_FOR_BUILD = " tcc "/lib/x86-mes-gcc/libgetopt.o"))))))
          (add-after 'configure 'configure-fixups
            (lambda _
              (let ((config.h (open-file "config.h" "a")))
                (display (string-append "
// tcc: error: undefined symbol 'enable_hostname_completion'
#define enable_hostname_completion(on_or_off) 0

// /gnu/store/cq0cmv35s9dhilx14zaghlc08gpc0hwr-tcc-boot0-0.9.26-6.c004e9a/lib/libc.a: error: 'sigprocmask' defined twice
#define HAVE_POSIX_SIGNALS 1
")
                         config.h)
                (close config.h))
              #t))
          (replace 'check
            (lambda _
              (invoke "./bash" "--version")))
          (replace 'install
            (lambda _
              (let* ((out (assoc-ref %outputs "out"))
                     (bin (string-append out "/bin")))
                (mkdir-p bin)
                (copy-file "bash" (string-append bin "/bash"))
                (copy-file "bash" (string-append bin "/sh"))
                #t)))))))))

;; FIXME: tar-1.15 would make overriding of `unpack' stage unecessary...but it
;; only lists/unpack the first directory

;; version 1.15 - Sergey Poznyakoff, 2004-12-20

;; * Compressed archives are recognised automatically, it is no longer
;; necessary to specify -Z, -z, or -j options to read them.  Thus, you can
;; now run 'tar tf archive.tar.gz'.

;; version 1.15.1 - Sergey Poznyakoff, 2004-12-21

;; This version fixes a bug introduced in 1.15 which caused
;; tar to refuse to extract files from standard input.

(define-public tar-mesboot1
  (package-with-bootstrap-guile
   (package
     (inherit tar)
     (name "tar-mesboot1")
     ;; ;; (version  "1.15")
     ;; (version  "1.15.1")
     ;; (source (origin
     ;;           (method url-fetch)
     ;;           (uri (string-append
     ;;                 "http://lilypond.org/janneke/mes/"
     ;;                 "tar" "-" version ".tar"))
     ;;           (sha256
     ;;            (base32
     ;;             ;; 1.15
     ;;             ;;"0cz53piqv27qgix1n6cpfdyz0mmbamy26bba9gqj94hghsi9f54w"
     ;;             ;; 1.15.1
     ;;             "0sm9mhrx652zxgp156qyflnfimbhmal3ifzk9gvybl76fjav5jbw"
     ;;             ))))

     ;; (version  "1.22")
     ;; (source (origin
     ;;           (method url-fetch)
     ;;           (uri (string-append
     ;;                 "http://lilypond.org/janneke/mes/"
     ;;                 "tar" "-" version ".tar"))
     ;;           (sha256
     ;;            (base32
     ;;             "1gkilfwnbjqb35vn6978lby4gmp74q0dzhrrsfix8cg1qnlwd2b3"))))

     ;; fseeko.c:41: warning: implicit declaration of function 'fileno'
     ;; fseeko.c:88: error: #error "Please port gnulib fseeko.c to your platform! Look at the code in fpurge.c, then report this to bug-gnulib."
     (version  "1.22")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/tar/tar-"
                                   version
                                   ".tar.gz"))
               (sha256
                (base32
                 "19nvix64y95n5v6rr5g9g3fn08zz85cb5anzd7csfv4a4sz9lw4y"))))
     ;; (version  "1.23")
     ;; (source (origin
     ;;           (method url-fetch)
     ;;           ;; (uri (string-append
     ;;           ;;       "http://lilypond.org/janneke/mes/"
     ;;           ;;       "tar" "-" version ".tar"))
     ;;           (uri (string-append "mirror://gnu/tar/tar-"
     ;;                               version
     ;;                               ".tar.gz"))
     ;;           (sha256
     ;;            (base32
     ;;             "0dnni4awcfzbdhkxlvh90z5qqvxr0dcc4ij537cs2g9v9pd81hnb"))))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("bash" ,bash-mesboot0)
                      ("make" ,make-mesboot0)
                      ("mes" ,mes-boot)
                      ("gawk" ,gawk-mesboot0)
                      ("tcc" ,tcc-boot0)
                      ("gzip" ,gzip-mesboot0)
                      ("tar" ,tar-mesboot0)
                      
                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:parallel-build? #f
        #:tests? #f           ; check is naive, also checks non-built PROGRAMS
        #:strip-binaries? #f  ; no strip yet
        #:configure-flags '("CC=tcc"
                            "LD=tcc"
                            "AR=tcc -ar"
                            "--build=i686-unknown-linux-gnu"
                            "--host=i686-unknown-linux-gnu"
                            "--disable-nls"
                            "ac_cv_func_ftruncate=yes")
        #:make-flags '("AR=tcc -ar")
        #:phases
        (modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "zxvf" source)
              ;;(chdir (first-subdirectory "."))
              (chdir (string-append "tar-" ,version))
              #t))
          ;; (add-after 'unpack 'scripted-patch
          ;;   (lambda _
          ;;     (let* (
          ;;            ;; (bash (assoc-ref %build-inputs "bash"))
          ;;            ;; (shell (string-append bash "/bin/bash"))
          ;;            )
          ;;      ;; (substitute* "configure"
          ;;      ;;   ((" /bin/sh") shell))
          ;;      ;; (substitute* "Makefile.in"
          ;;      ;;   ((" lib intl ") " lib ")
          ;;      ;;   (("^SUBDIRS = doc") "SUBDIRS ="))
          ;;      ;; (substitute* "lib/Makefile.in"
          ;;      ;;   (("\\$\\(AR\\) cru ") "tcc -ar cr "))
          ;;      ;; (substitute* "lib/Makefile.in"
          ;;      ;;   (("\\$\\(AR\\) cru ") "tcc -ar cr "))
          ;;      ;; (substitute* "lib/rtapelib.c"
          ;;      ;;   (("DEFAULT_RMT_COMMAND;") "\"rmt\";"))
          ;;      ;; (substitute* "lib/argp-eexst.c"
          ;;      ;;   (("#include <sysexits.h>") "//#include <sysexits.h>"))
          ;;      ;; (substitute* "lib/argp-parse.c"
          ;;      ;;   (("#define N_[(]msgid[)] [(]msgid[)]") "#define N_(msgid) msgid"))
          ;;      ;; (substitute* "lib/human.c"
          ;;      ;;   (("l->decimal_point") "\".\"")
          ;;      ;;   (("l->grouping") "\"\"")
          ;;      ;;   (("l->thousands_sep") "\",\""))
          ;;      ;; (substitute* "lib/modechange.h"
          ;;      ;;   (("# include <sys/types.h>" all)
          ;;      ;;    (string-append all "\n#include <sys/stat.h>")))
          ;;      ;; (substitute* "lib/mktime.c"
          ;;      ;;   (("^time_t") "int"))

          ;;      ;; In file included from imaxtostr.c:6:
          ;;      ;; inttostr.c: In function `imaxtostr':
          ;;      ;; inttostr.c:37: parse error before `extern'
          ;;      ;; gnu/verify.h: # define verify(R) extern int (* verify_function__ (void)) [verify_true (R)]
          ;;      (substitute* "gnu/verify.h"
          ;;        (("# define verify.*") "#define verify(x) 1\n"))
          ;;      (substitute* "gnu/getdate.c"
          ;;        (("^verify* [(].*;" all) (string-append "//" all "\n"))
          ;;        (("^verify* [(].*" all) "//verify..."))
          ;;      (substitute* "gnu/backupfile.c"
          ;;        (("^ARGMATCH_VERIFY" all) (string-append "//" all)))
          ;;      #t)))
          (add-after 'configure 'fixup-configure
            (lambda _
              ;; (substitute* "config.h"
              ;;   (("/[*] #undef HAVE_DIRENT_H [*]/") "#define HAVE_DIRENT_H 1")
              ;;   (("/[*] #undef HAVE_FCNTL_H [*]/") "#define HAVE_FCNTL_H 1")
              ;;   (("/[*] #undef HAVE_UNISTD_H [*]/") "#define HAVE_UNISTD_H 1")

              ;;   (("/[*] #undef HAVE_GETCWD [*]/") "#define HAVE_GETCWD 1")
              ;;   (("/[*] #undef HAVE_MKTIME [*]/") "#define HAVE_MKTIME 1")

              ;;   (("#undef HAVE_DIRENT_H") "#define HAVE_DIRENT_H 1")
              ;;   (("#undef HAVE_FCNTL_H") "#define HAVE_FCNTL_H 1")
              ;;   (("#undef HAVE_UNISTD_H") "#define HAVE_UNISTD_H 1"))

              (let ((config.h (open-file "config.h" "a")))
                (display (string-append "
#include <dirent.h>
#include <fcntl.h>
#define MB_LEN_MAX 16
#define localeconv(x) 0
#define EX_USAGE 1

#ifndef O_NONBLOCK
#define O_NONBLOCK 00004000
#endif
#ifndef O_NOCTTY
#define O_NOCTTY 00000400
#endif

#define	EPERM 1
#define CLOCK_REALTIME 1
#define LC_ALL 0

#define asctime(x) 0
#define getpagesize(x) 0x1000
#define strtok(x,y) 0
#define asctime(x) 0

#define ftruncate(x) 0
")
                         config.h)
                (close config.h))
              #t))
          (replace 'install
            (lambda _
              (let* ((out (assoc-ref %outputs "out"))
                     (bin (string-append out "/bin")))
                (install-file "src/tar" bin)
                #t)))))))))

(define-public sed-mesboot0
  (package-with-bootstrap-guile
   (package
     (inherit sed)
     (name "sed-mesboot0")
     (version "1.18")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/sed/sed-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "1hyv7i82jd0q18xcql51ylc8jwadp3gb3irgcqlis3v61p35jsv2"))))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("bash" ,bash-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("make" ,make-mesboot0)
                      ("tar" ,tar-mesboot0)
                      ("tcc" ,tcc-boot0)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:parallel-build? #f
        #:make-flags '("CC=tcc")
        #:strip-binaries? #f            ; no strip yet
        #:phases
        (modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "zxvf" source)
              ;;(chdir (first-subdirectory "."))
              (chdir (string-append "sed-" ,version))
              #t))
          (add-after 'unpack 'scripted-patch
            (lambda _
              (let* ((out (assoc-ref %outputs "out"))
                     (bash (assoc-ref %build-inputs "bash"))
                     (shell (string-append bash "/bin/bash")))
                (substitute* "configure"
                  (("/bin/sh") shell))
                #t)))
          (add-before 'configure 'setenv
            (lambda _
              (setenv "CC" "tcc")
              #t))
          (add-after 'configure 'fixup-configure
            (lambda _
              (substitute* "Makefile"
                (("HAVE_bcopy") "HAVE_BCOPY")
                (("HAVE_memcpy") "HAVE_MEMCPY")
                (("HAVE_string_h") "HAVE_STRING_H"))
              #t))
          (replace 'check
            (lambda _
              (invoke "./sed" "--version")))
          (replace 'install
            (lambda _
              (let* ((out (assoc-ref %outputs "out"))
                     (bin (string-append out "/bin")))
                (install-file "sed" bin)
                #t)))))))))

(define-public grep-mesboot0
  (package-with-bootstrap-guile
   (package
     (inherit sed)
     (name "grep-mesboot0")
     (version "2.0") ;; 1996
     ;;(version "2.5") ;; 2004
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/grep/grep-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "1w862l80lgc5mxvpiy4cfwk761d6xxavn0m3xd2l7xs2kmzvp6lq"))))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("gzip" ,gzip-mesboot0)
                      ("make" ,make-mesboot0)
                      ("tar" ,tar-mesboot0)
                      ("tcc" ,tcc-boot0)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:parallel-build? #f
        #:make-flags '("grep") ; target `all' includes check, which needs Awk
        #:strip-binaries? #f            ; no strip yet
        #:phases
        (modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "zxvf" source)
              ;;(chdir (first-subdirectory "."))
              (chdir (string-append "grep-" ,version))
              #t))

          (add-before 'configure 'patch-configure
            (lambda _
              (let* ((gash (assoc-ref %build-inputs "gash"))
                     (shell (string-append gash "/bin/gash")))
                (substitute* "configure"
                  ;; Gash bug: 50-redirect-sh-exec.sh
                  ((" [|][|] ./config.status") " || sh ./config.status")
                  ;; Gash bug: 30-substitution-escape-doublequote.sh
                  (("echo [\\]\"[$]DEFS[\\]\" ") "echo $DEFS")))))

          ;; gcc -O -DGREP  -DSTDC_HEADERS=1 -DHAVE_string_h=1 -DHAVE_sys_pArAm_h=1 -DHAVE_UNISTD_H=1 -DHAVE_ALLOCA_H=1 -DHAVE_getpAgesiZe=1 -DHAVE_memchr=1 -DHAVE_strerror=1 -DHAVE_vAlloc=1 -DHAVE_WORKING_MMAP=1 -I. -c grep.c

          (add-before 'configure 'setenv
            (lambda _
              (setenv "CC" "tcc")
              #t))

          (add-after 'configure 'fixup-configure
            (lambda _
              (substitute* "Makefile"
                (("HAVE_getpAgesiZe") "HAVE_GETPAGESIZE")
                (("HAVE_memchr") "HAVE_MEMCHR")
                (("HAVE_memcpy") "HAVE_MEMCPY")
                (("HAVE_strerror") "HAVE_STRERROR")
                (("HAVE_string_h") "HAVE_STRING_H")
                (("HAVE_sys_pArAm_h") "HAVE_SYSPARAM_H")
                (("HAVE_vAlloc") "HAVE_VALLOC"))
              #t))

          (replace 'check               ; needs Awk
            (lambda _
              (invoke "./grep" "GNU" "README")))
          (replace 'install             ; target `install' ensures `check'
            (lambda _
              (let* ((out (assoc-ref %outputs "out"))
                     (bin (string-append out "/bin")))
                (install-file "grep" bin)
                (symlink "grep" (string-append bin "/egrep"))
                (symlink "grep" (string-append bin "/fgrep"))
                #t)))))))))

(define-public patch-mesboot0
  (package-with-bootstrap-guile
   (package
     (inherit patch)
     (name "patch-mesboot0")
     (version "2.5.9")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/patch/patch-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "12nv7jx3gxfp50y11nxzlnmqqrpicjggw6pcsq0wyavkkm3cddgc"))))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("bash" ,bash-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("make" ,make-mesboot0)
                      ("mes" ,mes-boot)
                      ("tar" ,tar-mesboot0)
                      ("tcc" ,tcc-boot0)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:parallel-build? #f
        #:tests? #f           ; check is naive, also checks non-built PROGRAMS
        #:strip-binaries? #f  ; no strip yet
        #:phases
        (modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "zxvf" source)
              ;;(chdir (first-subdirectory "."))
              (chdir (string-append "patch-" ,version))
              #t))
          (add-after 'unpack 'scripted-patch
            (lambda _
              ;; avoid another segfault
              (substitute* "pch.c"
                (("while [(]p_end >= 0[)]" all)
                 "p_end = -1;\nwhile (0)"))
              #t))
          (add-before 'configure 'setenv
            (lambda _
              (setenv "AR" "tcc -ar")
              (setenv "CC" "tcc")
              (setenv "LD" "tcc")
              #t))
          (add-after 'configure 'fixup-configure
            (lambda _
              (let ((config.h (open-file "config.h" "a")))
                (display (string-append "
#define	EXDEV 18
")
                         config.h)
                (close config.h))
              #t))))))))

(define-public bzip2-mesboot0
  (package-with-bootstrap-guile
   (package
     (inherit bzip2)
     (name "bzip2-mesboot0")
     (version (package-version bzip2))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("bash" ,bash-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("make" ,make-mesboot0)
                      ("mes" ,mes-boot)
                      ("tar" ,tar-mesboot0)
                      ("tcc" ,tcc-boot0)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)))
     (outputs '("out"))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:parallel-build? #f
        #:tests? #f           ; check is naive, also checks non-built PROGRAMS
        #:strip-binaries? #f  ; no strip yet
        #:make-flags (list "CC=tcc -I ." "AR=tcc -ar" "bzip2"
                           (string-append "PREFIX="
                                          (assoc-ref %outputs "out")))
        #:phases
        (modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "zxvf" source)
              ;;(chdir (first-subdirectory "."))
              (chdir (string-append "bzip2-" ,version))
              #t))
          (add-after 'unpack 'scripted-patch
            (lambda _
              (substitute* "Makefile"
                (("\tln " all)
                 (string-append "\t#" all)))
              (substitute* "bzip2.c"
                (("struct utimbuf uTimBuf;" all)
                 (string-append "// " all))
                (("uTimBuf[.]" all)
                 (string-append "// " all))
                (("retVal = utime [(] dstName, &uTimBuf [)];" all)
                 (string-append "retVal = 0; // " all)))
              #t))
          (replace 'configure
            (lambda _
              (with-output-to-file "utime.h"
                (lambda _ (display "
#define fchown(filedes, owner, group) 0
#define fchmod(filedes, mode) 0
")))
              #t))
          (replace 'check
            (lambda _
              (invoke "./bzip2" "--help")))))))))

;; XZ's configure refuses to build with tcc
(define-public xz-mesboot0 ;; WIP
  (package-with-bootstrap-guile
   (package
     (inherit xz)
     (name "xz-mesboot0")
     ;; needs poll.h etc
     ;; file_io.c:20: error: include file 'poll.h' not found
     ;; (version (package-version xz))
     (version "5.0.0")
     ;; check/crc32_fast.c:38: error: cannot use pointers here
     ;; while ((uintptr_t)(buf) & 7) {
     (source (origin
               (method url-fetch)
               (uri (list (string-append "http://tukaani.org/xz/xz-" version
                                         ".tar.gz")
                          (string-append "http://multiprecision.org/guix/xz-"
                                         version ".tar.gz")))
               (sha256
                (base32
                 "0kf40ggbs1vaaj5s9k4csycahzqcf65n20pa6lngqhm6j0cj3agb"))))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("bash" ,bash-mesboot0)
                      ("gawk" ,gawk-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("make" ,make-mesboot0)
                      ("mes" ,mes-boot)
                      ("sed" ,sed-mesboot0)
                      ("tar" ,tar-mesboot0)
                      ("tcc" ,tcc-boot0)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:parallel-build? #f
        #:tests? #f           ; check is naive, also checks non-built PROGRAMS
        #:strip-binaries? #f  ; no strip yet
        #:configure-flags '("--disable-assembler"
                            "--enable-small"
                            "--disable-threads"
                            "--disable-xzdec"
                            "--disable-lzmadec"
                            "--disable-lzmainfo"
                            "--disable-lzma-links"
                            "--disable-scripts"
                            "--disable-doc"
                            "--disable-nls"
                            "--disable-symbol-versions")
        #:phases
        (modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "zxvf" source)
              ;;(chdir (first-subdirectory "."))
              (chdir (string-append "xz-" ,version))
              #t))
          (add-after 'unpack 'scripted-patch
            (lambda _
              (substitute* "src/liblzma/check/crc32_fast.c"
                (("while [(][(]uintptr_t[)][(]buf[)] & 7[)]")
                 "while ((long)(buf) & 7) {"))
              (substitute* "src/liblzma/check/sha256.c"
                (("[[]static 8[]]") "[8]")
                (("[[]static 16[]]") "[16]"))
              (substitute* "src/xz/signals.h"
                (("extern volatile sig_atomic_t user_abort;")
                 "extern volatile long user_abort;"))
              #t))
          (add-before 'configure 'setenv
            (lambda _
              (setenv "AR" "tcc -ar")
              (setenv "CC" "tcc")
              (setenv "CPP" "tcc -E")
              (setenv "LD" "tcc")
              (setenv "ac_cv_prog_cc_c99" "-std=c99")
              (setenv "gl_cv_posix_shell" "bash")
              #t))))))))

(define-public tcc-boot
  (package-with-bootstrap-guile
   (package
     (inherit tcc-boot0)
     (name "tcc-boot")
     (version "0.9.27")
     ;; Upstream only provides bz2 -- Look, Ma how many wonderful
     ;; non-bootstrappable compressors we can choose from!
     (source (origin
               (inherit (package-source tcc))
               ;; patches needs XZ
               ;; (patches (search-patches "tcc-boot-0.9.27.patch"))
               ))
     (build-system gnu-build-system)
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("boot-patch" ,(search-patch "tcc-boot-0.9.27.patch"))
                      ("bash" ,bash-mesboot0)
                      ("bzip2" ,bzip2-mesboot0)
                      ("make" ,make-mesboot0)
                      ("mes" ,mes-boot)
                      ("patch" ,patch-mesboot0)
                      ("tar" ,tar-mesboot0)
                      ("tcc" ,tcc-boot0)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:validate-runpath? #f ; no dynamic executables
        #:strip-binaries? #f            ; no strip yet
        #:phases
        (modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (with-output-to-file "tarball"
                (lambda _ (invoke "bzip2" "-dc" source)))
              (invoke "tar" "xvf" "tarball")
              ;;(chdir (first-subdirectory "."))
              (chdir (string-append "tcc-" ,version))
              #t))
          (add-after 'unpack 'apply-boot-patch
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((patch-file (assoc-ref inputs "boot-patch")))
                (invoke "patch" "-p1" "-i" patch-file))))
          (replace 'configure
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref %outputs "out"))
                     (coreutils (assoc-ref %build-inputs "coreutils"))
                     (mes (assoc-ref %build-inputs "mes"))
                     (tcc (assoc-ref %build-inputs "tcc"))
                     (libc (assoc-ref %build-inputs "libc"))
                     (interpreter (if libc
                                      ;; also for x86_64-linux, we are still on i686-linux
                                      (string-append libc ,(glibc-dynamic-linker "i686-linux"))
                                      (string-append mes "/lib/mes-loader"))))
                (invoke "sh" "configure"
                        (string-append "--cc=tcc")
                        (string-append "--cpu=i386")
                        (string-append "--prefix=" out)
                        (string-append "--elfinterp=" interpreter)
                        (string-append "--crtprefix=" tcc "/lib")
                        (string-append "--sysincludepaths=" tcc "/include")
                        (string-append "--libpaths=" tcc "/lib")))))
          (replace 'build
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref %outputs "out"))
                     (mes (assoc-ref %build-inputs "mes"))
                     (tcc (assoc-ref %build-inputs "tcc"))
                     (libc (assoc-ref %build-inputs "libc"))
                     (interpreter (if libc
                                      ;; also for x86_64-linux, we are still on i686-linux
                                      (string-append libc ,(glibc-dynamic-linker "i686-linux"))
                                      (string-append mes "/lib/mes-loader"))))
                (invoke "tcc"
                        "-vvv"
                        "-D" "BOOTSTRAP=1"
                        "-D" "ONE_SOURCE=1"
                        "-D" "TCC_TARGET_I386=1"
                        "-D" "CONFIG_TCC_STATIC=1"
                        "-D" "CONFIG_USE_LIBGCC=1"
                        "-D" (string-append "CONFIG_TCCDIR=\"" out "/lib/tcc\"")
                        "-D" (string-append "CONFIG_TCC_CRTPREFIX=\"" out "/lib:{B}/lib:.\"")
                        "-D" (string-append "CONFIG_TCC_CRTPREFIX=\"" out "/lib:{B}/lib:.\"")
                        "-D" (string-append "CONFIG_TCC_ELFINTERP=\"" interpreter "\"")
                        "-D" (string-append "CONFIG_TCC_LIBPATHS=\"" tcc "/lib:{B}/lib:.\"")
                        "-D" (string-append "CONFIG_TCC_SYSINCLUDEPATHS=\"" tcc "/include" ":/include:{B}/include\"")
                        "-D" (string-append "TCC_LIBGCC=\"" tcc "/lib/libc.a\"")
                        "-o" "tcc"
                        "tcc.c"))))
          (replace 'check
            (lambda _
              ;; FIXME: add sensible check target (without depending on make)
              ;; ./check.sh ?
              (= 1 (status:exit-val (system* "./tcc" "--help")))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref %outputs "out"))
                    (tcc (assoc-ref %build-inputs "tcc")))
                (and
                 (mkdir-p (string-append out "/bin"))
                 (copy-file "tcc" (string-append out "/bin/tcc"))
                 (mkdir-p (string-append out "/lib/tcc"))
                 (copy-recursively (string-append tcc "/include")
                                   (string-append out "/include"))
                 (copy-recursively (string-append tcc "/lib")
                                   (string-append out "/lib"))
                 (invoke "tcc" "-D" "TCC_TARGET_I386=1" "-c" "-o" "libtcc1.o" "lib/libtcc1.c")
                 (invoke "tcc" "-ar" "rc" "libtcc1.a" "libtcc1.o")
                 (copy-file "libtcc1.a" (string-append out "/lib/libtcc1.a"))
                 (delete-file (string-append out "/lib/tcc/libtcc1.a"))
                 (copy-file "libtcc1.a" (string-append out "/lib/tcc/libtcc1.a"))
                 #t))))))))))

(define-public diffutils-mesboot0
  (package-with-bootstrap-guile
   (package
     (inherit diffutils)
     (name "diffutils-mesboot0")
     (version "2.7")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/diffutils/diffutils-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "1mirn5i825bn5w7rh6mgn0r8aj9xqanav95dwcl1b8sn82f4iwnm"))))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("bash" ,bash-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("make" ,make-mesboot0)
                      ("sed" ,sed-mesboot0)
                      ("tar" ,tar-mesboot0)
                      ("tcc" ,tcc-boot)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:configure-flags '("CC=tcc"
                            "LD=tcc")
        #:parallel-build? #f
        #:tests? #f           ; check is naive, also checks non-built PROGRAMS
        #:strip-binaries? #f  ; no strip yet
        #:phases
        (modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "zxvf" source)
              ;;(chdir (first-subdirectory "."))
              (chdir (string-append "diffutils-" ,version))
              #t))
          (add-before 'configure 'remove-diff3-sdiff
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "Makefile.in"
                (("PROGRAMS = .*" all) "PROGRAMS = cmp diff"))))
          ;; diffutils-2.7 needs a more traditional configure
          (replace 'configure
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bash (assoc-ref %build-inputs "bash"))
                     (shell (string-append bash "/bin/bash")))
                (setenv "CONFIG_SHELL" shell)
                (setenv "CC" "tcc")
                (setenv "LD" "tcc")
                (invoke "./configure" (string-append "--prefix=" out)))))
          (add-after 'configure 'configure-fixups
            (lambda _
              (let* ((out (assoc-ref %outputs "out"))
                     (localedir (string-append out "/share/locale")))
                (substitute* "config.h"
                  (("^/[*] #undef (HAVE_(ALLOCA_H|DIRENT_H|FCNTL_H|STRING_H|UNISTD_H)) [*]/" all have)
                   (string-append "#define " have " 1")))
                #t)))))))))

(define-public binutils-mesboot0
  (package-with-bootstrap-guile
   (package
     (inherit binutils)
     (name "binutils-mesboot0")
     (version "2.14")
     (source (origin
               (method url-fetch)
               ;; Upstream only provides bz2 -- Look, Ma how many wonderful
               ;; non-bootstrappable compressors we can choose from!
               (uri (string-append "mirror://gnu/binutils/binutils-"
                                   version ".tar.gz"))
               ;; Patch needs XZ
               ;; (patches (search-patches "binutils-boot-2.20.1a.patch"))
               (sha256
                (base32
                 "1w8xp7k44bkijr974x9918i4p1sw4g2fcd5mxvspkjpg38m214ds"))))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("bash" ,bash-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("make" ,make-mesboot0)
                      ("mes" ,mes-boot)
                      ("patch" ,patch-mesboot0)
                      ("sed" ,sed-mesboot0)
                      ("tar" ,tar-mesboot0)
                      ("tcc" ,tcc-boot0)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:tests? #f                     ; runtest: command not found
        #:parallel-build? #f
        #:strip-binaries? #f            ; no strip yet
        #:configure-flags
        (let ((out (assoc-ref %outputs "out"))
              (cppflags (string-append " -D __GLIBC_MINOR__=6"
                                       " -D MES_BOOTSTRAP=1"))
              (bash (assoc-ref %build-inputs "bash")))
          `("--disable-nls"
            "--disable-shared"
            "--disable-werror"
            "--build=i386-unknown-linux"
            "--host=i386-unknown-linux"
            "--target=i386-unknown-linux"
            "--with-sysroot=/"
            ,(string-append "--prefix=" out)))
        #:phases
        (modify-phases %standard-phases
          (add-before 'configure 'setenv
            (lambda _
              (let* ((out (assoc-ref %outputs "out"))
                     (bash (assoc-ref %build-inputs "bash"))
                     (shell (string-append bash "/bin/bash"))
                     (cppflags (string-append " -D __GLIBC_MINOR__=6"
                                              " -D MES_BOOTSTRAP=1")))
                (setenv "CONFIG_SHELL" shell)
                (setenv "SHELL" shell)
                (setenv "AR" "tcc -ar")
                (setenv "RANLIB" "true")
                (setenv "CC" (string-append "tcc" cppflags))
                #t)))
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "zxvf" source)
              ;;(chdir (first-subdirectory "."))
              (chdir (string-append "binutils-" ,version))
              #t))
          (add-after 'unpack 'scripted-patch
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Must use Gash' sed in top configure to avoid Mes C Lib bug
              ;; (let* ((gash (assoc-ref %build-inputs "gash"))
              ;;        (sed (string-append gash "/bin/sed")))
              ;;   (substitute* "configure"
              ;;     (("( |\|)sed " all sep) (string-append sep sed " "))))

              (substitute* "intl/configure"
                ((" ln ") " cp "))
              (substitute* "bfd/configure"
                (("^sed -e '/SRC-POTFILES.*" all)
                 "echo -e 'all:\\n\\ttrue\\n\\ninstall:\\n\\ttrue\\n' > po/Makefile\n"))
              #t))
          ;; binutils-2.14 needs a more classic invocation of configure
          ;; configure: warning: CONFIG_SHELL=/gnu/store/28ziymi92vqbw6sh80f0s4frbls1s4zp-bash-mesboot0-2.05b/bin/bash: invalid host type
          ;; configure: warning: SHELL=/gnu/store/28ziymi92vqbw6sh80f0s4frbls1s4zp-bash-mesboot0-2.05b/bin/bash: invalid host type
          ;; configure: error: can only configure for one host and one target at a time
          (replace 'configure
            (lambda* (#:key configure-flags #:allow-other-keys)
              (format (current-error-port)
                      "running ./configure ~a\n" (string-join configure-flags))
              ;; sed: Couldn't open file .//dev/null
              ;; (apply invoke "./configure" configure-flags)

              (apply system* "./configure" configure-flags)
              (substitute* "config.status"
                (("[.]//dev/null") "/dev/null"))
              (invoke "sh" "./config.status")))))))))

(define-public gcc-core-mesboot0
  ;; Gcc-2.95.3 is the most recent GCC that is supported by what the Mes C
  ;; Library v0.16 offers.  Gcc-3.x (and 4.x) place higher demands on a C
  ;; library, such as dir.h/struct DIR/readdir, locales, signals...  Also,
  ;; with gcc-2.95.3, binutils-boot-2.20.1a and glibc-2.2.5 we found a GNU
  ;; toolchain triplet "that works".
  (package-with-bootstrap-guile
   (package
     (inherit gcc)
     (name "gcc-core-mesboot0")
     (version "2.95.3")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/gcc/gcc-2.95.3/gcc-core-"
                                   version
                                   ".tar.gz"))
               ;; Patch needs XZ
               ;; (patches (search-patches "gcc-boot-2.95.3.patch"))
               (sha256
                (base32
                 "1xvfy4pqhrd5v2cv8lzf63iqg92k09g6z9n2ah6ndd4h17k1x0an"))))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("boot-patch" ,(search-patch "gcc-boot-2.95.3.patch"))
                      ("binutils" ,binutils-mesboot0)
                      ("bash" ,bash-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("make" ,make-mesboot0)
                      ("mes" ,mes-boot)
                      ("tcc" ,tcc-boot)
                      ("patch" ,patch-mesboot0)
                      ("sed" ,sed-mesboot0)
                      ("tar" ,tar-mesboot0)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)))
     (outputs '("out"))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:tests? #f
        #:parallel-build? #f
        #:strip-binaries? #f
        #:configure-flags
        (let ((out (assoc-ref %outputs "out")))
          `("--enable-static"
            "--disable-shared"
            "--disable-werror"
            "--build=i686-unknown-linux-gnu"
            "--host=i686-unknown-linux-gnu"
            ,(string-append "--prefix=" out)))
        #:make-flags
        `("CC=tcc -static -D __GLIBC_MINOR__=6"
          "OLDCC=tcc -static -D __GLIBC_MINOR__=6"
          "CC_FOR_BUILD=tcc -static -D __GLIBC_MINOR__=6"
          "AR=ar"
          "RANLIB=ranlib"
          ,(string-append "LIBGCC2_INCLUDES=-I "
                          (assoc-ref %build-inputs "tcc")
                          "/include")
          "LANGUAGES=c"
          ,(string-append "BOOT_LDFLAGS="
                          " -B" (assoc-ref %build-inputs "tcc")
                          "/lib/"))
        #:modules ((guix build gnu-build-system)
                   (guix build utils)
                   (srfi srfi-1))
        #:phases
        (modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "zxvf" source)
              ;;(chdir (first-subdirectory "."))
              (chdir (string-append "gcc-" ,version))
              #t))
          (add-after 'unpack 'apply-boot-patch
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((patch-file (assoc-ref inputs "boot-patch")))
                (system* "patch" "--force" "-p1" "-i" patch-file)
                #t)))
          (add-before 'configure 'setenv
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bash (assoc-ref %build-inputs "bash"))
                     (shell (string-append bash "/bin/bash"))
                     (tcc (assoc-ref %build-inputs "tcc"))
                     (cppflags " -D __GLIBC_MINOR__=6"))
                (setenv "CONFIG_SHELL" shell)
                (setenv "CPPFLAGS" cppflags)
                (setenv "CC" (string-append "tcc" cppflags))
                (setenv "CC_FOR_BUILD" (string-append "tcc" cppflags))
                (setenv "CPP" (string-append "tcc -E" cppflags))
                (with-output-to-file "config.cache"
                  (lambda _
                    (display "
ac_cv_c_float_format='IEEE (little-endian)'
"))))))
          ;; gcc-2.95.3 needs a more traditional configure
          (replace 'configure
            (lambda* (#:key configure-flags  #:allow-other-keys)
              (format (current-error-port)
                      "running ./configure ~a\n" (string-join configure-flags))
              (apply invoke "./configure" configure-flags)))
          (add-after 'configure 'remove-info
            (lambda _
              ;; no info at this stage
              (delete-file-recursively "texinfo")
              (invoke "touch" "gcc/cpp.info" "gcc/gcc.info")))
          (add-after 'install 'install2
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((tcc (assoc-ref %build-inputs "tcc"))
                     (tcc-lib (string-append tcc "/lib/x86-mes-gcc"))
                     (out (assoc-ref outputs "out"))
                     (gcc-dir (string-append
                               out "/lib/gcc-lib/i686-unknown-linux-gnu/2.95.3")))
                (and
                 (mkdir-p "tmp")
                 (zero? (system (string-append "set -x; cd tmp && ar x ../gcc/libgcc2.a")))
                 (zero? (system (string-append "set -x; cd tmp && ar r " gcc-dir "/libgcc.a *.o")))
                 (copy-file "gcc/libgcc2.a" (string-append out "/lib/libgcc2.a"))
                 (copy-file (string-append tcc "/lib/libtcc1.a")
                            (string-append out "/lib/libtcc1.a"))
                 (invoke "ar" "r" (string-append gcc-dir "/libc.a")
                         (string-append tcc-lib "/libc+gnu.o")
                         (string-append tcc-lib "/libtcc1.o"))
                 (invoke "ar" "r" (string-append out "/lib/libc.a")
                         (string-append tcc-lib "/libc+gnu.o")
                         (string-append tcc-lib "/libtcc1.o"))
                 (invoke "ls" "-ltrF" gcc-dir)
                 (copy-recursively (string-append tcc "/include")
                                   (string-append out "/include"))
                 #t)))))))
     (native-search-paths
      ;; Use the language-specific variables rather than 'CPATH' because they
      ;; are equivalent to '-isystem' whereas 'CPATH' is equivalent to '-I'.
      ;; The intent is to allow headers that are in the search path to be
      ;; treated as "system headers" (headers exempt from warnings) just like
      ;; the typical /usr/include headers on an FHS system.
      (list (search-path-specification
             (variable "C_INCLUDE_PATH")
             (files '("include" "/lib/gcc-lib/i686-unknown-linux-gnu/2.95.3/include")))
            (search-path-specification
             (variable "LIBRARY_PATH")
             (files '("lib"))))))))

(define-public mesboot-headers
  (package-with-bootstrap-guile
   (package
     (inherit mes-boot)
     (name "mesboot-headers")
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("headers" ,%bootstrap-linux-libre-headers)

                      ("gash" ,%bootstrap-gash+coreutils)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:tests? #f
        #:strip-binaries? #f
        #:phases
        (modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (include (string-append out "/include"))
                     (headers (assoc-ref %build-inputs "headers" )))
                (mkdir-p include)
                (copy-recursively "include" out)
                (copy-recursively headers out)
                #t))))))
     (native-search-paths
      ;; Use the language-specific variables rather than 'CPATH' because they
      ;; are equivalent to '-isystem' whereas 'CPATH' is equivalent to '-I'.
      ;; The intent is to allow headers that are in the search path to be
      ;; treated as "system headers" (headers exempt from warnings) just like
      ;; the typical /usr/include headers on an FHS system.
      (list (search-path-specification
             (variable "C_INCLUDE_PATH")
             (files '("include"))))))))

(define-public glibc-mesboot0
  ;; GNU C Library 2.2.5 is the most recent glibc that we managed to build
  ;; using gcc-2.95.3.  Newer versions (2.3.x, 2.6, 2.1x) seem to need a newer
  ;; gcc.
  (package-with-bootstrap-guile
   (package
     (inherit glibc)
     (name "glibc-mesboot0")
     (version "2.2.5")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/glibc/glibc-"
                                   version
                                   ".tar.gz"))
               ;; Patch needs XZ
               ;; (patches (search-patches "glibc-boot-2.2.5.patch"))
               (sha256
                (base32
                 "1vl48i16gx6h68whjyhgnn1s57vqq32f9ygfa2fls7pdkbsqvp2q"))))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("boot-patch" ,(search-patch "glibc-boot-2.2.5.patch"))
                      ("system-patch" ,(search-patch "glibc-bootstrap-system-2.2.5.patch"))
                      ("binutils" ,binutils-mesboot0)
                      ("bash" ,bash-mesboot1)
                      ;; posix/uname.c gets compiled where it shouldn't
                      ;; ../sysdeps/generic/uname.c:25: config-name.h: error 02
                      ;; ("bash" ,bash-mesboot0)
                      ("diffutils" ,diffutils-mesboot0)
                      ("gawk" ,gawk-mesboot0)
                      ("gcc" ,gcc-core-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("headers" ,mesboot-headers)
                      ("make" ,make-mesboot0)
                      ("mes" ,mes-boot)
                      ("tcc" ,tcc-boot)
                      ("patch" ,patch-mesboot0)
                      ("sed" ,sed-mesboot0)
                      ("tar" ,tar-mesboot0)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)))
     (outputs '("out"))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:tests? #f
        #:strip-binaries? #f
        #:validate-runpath? #f ; no dynamic executables
        #:parallel-build? #f    ; gcc-2.95.3 ICEs on massively parallel builds
        #:make-flags (list (string-append
                            "SHELL="
                            (assoc-ref %build-inputs "bash")
                            "/bin/sh"))
        #:configure-flags
        (let ((out (assoc-ref %outputs "out"))
              (headers (assoc-ref %build-inputs "headers")))
          `("--disable-shared"
            "--enable-static"
            "--disable-sanity-checks"
            "--build=i686-unknown-linux-gnu"
            "--host=i686-unknown-linux-gnu"
            ,(string-append "--with-headers=" headers "/include")
            "--enable-static-nss"
            "--without-__thread"
            "--without-cvs"
            "--without-gd"
            "--without-tls"
            ,(string-append "--prefix=" out)))
        #:phases
        (modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "zxvf" source)
              ;;(chdir (first-subdirectory "."))
              (chdir (string-append "glibc-" ,version))
              #t))
          (add-after 'unpack 'apply-boot-patch
            (lambda* (#:key inputs #:allow-other-keys)
              (and (let ((patch (assoc-ref inputs "boot-patch")))
                     (invoke "patch" "--force" "-p1" "-i" patch))
                   (let ((patch (assoc-ref inputs "system-patch")))
                     (invoke "patch" "--force" "-p1" "-i" patch)))))
          (add-before 'configure 'setenv
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bash (assoc-ref %build-inputs "bash"))
                     (shell (string-append bash "/bin/bash"))
                     (gcc (assoc-ref %build-inputs "gcc"))
                     (headers (assoc-ref %build-inputs "headers"))
                     (cppflags (string-append
                                ;;" -D __STDC__=1"
                                " -D MES_BOOTSTRAP=1"
                                " -D BOOTSTRAP_GLIBC=1"))
                     (cflags (string-append " -L " (getcwd))))
                (setenv "CONFIG_SHELL" shell)
                (setenv "SHELL" shell)
                (setenv "CPP" (string-append gcc "/bin/gcc -E " cppflags))
                (setenv "CC" (string-append gcc "/bin/gcc " cppflags cflags))
                #t)))
          ;; glibc-2.2.5 needs a more classic invocation of configure
          ;; configure: warning: CONFIG_SHELL=/gnu/store/kpxi8h3669afr9r1bgvaf9ij3y4wdyyn-bash-minimal-4.4.12/bin/bash: invalid host type
          ;; configure: error: *** A pwd binary could not be found.
          (replace 'configure
            (lambda* (#:key configure-flags #:allow-other-keys)
              (format (current-error-port)
                      "running ./configure ~a\n" (string-join configure-flags))
              (apply invoke "./configure" configure-flags)))
          (add-after 'configure 'fixup-configure
            (lambda _
              (substitute* "config.make"
                (("INSTALL = scripts/") "INSTALL = $(..)./scripts/"))
              #t)))))
     (native-search-paths
      ;; Use the language-specific variables rather than 'CPATH' because they
      ;; are equivalent to '-isystem' whereas 'CPATH' is equivalent to '-I'.
      ;; The intent is to allow headers that are in the search path to be
      ;; treated as "system headers" (headers exempt from warnings) just like
      ;; the typical /usr/include headers on an FHS system.
      (list (search-path-specification
             (variable "C_INCLUDE_PATH")
             (files '("include")))
            (search-path-specification
             (variable "CPLUS_INCLUDE_PATH")
             (files '("include")))
            (search-path-specification
             (variable "LIBRARY_PATH")
             (files '("lib"))))))))

(define gcc-mesboot0
  (package-with-bootstrap-guile
   (package
     (inherit gcc-core-mesboot0)
     (name "gcc-mesboot0")

     (native-inputs `(("boot-patch" ,(search-patch "gcc-boot-2.95.3.patch"))
                      ("binutils" ,binutils-mesboot0)
                      ("bash" ,bash-mesboot0)
                      ("gcc" ,gcc-core-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("libc" ,glibc-mesboot0)
                      ("make" ,make-mesboot0)
                      ("patch" ,patch-mesboot0)
                      ("sed" ,sed-mesboot0)
                      ("tar" ,tar-mesboot0)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)))

     (arguments
      (substitute-keyword-arguments (package-arguments gcc-core-mesboot0)
        ((#:phases phases)
         `(modify-phases ,phases
            (replace 'setenv
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out"))
                      (bash (assoc-ref %build-inputs "bash"))
                      (gcc (assoc-ref %build-inputs "gcc"))
                      (glibc (assoc-ref %build-inputs "libc"))
                      (kernel-headers (assoc-ref %build-inputs "kernel-headers")))
                  (setenv "CONFIG_SHELL" (string-append bash "/bin/sh"))
                  (format (current-error-port) "C_INCLUDE_PATH=~a\n" (getenv "C_INCLUDE_PATH"))
                  (setenv "C_INCLUDE_PATH" (string-append
                                            gcc "/lib/gcc-lib/i686-unknown-linux-gnu/2.95.3/include"
                                            ":" kernel-headers "/include"
                                            ":" glibc "/include"))
                  (format (current-error-port) "C_INCLUDE_PATH=~a\n" (getenv "C_INCLUDE_PATH"))
                  (format (current-error-port) "LIBRARY_PATH=~a\n" (getenv "LIBRARY_PATH"))
                  ;; FIXME: add glibc dirs to paths manually
                  (setenv "LIBRARY_PATH" (string-join
                                          (list (string-append glibc "/lib")
                                                (getenv "LIBRARY_PATH"))
                                          ":"))
                  (format (current-error-port) "LIBRARY_PATH=~a\n" (getenv "LIBRARY_PATH"))
                  (with-output-to-file "config.cache"
                    (lambda _
                      (display "
ac_cv_c_float_format='IEEE (little-endian)'
")))
                  #t)))
            (replace 'install2
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (gcc-dir (string-append
                                 out "/lib/gcc-lib/i686-unknown-linux-gnu/2.95.3")))
                  (and
                   (mkdir-p "tmp")
                   (zero? (system (string-append "set -x; cd tmp && ar x ../gcc/libgcc2.a")))
                   (zero? (system (string-append "set -x; cd tmp && ar r " gcc-dir "/libgcc.a *.o")))
                   (copy-file "gcc/libgcc2.a" (string-append out "/lib/libgcc2.a"))))))))
        ((#:configure-flags configure-flags)
         `(let ((out (assoc-ref %outputs "out")))
            `("--disable-shared"
              "--disable-werror"
              "--build=i686-unknown-linux-gnu"
              "--host=i686-unknown-linux-gnu"
              ,(string-append "--prefix=" out))))
        ((#:make-flags make-flags)
         `(let ((gcc (assoc-ref %build-inputs "gcc")))
            `("RANLIB=true"
              ,(string-append "LIBGCC2_INCLUDES=-I " gcc "/include")
              "LANGUAGES=c"))))))))

;; version 1.23 -
;; * New command line option '--warning' [ => `implausibly old timestamp' > /dev/null ]

;; version 1.22 - Sergey Poznyakoff, 2009-03-05

;; * Support for xz compression

(define-public tar-mesboot
  (package-with-bootstrap-guile
   (package
     (inherit tar)
     (name "tar-mesboot")
     ;; backupfile.c: In function `numbered_backup':
     ;; backupfile.c:253: parse error before `new_buffer_size'
     ;; (version  "1.30")
     ;; (source (origin
     ;;           (method url-fetch)
     ;;           (uri (string-append
     ;;                 "http://lilypond.org/janneke/mes/"
     ;;                 "tar" "-" version ".tar"))
     ;;           (sha256
     ;;            (base32
     ;;             "04p99wl19wan9h726y9p3ysnp4cm3ndn2iplvnwqalv5767vafpd"))))
     (version  "1.22")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/tar/tar-"
                                   version
                                   ".tar.gz"))
               (sha256
                (base32
                 "19nvix64y95n5v6rr5g9g3fn08zz85cb5anzd7csfv4a4sz9lw4y"))))
     ;; (version  "1.23")
     ;; (source (origin
     ;;           (method url-fetch)
     ;;           (uri (string-append
     ;;                 "http://lilypond.org/janneke/mes/"
     ;;                 "tar" "-" version ".tar"))
     ;;           (sha256
     ;;            (base32
     ;;             "0dnni4awcfzbdhkxlvh90z5qqvxr0dcc4ij537cs2g9v9pd81hnb"))))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("binutils" ,binutils-mesboot0)
                      ("bash" ,bash-mesboot0)
                      ("gawk" ,gawk-mesboot0)
                      ("gcc" ,gcc-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("libc" ,glibc-mesboot0)
                      ("make" ,make-mesboot0)
                      ("patch" ,patch-mesboot0)
                      ("tar" ,tar-mesboot0)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:parallel-build? #f
        #:tests? #f           ; check is naive, also checks non-built PROGRAMS
        #:strip-binaries? #f  ; no strip yet
        #:configure-flags '("--build=i686-unknown-linux-gnu"
                            "--host=i686-unknown-linux-gnu"
                            "--disable-nls"
                            "LIBS=-lc -lnss_files -lnss_dns -lresolv")
        #:phases
        (modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "zxvf" source)
              ;;(chdir (first-subdirectory "."))
              (chdir (string-append "tar-" ,version))
              #t))
          (replace 'install
            (lambda _
              (let* ((out (assoc-ref %outputs "out"))
                     (bin (string-append out "/bin")))
                (install-file "src/tar" bin)
                #t)))))))))

(define-public grep-mesboot
  (package-with-bootstrap-guile
   (package
     (inherit grep-mesboot0)
     (name "grep-mesboot")
     (version "2.0")
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("binutils" ,binutils-mesboot0)
                      ("bash" ,bash-mesboot0)
                      ;; ("gawk" ,gawk-mesboot0) ;; Gash awk: !~ : scriptgen.awk
                      ("gcc" ,gcc-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("libc" ,glibc-mesboot0)
                      ("make" ,make-mesboot0)
                      ("patch" ,patch-mesboot0)
                      ("sed" ,sed-mesboot0)
                      ("tar" ,tar-mesboot)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:make-flags '("grep") ; target `all' includes check, which needs Awk
        #:parallel-build? #f
        #:phases
        (modify-phases %standard-phases
          (add-before 'configure 'patch-configure
            (lambda _
              (let* ((bash (assoc-ref %build-inputs "bash"))
                     (shell (string-append bash "/bin/bash")))
                (substitute* "configure"
                  ;; +./config.status
                  ;; Backtrace:
                  ;;            8 (apply-smob/1 #<catch-closure 24e21c0>)
                  ;; In ice-9/boot-9.scm:
                  ;;     705:2  7 (call-with-prompt _ _ #<procedure default-prompt-handler (k proc)>)
                  ;; In ice-9/eval.scm:
                  ;;     619:8  6 (_ #(#(#<directory (guile-user) 2576140>)))
                  ;; In ice-9/ports.scm:
                  ;;    444:17  5 (call-with-input-file _ _ #:binary _ #:encoding _ #:guess-encoding _)
                  ;; In gash/gash.scm:
                  ;;    152:27  4 (_ _)
                  ;; In gash/repl.scm:
                  ;;     38:13  3 (run-repl _ _)
                  ;; In gash/shell.scm:
                  ;;     333:4  2 (sh:or _ _)
                  ;;     136:6  1 (sh:exec-let _ _ . _)
                  ;; In unknown file:
                  ;;            0 (execle "./config.status" ("GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt" "_=/gnu/store/…" …) …)
                  ;; ERROR: In procedure execle:
                  ;; In procedure execle: Text file busy
                  ((" [|][|] ./config.status") " || sh ./config.status"))

                )))

          ;; gcc -O -DGREP  -DSTDC_HEADERS=1 -DHAVE_string_h=1 -DHAVE_sys_pArAm_h=1 -DHAVE_UNISTD_H=1 -DHAVE_ALLOCA_H=1 -DHAVE_getpAgesiZe=1 -DHAVE_memchr=1 -DHAVE_strerror=1 -DHAVE_vAlloc=1 -DHAVE_WORKING_MMAP=1 -I. -c grep.c
          (add-after 'configure 'fixup-configure
            (lambda _
              (substitute* "Makefile"
                (("HAVE_getpAgesiZe") "HAVE_GETPAGESIZE")
                (("HAVE_memchr") "HAVE_MEMCHR")
                (("HAVE_memcpy") "HAVE_MEMCPY")
                (("HAVE_strerror") "HAVE_STRERROR")
                (("HAVE_string_h") "HAVE_STRING_H")
                (("HAVE_sys_pArAm_h") "HAVE_SYSPARAM_H")
                (("HAVE_vAlloc") "HAVE_VALLOC"))
              #t))

          (replace 'check               ; needs Awk
            (lambda _
              (invoke "./grep" "GNU" "README")))
          (replace 'install             ; target `install' ensures `check'
            (lambda _
              (let* ((out (assoc-ref %outputs "out"))
                     (bin (string-append out "/bin")))
                (install-file "grep" bin)
                (symlink "grep" (string-append bin "/egrep"))
                (symlink "grep" (string-append bin "/fgrep"))
                #t)))))))))

(define-public bash-mesboot
  (package-with-bootstrap-guile
   (package
     (inherit bash-mesboot1)
     (version "4.4")
     (name "bash-mesboot")
     ;; (source (origin
     ;;           (method url-fetch)
     ;;           (uri (string-append "mirror://gnu/bash/bash-"
     ;;                               version ".tar.gz"))
     ;;           (sha256
     ;;            (base32
     ;;             "1jyz6snd63xjn6skk7za6psgidsd53k05cr3lksqybi0q6936syq"))))
     ;; (source (origin
     ;;           (method url-fetch)
     ;;           (uri (string-append
     ;;                 "http://lilypond.org/janneke/mes/"
     ;;                 "bash-4.4.tar"))
     ;;           (sha256
     ;;            (base32
     ;;             "1p0ixbf04bbbdrzsd1y1ibzvk3iwwihmrkfh6irwqcaz93xi7rxb"))))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("bash" ,bash-mesboot0)
                      ("binutils" ,binutils-mesboot0)
                      ("gawk" ,gawk-mesboot0)
                      ("gcc" ,gcc-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("libc" ,glibc-mesboot0)
                      ("make" ,make-mesboot0)
                      ("patch" ,patch-mesboot0)
                      ("sed" ,sed-mesboot0)
                      ("tar" ,tar-mesboot)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)))
     (outputs '("out"))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:parallel-build? #f
        #:configure-flags '("--build=i686-unknown-linux-gnu"
                            "--host=i686-unknown-linux-gnu"

                            "--without-bash-malloc"
                            "--disable-readline"
                            "--disable-history"
                            "--disable-help-builtin"
                            "--disable-progcomp"
                            "--disable-net-redirections"
                            "--disable-nls"

                            "AWK=gawk"
                            "LIBS=-lc -lnss_files -lnss_dns -lresolv"
                            "gl_cv_func_rename_dest_works=yes"
                            ;; Pretend 'dlopen' is missing so we don't build loadable
                            ;; modules and related code.
                            "ac_cv_func_dlopen=no")
        #:make-flags '("bash")
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'scripted-patch
            (lambda _
              (substitute* "shell.c"
                ((";;") ";"))
              #t))
          (add-after 'configure 'configure-fixups
            (lambda _
              (let ((config.h (open-file "config.h" "a")))
                (display (string-append "
// tcc: error: undefined symbol 'enable_hostname_completion'
#define enable_hostname_completion(on_or_off) 0

// /gnu/store/cq0cmv35s9dhilx14zaghlc08gpc0hwr-tcc-boot0-0.9.26-6.c004e9a/lib/libc.a: error: 'sigprocmask' defined twice
#define HAVE_POSIX_SIGNALS 1
")
                         config.h)
                (close config.h))
              #t))
          (replace 'check
            (lambda _
              (invoke "./bash" "--version")))
          (replace 'install
            (lambda _
              (let* ((out (assoc-ref %outputs "out"))
                     (bin (string-append out "/bin")))
                (mkdir-p bin)
                (copy-file "bash" (string-append bin "/bash"))
                (copy-file "bash" (string-append bin "/sh"))
                #t)))))))))

(define binutils-mesboot1
  (package-with-bootstrap-guile
   (package
     (inherit binutils-mesboot0)
     (name "binutils-mesboot1")
     (native-inputs `(("binutils" ,binutils-mesboot0)
                      ("bash" ,bash-mesboot)
                      ("gcc" ,gcc-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("libc" ,glibc-mesboot0)
                      ("make" ,make-mesboot0)
                      ("patch" ,patch-mesboot0)
                      ("sed" ,sed-mesboot0)
                      ("tar" ,tar-mesboot0)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)))
     (arguments
      (substitute-keyword-arguments (package-arguments binutils-mesboot0)
        ((#:configure-flags configure-flags)
         '(let ((out (assoc-ref %outputs "out")))
            `("--disable-nls"
              "--disable-shared"
              "--disable-werror"
              "--build=i686-unknown-linux-gnu"
              "--host=i686-unknown-linux-gnu"
              "--with-sysroot=/"
              ,(string-append "--prefix=" out))))
        ((#:phases phases)
         `(modify-phases ,phases
            (replace 'setenv
              (lambda _
                (let* ((out (assoc-ref %outputs "out"))
                       (bash (assoc-ref %build-inputs "bash"))
                       (shell (string-append bash "/bin/bash")))
                  (setenv "CONFIG_SHELL" shell)
                  #t))))))))))

(define-public coreutils-mesboot0
  (package-with-bootstrap-guile
   (package
     (inherit coreutils)
     (name "coreutils-mesboot0")
     ;;(version "8.30")
     ;; most recent .gz release
     ;; (version "8.13")
     ;; randperm.c: In function `sparse_swap':
     ;; randperm.c:117: invalid lvalue in unary `&'
     (version "5.0") ;; 2003-04
     ;; No real bootstrap from source yet: Perl dependency
     ;; WARNING: You don't seem to have perl5.003 or newer installed, or you lack
     ;;          a usable version of the Perl File::Compare module.  As a result,
     ;;          you may be unable to run a few tests or to regenerate certain
     ;;          files if you modify the sources from which they are derived.
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/coreutils/coreutils-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "10wq6k66i8adr4k08p0xmg87ff4ypiazvwzlmi7myib27xgffz62"))))
     (native-inputs `(("bash" ,bash-mesboot)
                      ("binutils" ,binutils-mesboot0)
                      ("diffutils" ,diffutils-mesboot0)
                      ("gawk" ,gawk-mesboot0)
                      ("gcc" ,gcc-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("libc" ,glibc-mesboot0)
                      ("make" ,make-mesboot0)
                      ("sed" ,sed-mesboot0)
                      ("tar" ,tar-mesboot)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (arguments
      `(#:implicit-inputs? #f
        #:tests? #f           ; WARNING: `perl' is needed, ...
        #:strip-binaries? #f  ; strip: unrecognized option `--only-keep-debug'
        #:guile ,%bootstrap-guile
        #:configure-flags
        '("--disable-doc"
          "LIBS=-lc -lnss_files -lnss_dns -lresolv"
          ;; checking whether rename manages existing destinations correctly... Backtrace:
          ;;            4 (apply-smob/1 #<catch-closure 9e5260>)
          ;; In ice-9/boot-9.scm:
          ;;     705:2  3 (call-with-prompt _ _ #<procedure default-prompt-handle?>)
          ;; In ice-9/eval.scm:
          ;;     619:8  2 (_ #(#(#<directory (guile-user) a670a0>)))
          ;; In ice-9/boot-9.scm:
          ;;    260:13  1 (for-each #<procedure mkdir (_ #:optional _)> _)
          ;; In unknown file:
          ;;            0 (mkdir "conftest.d2" #<undefined>)
          ;; ERROR: In procedure mkdir:
          ;; In procedure mkdir: File exists
          ;; configure: error: cannot create temporary files
          "gl_cv_func_rename_dest_works=yes"))))))

(define make-mesboot
  (package-with-bootstrap-guile
   (package
     (inherit gnu-make)
     (name "make-mesboot")
     (version "3.82")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/make/make-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "1rs2f9hmvy3q6zkl15jnlmnpgffm0bhw5ax0h5c7q604wqrip69x"))))
     (native-inputs `(("bash" ,bash-mesboot)
                      ("binutils" ,binutils-mesboot0)
                      ("diffutils" ,diffutils-mesboot0)
                      ("gawk" ,gawk-mesboot0)
                      ("gcc" ,gcc-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("libc" ,glibc-mesboot0)
                      ("make" ,make-mesboot0)
                      ("sed" ,sed-mesboot0)
                      ("tar" ,tar-mesboot)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (arguments
      `(#:implicit-inputs? #f
        #:parallel-build? #f
        #:guile ,%bootstrap-guile
        #:configure-flags '("LIBS=-lc -lnss_files -lnss_dns -lresolv")
        #:phases
        (modify-phases %standard-phases
          (replace 'check
            (lambda _
              (invoke "./make" "--version")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
                (install-file "make" bin)
                #t)))))))))

;; NOT needed..
(define make-mesboot4
  (package-with-bootstrap-guile
   (package
     (inherit gnu-make)
     (name "make-mesboot4")
     (version "4.0")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/make/make-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "0s8z0lr4jzgg6hv1zqwmrbmffk9agvvvm3kq54cjkd6ln2gi6hpw"))))
     (native-inputs `(("bash" ,bash-mesboot)
                      ("binutils" ,binutils-mesboot0)
                      ("diffutils" ,diffutils-mesboot0)
                      ("gawk" ,gawk-mesboot0)
                      ("gcc" ,gcc-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("libc" ,glibc-mesboot0)
                      ("make" ,make-mesboot0)
                      ("sed" ,sed-mesboot0)
                      ("tar" ,tar-mesboot)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (arguments
      `(#:implicit-inputs? #f
        #:parallel-build? #f
        #:guile ,%bootstrap-guile
        #:configure-flags '("LIBS=-lc -lnss_files -lnss_dns -lresolv")
        #:phases
        (modify-phases %standard-phases
          (add-after 'configure 'configure-fixups
            (lambda _
              (let ((config.h (open-file "config.h" "a")))
                (display (string-append "
#define va_copy(dest, src) dest = src
")
                         config.h)
                (close config.h))
              #t))
          (replace 'check
            (lambda _
              (invoke "./make" "--version")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
                (install-file "make" bin)
                #t)))))))))

(define gawk-mesboot1
  (package-with-bootstrap-guile
   (package
     (inherit gawk)
     (name "gawk-mesboot1")
     (version "3.1.3")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/gawk/gawk-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "0nrchvcp395aqcq1ip5bg0wrpicfjqix1wxvwx20nr5fb7lp9f9m"))))
     (native-inputs `(("bash" ,bash-mesboot)
                      ("binutils" ,binutils-mesboot0)
                      ("diffutils" ,diffutils-mesboot0)
                      ("gawk" ,gawk-mesboot0)
                      ("gcc" ,gcc-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("libc" ,glibc-mesboot0)
                      ("make" ,make-mesboot)
                      ("sed" ,sed-mesboot0)
                      ("tar" ,tar-mesboot)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (arguments
      `(#:implicit-inputs? #f
        #:parallel-build? #f
        #:guile ,%bootstrap-guile
        #:configure-flags '("LIBS=-lc -lnss_files -lnss_dns -lresolv")
        #:make-flags '("gawk")
        #:phases
        (modify-phases %standard-phases
          (replace 'check
            (lambda _
              (invoke "./gawk" "--version")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
                (install-file "gawk" bin)
                (symlink "gawk" (string-append bin "/awk"))
                #t)))))))))

(define gawk-mesboot2
  (package-with-bootstrap-guile
   (package
     (inherit gawk)
     (name "gawk-mesboot2")
     (version "3.1.8")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/gawk/gawk-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "03d5y7jabq7p2s7ys9alay9446mm7i5g2wvy8nlicardgb6b6ii1"))))
     (native-inputs `(("bash" ,bash-mesboot)
                      ("binutils" ,binutils-mesboot0)
                      ("diffutils" ,diffutils-mesboot0)
                      ("gawk" ,gawk-mesboot0)
                      ("gcc" ,gcc-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("libc" ,glibc-mesboot0)
                      ("make" ,make-mesboot)
                      ("sed" ,sed-mesboot0)
                      ("tar" ,tar-mesboot)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (arguments
      `(#:implicit-inputs? #f
        #:parallel-build? #f
        #:guile ,%bootstrap-guile
        #:configure-flags '("LIBS=-lc -lnss_files -lnss_dns -lresolv")
        #:make-flags '("gawk")
        #:phases
        (modify-phases %standard-phases
          (replace 'check
            (lambda _
              (invoke "./gawk" "--version")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
                (install-file "gawk" bin)
                (symlink "gawk" (string-append bin "/awk"))
                #t)))))))))

(define sed-mesboot
  (package-with-bootstrap-guile
   (package
     (inherit sed)
     (name "sed-mesboot")
     (version "4.0.6") ; 4.0.6 2003-04
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/sed/sed-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "0861ij94cqc4vaaki6r2wlapwcmhpx4ggp4r70f46mb21a8fkvf1"))))
     ;; (version "4.0.9")
     ;; (source (origin
     ;;           (method url-fetch)
     ;;           (uri (string-append "mirror://gnu/sed/sed-"
     ;;                               version ".tar.gz"))
     ;;           (sha256
     ;;            (base32
     ;;             "0006gk1dw2582xsvgx6y6rzs9zw8b36rhafjwm288zqqji3qfrf3"))))
     (native-inputs `(("bash" ,bash-mesboot)
                      ("binutils" ,binutils-mesboot0)
                      ("gawk" ,gawk-mesboot0)
                      ("gcc" ,gcc-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("libc" ,glibc-mesboot0)
                      ("make" ,make-mesboot)
                      ("sed" ,sed-mesboot0)
                      ("tar" ,tar-mesboot)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (arguments
      `(#:implicit-inputs? #f
        #:parallel-build? #f
        #:guile ,%bootstrap-guile
        #:configure-flags '("LIBS=-lc -lnss_files -lnss_dns -lresolv")
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack '/bin/sh
            (lambda _
              (let* ((bash (assoc-ref %build-inputs "bash"))
                     (shell (string-append bash "/bin/bash")))
                (substitute* "testsuite/Makefile.tests"
                 (("^SHELL = /bin/sh")
                  (string-append "SHELL = " shell)))
                #t)))))))))

(define gmp-boot
  (package
    (inherit gmp)
    (version "4.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gmp/gmp-" version
                                  ".tar.gz"))
              (sha256 (base32
                       "15rwq54fi3s11izas6g985y9jklm3xprfsmym3v1g6xr84bavqvv"))))))

(define mpfr-boot
  (package
    (inherit mpfr)
    (version "2.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/mpfr/mpfr-" version
                                  ".tar.gz"))
              (sha256 (base32
                       "0dxn4904dra50xa22hi047lj8kkpr41d6vb9sd4grca880c7wv94"))))))

(define mpc-boot
  (package
    (inherit mpc)
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnu/mpc/mpc-" version ".tar.gz"))
              (sha256
               (base32
                "1hzci2zrrd7v3g1jk35qindq05hbl0bhjcyyisq9z209xb3fqzb1"))))))

;; This package is not stricly needed, but very helpful for development.  If
;; this configures and builds then gcc-mesboot1 also builds.
(define-public gcc-core-mesboot1
  (package-with-bootstrap-guile
   (package
     (inherit gcc-mesboot0)
     (name "gcc-core-mesboot1")
     (version "4.6.4")
     (source (origin ;;(inherit (package-source gcc-4.7))
               (method url-fetch)
               (uri (string-append "mirror://gnu/gcc/gcc-"
                                   version "/gcc-core-" version ".tar.gz"))
               (sha256
                (base32
                 ;;4.7.4
                 ;;"06bqygv17f71f7g7pi7ddvhljcr8dimrypgiif9g1r6lqn1sbfnx"
                 ;;4.6.4
                 "173kdb188qg79pcz073cj9967rs2vzanyjdjyxy9v0xb0p5sad75"
                 ))
               ;; Patch needs XZ
               ;; (patches (search-patches "gcc-boot-4.7.4.patch"))
               ))
     (inputs `(("gmp-source" ,(package-source gmp-boot))
               ("mpfr-source" ,(package-source mpfr-boot))
               ("mpc-source" ,(package-source mpc-boot))))
     (native-inputs
      `( ;;("boot-patch" ,(search-patch "gcc-boot-4.7.4.patch"))
        ("boot-patch" ,(search-patch "gcc-boot-4.6.4.patch"))

        ;; hack -- which tool is broken?
        ;; Unless using %bootstrap-coreutils&co, or
        ;; all seeds: bash-seed, gawk-seed, grep-seed, sed-seed
        ;; we get configure/build errors like these:

        ;; This error is caused by using either bash-mesboot0 or
        ;; bash-mesboot instead of bash-seed:

        ;;./libtool: line 1: tr: command not found
        ;; ("bash" ,bash-mesboot1)  ;; Breaks the build!
        ;; ("bash" ,bash-mesboot0) ;; Breaks the build!

        ;; This error is caused by using Gash' awk ?? or SED
        ;; Makefile:231: *** missing separator.  Stop.
        ;; @if target-libstdc++-v3-bootstrap

        ;; This error is caused by using a broken awk
        ;; Makefile:520: *** missing separator.  Stop.
        ;; #### host and target specific makefile fragments come in here.
        ;; @target_makefile_frag@
        ;; @alphaieee_frag@
        ;; @ospace_frag@
        ;; @host_makefile_frag@
        ;; ###

        ;; This error is caused by using a broken sed
        ;; gawk: ./conf1444-14670/subs.awk:8: S["LTLIBOBJS"]=
        ;; gawk: ./conf1444-14670/subs.awk:8:                ^ unexpected newline or end of string
        ;; config.status: error: could not create Makefile

        ;; This error is caused by using Gash' sed
        ;; ERROR: In procedure scm-error:
        ;; SED: unsupported address type ("^@if " . "^@endif ")

        ("bash" ,bash-mesboot)
        ;; both broken
        ;; ("bash" ,bash-mesboot1)
        ;; ("bash" ,bash-mesboot0)

        ("binutils" ,binutils-mesboot0)
        ("coreutils" ,coreutils-mesboot0)
        ("diffutils" ,diffutils-mesboot0)

        ("gawk" ,gawk-mesboot0) ; MUST use instead of Gash' awk
        ("gcc" ,gcc-mesboot0)
        ("gzip" ,gzip-mesboot0)
        ;; ("grep" ,grep-mesboot0) ; alternative for Gash' grep
        ("libc" ,glibc-mesboot0)
        ("make" ,make-mesboot)
        ("patch" ,patch-mesboot0)
        ("sed" ,sed-mesboot) ; MUST instead of Gash' sed
        ;; ("sed" ,sed-mesboot0)
        ("tar" ,tar-mesboot) ; MUST use; Gash' tar cannot handle @LongLink (in gcc-g++)

        ("gash" ,%bootstrap-gash+coreutils) ; for grep, [sed, tar]
        ("guile" ,%bootstrap-guile)
        ("kernel-headers" ,%bootstrap-linux-libre-headers)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:tests? #f
        #:modules ((guix build gnu-build-system)
                   (guix build utils)
                   (srfi srfi-1))
        #:parallel-build? #f            ; for debugging
        #:make-flags
        (let* ((libc (assoc-ref %build-inputs "libc"))
               (ldflags (string-append
                         "-B" libc "/lib "
                         "-Wl,-dynamic-linker "
                         "-Wl," libc
                         ,(glibc-dynamic-linker "i686-linux"))))
          (list (string-append "LDFLAGS=" ldflags)
                (string-append "LDFLAGS_FOR_TARGET=" ldflags)))
        #:configure-flags
        (let ((out (assoc-ref %outputs "out"))
              (glibc (assoc-ref %build-inputs "libc")))
          (list (string-append "--prefix=" out)
                "--build=i686-unknown-linux-gnu"
                "--host=i686-unknown-linux-gnu"

                (string-append "--with-native-system-header-dir=" glibc "/include")
                (string-append "--with-build-sysroot=" glibc "/include")

                "--disable-bootstrap"
                "--disable-decimal-float"
                "--disable-libatomic"
                "--disable-libcilkrts"
                "--disable-libgomp"
                "--disable-libitm"
                "--disable-libmudflap"
                "--disable-libquadmath"
                "--disable-libsanitizer"
                "--disable-libssp"
                "--disable-libvtv"
                "--disable-lto"
                "--disable-lto-plugin"
                "--disable-multilib"
                "--disable-plugin"
                "--disable-threads"
                ;;"--enable-languages=c,c++"
                "--enable-languages=c"

                "--enable-static"
                ;; libstdc++.so: error: depends on 'libgcc_s.so.1', which cannot be found in RUNPATH ()
                "--disable-shared"
                "--enable-threads=single"

                ;; No pre-compiled libstdc++ headers, to save space.
                "--disable-libstdcxx-pch"

                ;; for libcpp ...
                "--disable-build-with-cxx"))
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'apply-boot-patch
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((patch-file (assoc-ref inputs "boot-patch")))
                (format (current-error-port) "patch file=~s\n" patch-file)
                (system* "patch" "--force" "-p1" "-i" patch-file))
              #t))
          ;; c&p from commencement.scm:gcc-boot0
          (add-after 'unpack 'unpack-gmp&co
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((gmp  (assoc-ref %build-inputs "gmp-source"))
                    (mpfr (assoc-ref %build-inputs "mpfr-source"))
                    (mpc  (assoc-ref %build-inputs "mpc-source")))

                ;; To reduce the set of pre-built bootstrap inputs, build
                ;; GMP & co. from GCC.
                (for-each (lambda (source)
                            (or (invoke "tar" "xvf" source)
                                (error "failed to unpack tarball"
                                       source)))
                          (list gmp mpfr mpc))

                ;; Create symlinks like `gmp' -> `gmp-x.y.z'.
                ,@(map (lambda (lib)
                         ;; Drop trailing letters, as gmp-6.0.0a unpacks
                         ;; into gmp-6.0.0.
                         `(symlink ,(string-trim-right
                                     (package-full-name lib "-")
                                     char-set:letter)
                                   ,(package-name lib)))
                       (list gmp-boot mpfr-boot mpc-boot))
                #t)))
          (add-before 'configure 'setenv
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (binutils (assoc-ref %build-inputs "binutils"))
                     (bash (assoc-ref %build-inputs "bash"))
                     (gcc (assoc-ref %build-inputs "gcc"))
                     (glibc (assoc-ref %build-inputs "libc"))
                     (kernel-headers (assoc-ref %build-inputs "kernel-headers")))
                (setenv "CONFIG_SHELL" (string-append bash "/bin/sh"))
                (setenv "C_INCLUDE_PATH" (string-append
                                          gcc "/lib/gcc-lib/i686-unknown-linux-gnu/2.95.3/include"
                                          ":" kernel-headers "/include"
                                          ":" glibc "/include"
                                          ":" (getcwd) "/mpfr/src"))
                (setenv "LIBRARY_PATH" (string-append glibc "/lib"
                                                      ":" gcc "/lib"))
                (format (current-error-port) "C_INCLUDE_PATH=~a\n" (getenv "C_INCLUDE_PATH"))
                (format (current-error-port) "LIBRARY_PATH=~a\n" (getenv "LIBRARY_PATH"))
                #t)))))))))

(define-public gcc-mesboot1
  (package-with-bootstrap-guile
   (package
     (inherit gcc-core-mesboot1)
     (name "gcc-mesboot1")
     (version "4.6.4")
     (native-inputs `(("gcc-g++"
                       ,(origin
                          (method url-fetch)
                          (uri (string-append "mirror://gnu/gcc/gcc-"
                                              version "/gcc-g++-" version ".tar.gz"))
                          (sha256
                           (base32
                            "1fqqk5zkmdg4vmqzdmip9i42q6b82i3f6yc0n86n9021cr7ms2k9"))))
                      ,@(package-native-inputs gcc-core-mesboot1)))
     (arguments
      (substitute-keyword-arguments (package-arguments gcc-core-mesboot1)
        ((#:configure-flags configure-flags)
         `(let ((out (assoc-ref %outputs "out")))
            `("--enable-languages=c,c++"
              ,@(filter
                 (negate (lambda (x) (string-prefix? "--enable-languages=" x)))
                 ,configure-flags))))
        ((#:phases phases)
         `(modify-phases ,phases
            (add-before 'unpack 'unpack-g++
              (lambda _
                (let ((source-g++ (assoc-ref %build-inputs "gcc-g++")))
                  (invoke "tar" "xvf" source-g++))
                #t)))))))))

(define-public xz-mesboot
  (package-with-bootstrap-guile
   (package
     (inherit xz)
     (name "xz-mesboot")
     (version "5.0.0")
     (source (origin
               (method url-fetch)
               (uri (list (string-append "http://tukaani.org/xz/xz-" version
                                         ".tar.gz")
                          (string-append "http://multiprecision.org/guix/xz-"
                                         version ".tar.gz")))
               (sha256
                (base32
                 "0kf40ggbs1vaaj5s9k4csycahzqcf65n20pa6lngqhm6j0cj3agb"))))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (outputs '("out"))
     (propagated-inputs '())
     (native-inputs `(("bash" ,bash-mesboot)
                      ("binutils" ,binutils-mesboot0) ;; try binutils-mesboot, or gawk-mesboot and remove __GNUC__=1 hack
                      ("coreutils" ,coreutils-mesboot0)
                      ("diffutils" ,diffutils-mesboot0)
                      ("gawk" ,gawk-mesboot0)
                      ("grep" ,grep-mesboot0)
                      ("gcc" ,gcc-mesboot1)
                      ("gzip" ,gzip-mesboot0)
                      ("libc" ,glibc-mesboot0)
                      ("make" ,make-mesboot0)
                      ("sed" ,sed-mesboot)
                      ("tar" ,tar-mesboot)

                      ("guile" ,%bootstrap-guile)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:parallel-build? #f
        #:configure-flags
        `("--disable-assembler"
          "--disable-shared"
          "--enable-small"
          "--disable-threads"
          "--disable-xzdec"
          "--disable-lzmadec"
          "--disable-lzmainfo"
          "--disable-lzma-links"
          "--disable-scripts"
          "--disable-doc"
          "--disable-nls"
          "--disable-symbol-versions"
          ;; configure disqualifies BASH, CPP, GCC and GREP
          ;; all of which seem fine for the build
          "ac_cv_prog_cc_c99=-std=gnu9x"
          "ac_cv_path_GREP=grep"
          "gl_cv_posix_shell=bash"
          "ac_cv_have_decl_optreset=no"
          "CPPFLAGS=-D__GNUC__=1"))))))

(define-public hello-mesboot
  (package-with-bootstrap-guile
   (package
     (inherit hello)
     (name "hello-mesboot")
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("bash" ,bash-mesboot)
                      ("binutils" ,binutils-mesboot0)
                      ("coreutils" ,coreutils-mesboot0)
                      ("gawk" ,gawk-mesboot0)
                      ("grep" ,grep-mesboot0)
                      ("gcc" ,gcc-mesboot1)
                      ("gzip" ,gzip-mesboot0)
                      ("libc" ,glibc-mesboot0)
                      ("make" ,make-mesboot0)
                      ("sed" ,sed-mesboot)
                      ("tar" ,tar-mesboot)

                      ("guile" ,%bootstrap-guile)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:parallel-build? #f
        ;;checking for grep that handles long lines and -e... configure: error: no acceptable grep could be found in /gnu/store/kk3ii89hn8rb8nbx06yc0y88g6fxd7n0-bash-mesboot0-2.05b/bin:/gnu/store/3dqmncxsy9rhndrpv4z525qwjd4a6p2w-binutils-mesboot0-2.14/bin:/gnu/store/drnh05bipvmq0i95chbg268zn53c94m9-coreutils-mesboot0-5.0/bin:/gnu/store/7qp8nbm7043d53ib9yzjx18w8rm59lj1-gawk-mesboot0-3.0.0/bin:/gnu/store/07xns0335wjr4v55fywlcahc9vxqzgg9-grep-mesboot-2.0/bin:/gnu/store/9sp747nqf75fcfmfaksp7131p1zqwjj6-grep-mesboot0-2.0/bin:/gnu/store/4m8gxymqbkcvkhrm5219jqyhhabb8cl9-gcc-mesboot1-wrapper-4.6.4/bin:/gnu/store/mcf357agb72kfdzni58v6lfjbcfd98yi-gzip-mesboot0-1.2.4/bin:/gnu/store/2l2ggsxg3lnsbp1yd1il2ph12z81iv58-glibc-mesboot0-2.2.5/bin:/gnu/store/2l2ggsxg3lnsbp1yd1il2ph12z81iv58-glibc-mesboot0-2.2.5/sbin:/gnu/store/ddh28d3l4vjmi5lsyihlb4kfm9hrmmv1-make-mesboot0-3.80/bin:/gnu/store/6m2yjkljqh0j58an0a0irviimm87bwna-sed-mesboot0-1.18/bin:/gnu/store/36v9sy54bhmrin1yblss5rzrfg3ikpva-tar-mesboot-1.22/bin:/gnu/store/7c8w4l68warf06c5s2m9sm8agb90v65c-guile-bootstrap-2.2/bin:/usr/xpg4/bin

        #:configure-flags '("ac_cv_path_GREP=grep")
        #:phases
        (modify-phases %standard-phases
          (replace 'check
            (lambda _
              (invoke "./hello")))))))))

(define-public binutils-mesboot
  (package-with-bootstrap-guile
   (package
     (inherit binutils)
     (name "binutils-mesboot")
     (version "2.20.1a")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/binutils/binutils-"
                                   version ".tar.bz2"))
               (patches (search-patches "binutils-boot-2.20.1a.patch"))
               (sha256
                (base32
                 "0r7dr0brfpchh5ic0z9r4yxqn4ybzmlh25sbp30cacqk8nb7rlvi"))))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("bash" ,bash-mesboot)
                      ("binutils" ,binutils-mesboot0)
                      ("bzip2" ,bzip2-mesboot0)
                      ("coreutils" ,coreutils-mesboot0)
                      ("gawk" ,gawk-mesboot0)
                      ("gcc" ,gcc-mesboot1)
                      ("grep" ,grep-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("libc" ,glibc-mesboot0)
                      ("make" ,make-mesboot)
                      ("patch" ,patch-mesboot0)
                      ("sed" ,sed-mesboot)
                      ("tar" ,tar-mesboot)
                      ("xz" ,xz-mesboot)

                      ("kernel-headers" ,%bootstrap-linux-libre-headers)))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:tests? #f                     ; runtest: command not found
        #:parallel-build? #f
        #:strip-binaries? #f            ; no strip yet
        #:configure-flags
        `("CC=gcc"
          "CXX=false"
          "RANLIB=true"
          "--disable-doc"
          "--disable-nls"
          "--disable-shared"
          "--disable-werror"
          "--build=i686-unknown-linux-gnu"
          "--host=i686-unknown-linux-gnu"
          "--with-sysroot=/"
          ;; checking for grep that handles long lines and -e
          "ac_cv_path_GREP=grep")
        ;; FIXME: ac_cv_path_GREP=grep doesn't seem to be forwarded to
        ;; cascading configure's?
        #:make-flags '("ac_cv_path_GREP=grep")
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'scripted-patch
            (lambda _
              ;; sed-mesboot0 cannot build these
              (copy-file "binutils/Makefile.in" "binutils/Makefile.in.orig")
              (substitute* "binutils/Makefile.in"
                ;; binutils/binutils uses an amazingly complex install
                ;; command, using FOR, SED, READ, IF, ECHO, SED, SED, AWK,
                ;; READ, and then LIBTOOL (to do something like
                ;; `mkdir $DESTDIR$bindir; cp readline $DESTDIR$bindir ...')

                ;; Some tool [debugme!] cannot handle two escaped newlines
                ;; (bash?), and the install stops after $(am__EXEEXT_11)
                ;; ("objcopy"), so $(am__EXEEXT_13) ("readelf") and others do
                ;; not get installed.  Remove the stray newline:
                (("^\t@BUILD_NLMCONV@ @BUILD_SRCONV@ @BUILD_DLLTOOL@ @BUILD_WINDRES@ .*") ""))
              (substitute* "opcodes/Makefile.in"
                (("^SUBDIRS = [.] po") "SUBDIRS = ."))
              (substitute* "binutils/Makefile.in"
                (("^SUBDIRS = doc po") "SUBDIRS ="))
              (substitute* "gas/Makefile.in"
                (("^SUBDIRS = doc po") "SUBDIRS ="))
              (substitute* "gprof/Makefile.in"
                (("^SUBDIRS = po") "SUBDIRS ="))
              (substitute* "ld/Makefile.in"
                (("^SUBDIRS = po") "SUBDIRS ="))
              #t))))))))

(define glibc-headers-mesboot
  (package-with-bootstrap-guile
   (package
     (inherit glibc-mesboot0)
     (name "glibc-headers-mesboot")
     (version "2.16.0")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/glibc/glibc-"
                                   version
                                   ".tar.gz"))
               (patches (search-patches "glibc-boot-2.16.0.patch"
                                        "glibc-bootstrap-system-2.16.0.patch"))
               (sha256
                (base32
                 "0vlz4x6cgz7h54qq4528q526qlhnsjzbsvgc4iizn76cb0bfanx7"))))
     (native-inputs `(("bash" ,bash-mesboot)
                      ("binutils" ,binutils-mesboot)
                      ("coreutils" ,coreutils-mesboot0)
                      ("diffutils" ,diffutils-mesboot0)

                      ("gawk" ,gawk-mesboot2)
                      ;; This error does not occur when using gawk-seed
                      ;; no error, but not good enough:
                      ;; checking for -z relro option... no
                      ;; configure: error: linker with -z relro support required

                      ;; ("gawk" ,gawk-mesboot1)
                      ;; gawk: cmd. line:4: (FILENAME=- FNR=10) fatal: function `strtonum' not defined
                      ;; ("gawk" ,gawk-mesboot0)
                      ("gcc" ,gcc-mesboot1)
                      ("grep" ,grep-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("headers" ,mesboot-headers)
                      ("libc" ,glibc-mesboot0)
                      ("make" ,make-mesboot)
                      ("patch" ,patch-mesboot0)
                      ;; ("sed" ,sed-seed)
                      ("sed" ,sed-mesboot)
                      ("tar" ,tar-mesboot)
                      ;; ("tar" ,tar-seed)
                      ("xz" ,xz-mesboot)

                      ;; ("gash" ,%bootstrap-gash+coreutils)
                      ;; ("guile" ,%bootstrap-guile)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)))

     (arguments
      (substitute-keyword-arguments (package-arguments glibc-mesboot0)
        ((#:configure-flags configure-flags)
         `(let ((out (assoc-ref %outputs "out"))
                (headers (assoc-ref %build-inputs "headers")))
            (list
             (string-append "--prefix=" out)
             ;; "--debug"
             ;; "ac_config_status_args=--debug"
             "--disable-obsolete-rpc"
             "--host=i686-unknown-linux-gnu"
             (string-append "--with-headers=" headers "/include")
             "--enable-static-nss"
             "--with-pthread"
             "--without-cvs"
             "--without-gd"
             "--enable-add-ons=nptl"
             ;; avoid: configure: error: confusing output from nm -u
             "libc_cv_predef_stack_protector=no")))
        ((#:make-flags make-flags)
         '(list "install-bootstrap-headers=yes" "install-headers"))
        ((#:phases phases)
         `(modify-phases ,phases
            ;;(add-after 'build 'inspect (lambda _ (throw 'inspect)))
            (delete 'apply-boot-patch)
            (delete 'set-path)
            ;; back to the default...but we lost that in glibc-mesboot0 :-(
            (replace 'unpack
              (lambda* (#:key source #:allow-other-keys)
                (invoke "tar" "xvf" source)
                ;;(chdir (first-subdirectory "."))
                (chdir (string-append "glibc-" ,version))
                #t))
            (replace 'setenv
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (headers (assoc-ref %build-inputs "headers"))
                       (bash (assoc-ref %build-inputs "bash"))
                       (shell (string-append bash "/bin/bash"))
                       (coreutils (assoc-ref %build-inputs "coreutils"))
                       (libc (assoc-ref %build-inputs "libc"))
                       (gcc (assoc-ref %build-inputs "gcc"))
                       (cppflags (string-append
                                  " -I " (getcwd) "/nptl/sysdeps/pthread/bits"
                                  " -D BOOTSTRAP_GLIBC=1"))
                       (cflags (string-append " -L " (getcwd)
                                              " -L " libc "/lib")))
                  (setenv "libc_cv_friendly_stddef" "yes")
                  (setenv "CONFIG_SHELL" shell)
                  (setenv "SHELL" shell)

                  (setenv "CPP" (string-append gcc "/bin/gcc -E " cppflags))
                  (setenv "CC" (string-append gcc "/bin/gcc " cppflags cflags))
                  ;; (setenv "CPP" (string-append "gcc -E " cppflags))
                  ;; (setenv "CC" (string-append "gcc " cppflags cflags))

                  (setenv "LD" "gcc")

                  ;; avoid -fstack-protector
                  (setenv "libc_cv_ssp" "false")
                  (substitute* "configure"
                    (("/bin/pwd") (string-append coreutils "/bin/pwd")))
                  (setenv "C_INCLUDE_PATH" (string-append libc "/include"
                                                          headers "/include"))
                  (setenv "LIBRARY_PATH" (string-append libc "/lib")))))
            (replace 'install
              (lambda* (#:key outputs make-flags #:allow-other-keys)
                (let ((kernel-headers (assoc-ref %build-inputs "kernel-headers"))
                      (out (assoc-ref outputs "out")))
                  (and (apply invoke "make" make-flags)
                       (copy-recursively kernel-headers out)
                       #t))))
            (replace 'configure
              (lambda* (#:key configure-flags #:allow-other-keys)
                (format (current-error-port) "running ../configure ~a\n" (string-join configure-flags))
                (mkdir-p "build")
                (chdir "build")
                (apply invoke "../configure" configure-flags)))
            (add-after 'configure 'remove-sunrpc
              (lambda _
                (let* ((out (assoc-ref %outputs "out"))
                       (bash (assoc-ref %build-inputs "bash"))
                       (shell (string-append bash "/bin/bash")))
                  (let ((Makefile (open-file "Makefile" "a")))
                    (display (string-append "

SHELL := " shell "
")
                             Makefile)
                    (close Makefile))
                  (substitute* "../Makefile"
                    (("^SHELL := /bin/sh") (string-append "SHELL := " shell)))
                  (substitute* "../Makeconfig"
                    (("^SHELL := /bin/sh") (string-append "SHELL := " shell)))
                  (substitute* "../elf/Makefile"
                    (("^SHELL := /bin/sh") (string-append "SHELL := " shell)))
                  (invoke "make" (string-append (getcwd) "/sysd-sorted" ))
                  (substitute* "sysd-sorted"
                    ((" sunrpc") " ")
                    ((" nis") " "))
                  #t))))))))))

;; /gnu/store/5apmw0r8mzk062ry7b436xdl99z5arbn-bash-mesboot-4.4/bin/bash: line 1:  8985 Segmentation fault      LC_ALL=C sed -f po2test.sed ../po/de.po > /tmp/guix-build-glibc-mesboot-2.16.0.drv-0/glibc-2.16.0/build/intl/msgs.h
;; ==> sys_siglist stub

;; Gash sed:
;; ERROR: In procedure scm-error:
;; Unterminated bracket expression

(define glibc-mesboot
  (package-with-bootstrap-guile
   (package
     (inherit glibc-headers-mesboot)
     (name "glibc-mesboot")
     (native-inputs `(("bash" ,bash-mesboot1)
                      ("binutils" ,binutils-mesboot)
                      ("coreutils" ,coreutils-mesboot0)
                      ("diffutils" ,diffutils-mesboot0)
                      ("gawk" ,gawk-mesboot2)
                      ("gcc" ,gcc-mesboot1)
                      ("grep" ,grep-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("headers" ,glibc-headers-mesboot)
                      ("libc" ,glibc-mesboot0)
                      ("make" ,make-mesboot)
                      ("patch" ,patch-mesboot0)
                      ("sed" ,sed-mesboot)
                      ;;("sed" ,sed-mesboot)
                      ("tar" ,tar-mesboot)
                      ("xz" ,xz-mesboot)

                      ("gash" ,%bootstrap-gash+coreutils)
                      ("guile" ,%bootstrap-guile)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)))
     (arguments
      `(#:validate-runpath? #f ; fails when using --enable-shared
        ,@(substitute-keyword-arguments (package-arguments glibc-headers-mesboot)
            ((#:make-flags make-flags)
             `(let ((bash (assoc-ref %build-inputs "bash")))
                (list (string-append "SHELL=" bash "/bin/sh"))))
            ((#:phases phases)
             `(modify-phases ,phases
                (replace 'install
                  (lambda* (#:key outputs make-flags #:allow-other-keys)
                    (let* ((kernel-headers (assoc-ref %build-inputs "kernel-headers"))
                           (out (assoc-ref outputs "out"))
                           (install-flags (cons "install" make-flags)))
                      (and (apply invoke "make" install-flags)
                           (copy-recursively kernel-headers out)
                           #t)))))))))
     (native-search-paths ;; FIXME: move to glibc-mesboot0
      ;; Use the language-specific variables rather than 'CPATH' because they
      ;; are equivalent to '-isystem' whereas 'CPATH' is equivalent to '-I'.
      ;; The intent is to allow headers that are in the search path to be
      ;; treated as "system headers" (headers exempt from warnings) just like
      ;; the typical /usr/include headers on an FHS system.
      (list (search-path-specification
             (variable "C_INCLUDE_PATH")
             (files '("include")))
            (search-path-specification
             (variable "CPLUS_INCLUDE_PATH")
             (files '("include")))
            (search-path-specification
             (variable "LIBRARY_PATH")
             (files '("lib"))))))))

(define gcc-mesboot1-wrapper
  ;; We need this so gcc-mesboot1 can be used to create shared binaries that
  ;; have the correct interpreter, otherwise configuring gcc-mesboot using
  ;; --enable-shared will fail.
  (package-with-bootstrap-guile
   (package
     (inherit gcc-mesboot1)
     (name "gcc-mesboot1-wrapper")
     (source #f)
     (inputs '())
     (native-inputs `(("bash" ,bash-mesboot)
                      ("coreutils" ,coreutils-mesboot0)
                      ("libc" ,glibc-mesboot)
                      ("gcc" ,gcc-mesboot1)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:phases
        (modify-phases %standard-phases
          (delete 'unpack)
          (delete 'configure)
          (delete 'install)
          (replace 'build
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bash (assoc-ref %build-inputs "bash"))
                     (libc (assoc-ref %build-inputs "libc"))
                     (gcc (assoc-ref %build-inputs "gcc"))
                     (bin (string-append out "/bin")))
                (mkdir-p bin)
                (for-each
                 (lambda (program)
                   (let ((wrapper (string-append bin "/" program)))
                     (with-output-to-file wrapper
                       (lambda _
                         (display (string-append "#! " bash "/bin/bash
exec " gcc "/bin/" program
" -Wl,--dynamic-linker"
;; also for x86_64-linux, we are still on i686-linux
" -Wl," libc ,(glibc-dynamic-linker "i686-linux")
" -Wl,--rpath"
" -Wl," libc "/lib"
" \"$@\"
"))
                         (chmod wrapper #o555)))))
                 '(
                   "cpp"
                   "gcc"
                   "g++"
                   "i686-unknown-linux-gnu-cpp"
                   "i686-unknown-linux-gnu-gcc"
                   "i686-unknown-linux-gnu-g++"
                   ))
                #t)))
          (replace 'check
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (program (string-append bin "/gcc")))
                (invoke program "--help"))))))))))

(define gcc-mesboot
  (package-with-bootstrap-guile
   (package
     (inherit gcc-mesboot1)
     (name "gcc-mesboot")
     (version "4.9.4")
     (source (package-source gcc-4.9))
     (native-inputs `(("bash" ,bash-mesboot)
                      ("binutils" ,binutils-mesboot)
                      ("bzip2" ,bzip2-mesboot0)
                      ("coreutils" ,coreutils-mesboot0)
                      ("diffutils" ,diffutils-mesboot0)
                      ("gawk" ,gawk-mesboot2)
                      ("gcc-wrapper" ,gcc-mesboot1-wrapper)
                      ("gcc" ,gcc-mesboot1)
                      ("grep" ,grep-mesboot0)
                      ("gzip" ,gzip-mesboot0)
                      ("libc" ,glibc-mesboot)
                      ("make" ,make-mesboot)
                      ("patch" ,patch-mesboot0)
                      ("sed" ,sed-mesboot)
                      ("tar" ,tar-mesboot) ; MUST use; Gash' tar cannot handle @LongLink
                      ("xz" ,xz-mesboot)

                      ("kernel-headers" ,%bootstrap-linux-libre-headers)))
     (arguments
      `(#:validate-runpath? #f
        ,@(substitute-keyword-arguments (package-arguments gcc-mesboot1)
            ((#:configure-flags configure-flags)
             `(let ((out (assoc-ref %outputs "out"))
                    (glibc (assoc-ref %build-inputs "libc")))
                (list (string-append "--prefix=" out)
                      "--build=i686-unknown-linux-gnu"
                      "--host=i686-unknown-linux-gnu"

                      "--with-host-libstdcxx=-lsupc++"

                      (string-append "--with-native-system-header-dir=" glibc "/include")
                      (string-append "--with-build-sysroot=" glibc "/include")

                      "--disable-bootstrap"
                      "--disable-decimal-float"
                      "--disable-libatomic"
                      "--disable-libcilkrts"
                      "--disable-libgomp"
                      "--disable-libitm"
                      "--disable-libmudflap"
                      "--disable-libquadmath"
                      "--disable-libsanitizer"
                      "--disable-libssp"
                      "--disable-libvtv"
                      "--disable-lto"
                      "--disable-lto-plugin"
                      "--disable-multilib"
                      "--disable-plugin"
                      "--disable-threads"
                      "--enable-languages=c,c++"

                      "--enable-static"
                      "--enable-shared"
                      "--enable-threads=single"

                      ;; No pre-compiled libstdc++ headers, to save space.
                      "--disable-libstdcxx-pch"

                      ;; for libcpp ...
                      "--disable-build-with-cxx")))
            ((#:phases phases)
             `(modify-phases ,phases
                (delete 'apply-boot-patch)
                (delete 'unpack-g++) ; sadly, gcc-4.9.4 does not provide
                                     ; partial core/language downloads
                (replace 'setenv
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (binutils (assoc-ref %build-inputs "binutils"))
                           (bash (assoc-ref %build-inputs "bash"))
                           (gcc (assoc-ref %build-inputs "gcc"))
                           (glibc (assoc-ref %build-inputs "libc"))
                           (kernel-headers (assoc-ref %build-inputs "kernel-headers")))
                      (setenv "CONFIG_SHELL" (string-append bash "/bin/sh"))
                      (setenv "C_INCLUDE_PATH" (string-append
                                                gcc "/lib/gcc-lib/i686-unknown-linux-gnu/4.6.4/include"
                                                ":" kernel-headers "/include"
                                                ":" glibc "/include"
                                                ":" (getcwd) "/mpfr/src"))
                      (setenv "CPLUS_INCLUDE_PATH" (string-append
                                                    gcc "/lib/gcc-lib/i686-unknown-linux-gnu/4.6.4/include"
                                                    ":" kernel-headers "/include"
                                                    ":" glibc "/include"
                                                    ":" (getcwd) "/mpfr/src"))
                      (setenv "LIBRARY_PATH" (string-append glibc "/lib"
                                                            ":" gcc "/lib"))
                      (format (current-error-port) "C_INCLUDE_PATH=~a\n" (getenv "C_INCLUDE_PATH"))
                      (format (current-error-port) "CPLUS_INCLUDE_PATH=~a\n" (getenv "CPLUS_INCLUDE_PATH"))
                      (format (current-error-port) "LIBRARY_PATH=~a\n" (getenv "LIBRARY_PATH"))
                      #t)))))))))))

(define gcc-mesboot-wrapper
  ;; We need this so gcc-mesboot can be used to create shared binaries that
  ;; have the correct interpreter and runpath to libc.
  (package-with-bootstrap-guile
   (package
     (inherit gcc-mesboot1-wrapper)
     (name "gcc-mesboot-wrapper")
     (source #f)
     (inputs '())
     (native-inputs `(("bash" ,bash-mesboot)
                      ("coreutils" ,coreutils-mesboot0)
                      ("libc" ,glibc-mesboot)
                      ("gcc" ,gcc-mesboot))))))

(define-public coreutils-mesboot
  (package-with-bootstrap-guile
   (package
     (inherit coreutils)
     (name "coreutils-mesboot")
     (native-inputs `(("bash" ,bash-mesboot)
                      ("binutils" ,binutils-mesboot)
                      ("coreutils" ,coreutils-mesboot0)
                      ("diffutils" ,diffutils-mesboot0)
                      ("gawk" ,gawk-mesboot0)
                      ("grep" ,grep-mesboot0)
                      ("gcc-wrapper" ,gcc-mesboot-wrapper)
                      ("gcc" ,gcc-mesboot)
                      ("gzip" ,gzip-mesboot0)
                      ("libc" ,glibc-mesboot)
                      ("make" ,make-mesboot0)
                      ("sed" ,sed-mesboot)
                      ("tar" ,tar-mesboot)
                      ("xz" ,xz-mesboot)))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:tests? #f)))))                ; FIXME: hangs after guard-input.sh

(define (%bootstrap-inputs+toolchain)
  ;; The traditional bootstrap-inputs.  For the i686-linux, x86_64-linux
  ;; Scheme-only bootstrap the actual reduced set with bootstrapped toolchain.
  (append (match (%current-system)
            ((or "i686-linux" "x86_64-linux")
             `(("bash" ,bash-mesboot)
               ("bzip2" ,bzip2-mesboot0)
               ("binutils" ,binutils-mesboot)
               ("coreutils" ,coreutils-mesboot)
               ("diffutils" ,diffutils-mesboot0)
               ("gawk" ,gawk-mesboot2)
               ("gcc-wrapper" ,gcc-mesboot-wrapper)
               ("gcc" ,gcc-mesboot)
               ("grep" ,grep-mesboot0)
               ("gzip" ,gzip-mesboot0)
               ("libc" ,glibc-mesboot)
               ("make" ,make-mesboot)
               ("patch" ,patch-mesboot0)
               ("sed" ,sed-mesboot)
               ("tar" ,tar-mesboot)
               ("xz" ,xz-mesboot)))
            (_
             '()))
          (%bootstrap-inputs)))

(define gnu-make-boot0
  (package-with-bootstrap-guile
   (package (inherit gnu-make)
     (name "make-boot0")
     (arguments
      `(#:guile ,%bootstrap-guile
        #:implicit-inputs? #f
        #:tests? #f                  ; cannot run "make check"
        ,@(substitute-keyword-arguments (package-arguments gnu-make)
            ((#:phases phases)
             `(modify-phases ,phases
                (replace 'build
                  (lambda _
                    (invoke "./build.sh")
                    #t))
                (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (bin (string-append out "/bin")))
                      (install-file "make" bin)
                      #t))))))))
     (native-inputs '())                          ; no need for 'pkg-config'
     (inputs (%bootstrap-inputs+toolchain)))))

(define diffutils-boot0
  (package-with-bootstrap-guile
   (let ((p (package-with-explicit-inputs diffutils
                                          (lambda _
                                            `(("make" ,gnu-make-boot0)
                                              ,@(%bootstrap-inputs+toolchain)))
                                          #:guile %bootstrap-guile)))
     (package (inherit p)
       (name "diffutils-boot0")
       (arguments `(#:tests? #f         ; the test suite needs diffutils
                    ,@(package-arguments p)))))))

(define findutils-boot0
  (package-with-bootstrap-guile
   (package-with-explicit-inputs (package
                                   (inherit findutils)
                                   (name "findutils-boot0"))
                                 (lambda _
                                   `(("make" ,gnu-make-boot0)
                                     ("diffutils" ,diffutils-boot0) ; for tests
                                     ,@(%bootstrap-inputs+toolchain)))
                                 (current-source-location)
                                 #:guile %bootstrap-guile)))

(define file-boot0
  (package-with-bootstrap-guile
   (package-with-explicit-inputs (package
                                   (inherit file)
                                   (name "file-boot0")
                                   (arguments
                                    '(#:strip-binaries? #f
                                      #:validate-runpath? #f)))
                                 (lambda _
                                   `(("make" ,gnu-make-boot0)
                                     ,@(%bootstrap-inputs+toolchain)))
                                 (current-source-location)
                                 #:guile %bootstrap-guile)))

(define (%boot0-inputs)
  `(("make" ,gnu-make-boot0)
    ("diffutils" ,diffutils-boot0)
    ("findutils" ,findutils-boot0)
    ("file" ,file-boot0)
    ,@(%bootstrap-inputs+toolchain)))

(define* (boot-triplet #:optional (system (%current-system)))
  ;; Return the triplet used to create the cross toolchain needed in the
  ;; first bootstrapping stage.
  (nix-system->gnu-triplet system "guix"))

;; Following Linux From Scratch, build a cross-toolchain in stage 0.  That
;; toolchain actually targets the same OS and arch, but it has the advantage
;; of being independent of the libc and tools in
;; (%BOOTSTRAP-INPUTS+TOOLCHAIN), since GCC-BOOT0 (below) is built without any
;; reference to the target libc.

(define binutils-boot0
  (package-with-bootstrap-guile
   (package (inherit binutils)
     (name "binutils-cross-boot0")
     (arguments
      `(#:guile ,%bootstrap-guile
        #:implicit-inputs? #f

        #:modules ((guix build gnu-build-system)
                   (guix build utils)
                   (ice-9 ftw))                    ; for 'scandir'
        #:phases (modify-phases %standard-phases
                   (add-after 'install 'add-symlinks
                     (lambda* (#:key outputs #:allow-other-keys)
                       ;; The cross-gcc invokes 'as', 'ld', etc, without the
                       ;; triplet prefix, so add symlinks.
                       (let ((out (assoc-ref outputs "out"))
                             (triplet-prefix (string-append ,(boot-triplet) "-")))
                         (define (has-triplet-prefix? name)
                           (string-prefix? triplet-prefix name))
                         (define (remove-triplet-prefix name)
                           (substring name (string-length triplet-prefix)))
                         (with-directory-excursion (string-append out "/bin")
                           (for-each (lambda (name)
                                       (symlink name (remove-triplet-prefix name)))
                                     (scandir "." has-triplet-prefix?)))
                         #t))))

        ,@(substitute-keyword-arguments (package-arguments binutils)
            ((#:configure-flags cf)
             `(cons ,(string-append "--target=" (boot-triplet))
                    ,cf)))))
     (inputs (%boot0-inputs)))))

(define libstdc++-boot0
  ;; GCC's libcc1 is always built as a shared library (the top-level
  ;; 'Makefile.def' forcefully adds --enable-shared) and thus needs to refer
  ;; to libstdc++.so.  We cannot build libstdc++-5.3 because it relies on
  ;; C++14 features missing in some of our bootstrap compilers.
  (let ((lib (package-with-bootstrap-guile (make-libstdc++ gcc-4.9))))
    (package
      (inherit lib)
      (name "libstdc++-boot0")
      (arguments
       `(#:guile ,%bootstrap-guile
         #:implicit-inputs? #f

         ;; XXX: libstdc++.so NEEDs ld.so for some reason.
         #:validate-runpath? #f

         ,@(match (%current-system)
             ((or "i686-linux" "x86_64-linux")
              (substitute-keyword-arguments (package-arguments lib)
                ((#:phases phases)
                 `(modify-phases ,phases
                    (add-after 'unpack 'workaround-wrapper-bug
                      ;; XXX: The crude gcc-cross-wrapper causes "g++ -v" to
                      ;; fail, which in turn confuses the configure script.
                      (lambda _
                        (substitute* "libstdc++-v3/configure"
                          (("g\\+\\+ -v") "true"))
                        #t))))))
             (_ (package-arguments lib)))))
      (inputs (%boot0-inputs))
      (native-inputs '()))))

(define gcc-boot0
  (package-with-bootstrap-guile
   (package (inherit gcc)
     (name "gcc-cross-boot0")
     (arguments
      `(#:guile ,%bootstrap-guile
        #:implicit-inputs? #f
        #:modules ((guix build gnu-build-system)
                   (guix build utils)
                   (ice-9 regex)
                   (srfi srfi-1)
                   (srfi srfi-26))
        ,@(substitute-keyword-arguments (package-arguments gcc)
            ((#:configure-flags flags)
             `(append (list ,(string-append "--target=" (boot-triplet))

                            ;; No libc yet.
                            "--without-headers"

                            ;; Disable features not needed at this stage.
                            "--disable-shared"
                            "--enable-languages=c,c++"

                            ;; libstdc++ cannot be built at this stage
                            ;; ("Link tests are not allowed after
                            ;; GCC_NO_EXECUTABLES.").
                            "--disable-libstdc++-v3"

                            "--disable-threads"
                            "--disable-libmudflap"
                            "--disable-libatomic"
                            "--disable-libsanitizer"
                            "--disable-libitm"
                            "--disable-libgomp"
                            "--disable-libcilkrts"
                            "--disable-libvtv"
                            "--disable-libssp"
                            "--disable-libquadmath"
                            "--disable-decimal-float")
                      (remove (cut string-match
                                "--(with-system-zlib|enable-languages.*)" <>)
                              ,flags)))
            ((#:make-flags flags)
             `(let* ((libc        (assoc-ref %build-inputs "libc"))
                     (libc-native (or (assoc-ref %build-inputs "libc-native")
                                      libc)))
                `(,(string-append "LDFLAGS="
                                  "-Wl,-rpath=" libc-native "/lib "
                                  "-Wl,-dynamic-linker "
                                  "-Wl," libc-native ,(glibc-dynamic-linker
                                                       (match (%current-system)
                                                         ("x86_64-linux" "i686-linux")
                                                         (_ (%current-system))))))))
            ((#:phases phases)
             `(modify-phases ,phases
                (add-after 'unpack 'unpack-gmp&co
                  (lambda* (#:key inputs #:allow-other-keys)
                    (let ((gmp  (assoc-ref %build-inputs "gmp-source"))
                          (mpfr (assoc-ref %build-inputs "mpfr-source"))
                          (mpc  (assoc-ref %build-inputs "mpc-source")))

                      ;; To reduce the set of pre-built bootstrap inputs, build
                      ;; GMP & co. from GCC.
                      (for-each (lambda (source)
                                  (invoke "tar" "xvf" source))
                                (list gmp mpfr mpc))

                      ;; Create symlinks like `gmp' -> `gmp-x.y.z'.
                      ,@(map (lambda (lib)
                               ;; Drop trailing letters, as gmp-6.0.0a unpacks
                               ;; into gmp-6.0.0.
                               `(symlink ,(string-trim-right
                                           (package-full-name lib "-")
                                           char-set:letter)
                                         ,(package-name lib)))
                             (list gmp-6.0 mpfr mpc))
                      #t)))
                ,(match (%current-system)
                   ((or "i686-linux" "x86_64-linux")
                    '(add-before 'configure 'fix-libcc1
                      (lambda* (#:key inputs #:allow-other-keys)
                        ;; libcc1.so NEEDs libgcc_s.so, so provide one here
                        ;; to placate the 'validate-runpath' phase.
                        (substitute* "libcc1/Makefile.in"
                          (("la_LDFLAGS =")
                           (string-append "la_LDFLAGS = -Wl,-rpath="
                                          (assoc-ref inputs "gcc") "/lib")))
                        ;; XXX: "g++ -v" is broken (see also libstdc++ above).
                        (substitute* "libcc1/configure"
                          (("g\\+\\+ -v") "true"))
                        #t)))
                   (_ identity))
                (add-after 'install 'symlink-libgcc_eh
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((out (assoc-ref outputs "lib")))
                      ;; Glibc wants to link against libgcc_eh, so provide
                      ;; it.
                      (with-directory-excursion
                          (string-append out "/lib/gcc/"
                                         ,(boot-triplet)
                                         "/" ,(package-version gcc))
                        (symlink "libgcc.a" "libgcc_eh.a"))
                      #t))))))))

     (inputs `(("gmp-source" ,(package-source gmp-6.0))
               ("mpfr-source" ,(package-source mpfr))
               ("mpc-source" ,(package-source mpc))
               ("binutils-cross" ,binutils-boot0)

               ;; The libstdc++ that libcc1 links against.
               ("libstdc++" ,libstdc++-boot0)

               ;; Call it differently so that the builder can check whether
               ;; the "libc" input is #f.
               ("libc-native" ,@(assoc-ref (%boot0-inputs) "libc"))
               ,@(alist-delete "libc" (%boot0-inputs))))

     ;; No need for the native-inputs to build the documentation at this stage.
     (native-inputs `()))))

(define perl-boot0
  (let ((perl (package
                (inherit perl)
                (name "perl-boot0")
                (arguments
                 ;; At the very least, this must not depend on GCC & co.
                 (let ((args `(#:validate-runpath? #f
                               #:disallowed-references
                               ,(list %bootstrap-binutils))))
                   `(,@args
                     ,@(substitute-keyword-arguments (package-arguments perl)
                         ((#:phases phases)
                          `(modify-phases ,phases
                             ;; Pthread support is missing in the bootstrap compiler
                             ;; (broken spec file), so disable it.
                             (add-before 'configure 'disable-pthreads
                               (lambda _
                                 (substitute* "Configure"
                                   (("^libswanted=(.*)pthread" _ before)
                                    (string-append "libswanted=" before)))
                                 #t))))
                         ;; Do not configure with '-Dusethreads' since pthread
                         ;; support is missing.
                         ((#:configure-flags configure-flags)
                          `(delete "-Dusethreads" ,configure-flags)))))))))
    (package-with-bootstrap-guile
     (package-with-explicit-inputs perl
                                   %boot0-inputs
                                   (current-source-location)
                                   #:guile %bootstrap-guile))))

(define m4-boot0
  (package-with-bootstrap-guile
   (package
     (inherit m4)
     (name "m4-boot0")
     (inputs (%boot0-inputs))
     (arguments
      `(#:guile ,%bootstrap-guile
        #:implicit-inputs? #f
        ,@(package-arguments m4))))))

(define bison-boot0
  ;; This Bison is needed to build MiG so we need it early in the process.
  ;; It is also needed to rebuild Bash's parser, which is modified by
  ;; its CVE patches.  Remove it when it's no longer needed.
  (let* ((bison (package (inherit bison)
                  (propagated-inputs `(("m4" ,m4)))
                  (inputs '())                    ;remove Flex...
                  (arguments
                   `(#:tests? #f                  ;... and thus disable tests

                     ;; Zero timestamps in liby.a; this must be done
                     ;; explicitly here because the bootstrap Binutils don't
                     ;; do that (default is "cru".)
                     #:make-flags `("ARFLAGS=crD"
                                    ,,(match (%current-system)
                                        ;; ranlib: '-D': No such file
                                        ((or "i686-linux" "x86_64-linux")
                                         "RANLIB=ranlib")
                                        (_
                                         "RANLIB=ranlib -D"))
                                    "V=1"))))))
    (package
      (inherit (package-with-bootstrap-guile
                (package-with-explicit-inputs bison %boot0-inputs
                                              (current-source-location)
                                              #:guile %bootstrap-guile)))
      (native-inputs `(("perl" ,perl-boot0))))))

(define flex-boot0
  ;; This Flex is needed to build MiG.
  (let* ((flex (package (inherit flex)
                 (native-inputs `(("bison" ,bison-boot0)))
                 (propagated-inputs `(("m4" ,m4)))
                 (inputs `(("indent" ,indent)))
                 (arguments '(#:tests? #f)))))
    (package-with-bootstrap-guile
     (package-with-explicit-inputs flex %boot0-inputs
                                   (current-source-location)
                                   #:guile %bootstrap-guile))))

(define linux-libre-headers-boot0
  (mlambda ()
    "Return Linux-Libre header files for the bootstrap environment."
    ;; Note: this is wrapped in a thunk to nicely handle circular dependencies
    ;; between (gnu packages linux) and this module.  Additionally, memoize
    ;; the result to play well with further memoization and code that relies
    ;; on pointer identity; see <https://bugs.gnu.org/30155>.
    (package-with-bootstrap-guile
     (package (inherit linux-libre-headers)
              (arguments `(#:guile ,%bootstrap-guile
                           #:implicit-inputs? #f
                           ,@(package-arguments linux-libre-headers)))
              (native-inputs
               `(("perl" ,perl-boot0)
                 ,@(%boot0-inputs)))))))

(define gnumach-headers-boot0
  (package-with-bootstrap-guile
   (package-with-explicit-inputs gnumach-headers
                                 (%boot0-inputs)
                                 (current-source-location)
                                 #:guile %bootstrap-guile)))

(define mig-boot0
  (let* ((mig (package (inherit mig)
                 (native-inputs `(("bison" ,bison-boot0)
                                  ("flex" ,flex-boot0)))
                 (inputs `(("flex" ,flex-boot0)))
                 (arguments
                  `(#:configure-flags
                    `(,(string-append "LDFLAGS=-Wl,-rpath="
                                      (assoc-ref %build-inputs "flex") "/lib/")))))))
    (package-with-bootstrap-guile
     (package-with-explicit-inputs mig (%boot0-inputs)
                                   (current-source-location)
                                   #:guile %bootstrap-guile))))

(define hurd-headers-boot0
  (let ((hurd-headers (package (inherit hurd-headers)
                        (native-inputs `(("mig" ,mig-boot0)))
                        (inputs '()))))
    (package-with-bootstrap-guile
     (package-with-explicit-inputs hurd-headers (%boot0-inputs)
                                   (current-source-location)
                                   #:guile %bootstrap-guile))))

(define hurd-minimal-boot0
  (let ((hurd-minimal (package (inherit hurd-minimal)
                        (native-inputs `(("mig" ,mig-boot0)))
                        (inputs '()))))
    (package-with-bootstrap-guile
     (package-with-explicit-inputs hurd-minimal (%boot0-inputs)
                                   (current-source-location)
                                   #:guile %bootstrap-guile))))

(define hurd-core-headers-boot0
  (mlambda ()
    "Return the Hurd and Mach headers as well as initial Hurd libraries for
the bootstrap environment."
    (package-with-bootstrap-guile
     (package (inherit hurd-core-headers)
              (arguments `(#:guile ,%bootstrap-guile
                           ,@(package-arguments hurd-core-headers)))
              (inputs
               `(("gnumach-headers" ,gnumach-headers-boot0)
                 ("hurd-headers" ,hurd-headers-boot0)
                 ("hurd-minimal" ,hurd-minimal-boot0)
                 ,@(%boot0-inputs)))))))

(define* (kernel-headers-boot0 #:optional (system (%current-system)))
  (match system
    ("i586-gnu" (hurd-core-headers-boot0))
    (_ (linux-libre-headers-boot0))))

(define texinfo-boot0
  ;; Texinfo used to build libc's manual.
  ;; We build without ncurses because it fails to build at this stage, and
  ;; because we don't need the stand-alone Info reader.
  ;; Also, use (%BOOT0-INPUTS) to avoid building Perl once more.
  (let ((texinfo (package (inherit texinfo)
                   (native-inputs '())
                   (inputs `(("perl" ,perl-boot0)))

                   ;; Some of Texinfo 6.1's tests would fail with "Couldn't
                   ;; set UTF-8 character type in locale" but we don't have a
                   ;; UTF-8 locale at this stage, so skip them.
                   (arguments '(#:tests? #f)))))
    (package-with-bootstrap-guile
     (package-with-explicit-inputs texinfo %boot0-inputs
                                   (current-source-location)
                                   #:guile %bootstrap-guile))))

(define (ld-wrapper-boot0)
  ;; We need this so binaries on Hurd will have libmachuser and libhurduser
  ;; in their RUNPATH, otherwise validate-runpath will fail.
  (make-ld-wrapper "ld-wrapper-boot0"
                   #:target boot-triplet
                   #:binutils binutils-boot0
                   #:guile %bootstrap-guile
                   #:bash (car (assoc-ref (%boot0-inputs) "bash"))
                   #:guile-for-build %bootstrap-guile))

(define (%boot1-inputs)
  ;; 2nd stage inputs.
  `(("gcc" ,gcc-boot0)
    ("ld-wrapper-cross" ,(ld-wrapper-boot0))
    ("binutils-cross" ,binutils-boot0)
    ,@(alist-delete "binutils" (%boot0-inputs))))

(define glibc-final-with-bootstrap-bash
  ;; The final libc, "cross-built".  If everything went well, the resulting
  ;; store path has no dependencies.  Actually, the really-final libc is
  ;; built just below; the only difference is that this one uses the
  ;; bootstrap Bash.
  (package-with-bootstrap-guile
   (package (inherit glibc)
     (name "glibc-intermediate")
     (arguments
      `(#:guile ,%bootstrap-guile
        #:implicit-inputs? #f

        ,@(substitute-keyword-arguments (package-arguments glibc)
            ((#:configure-flags flags)
             `(append (list ,(string-append "--host=" (boot-triplet))
                            ,(string-append "--build="
                                            (nix-system->gnu-triplet))

                            ;; Build Sun/ONC RPC support.  In particular,
                            ;; install rpc/*.h.
                            "--enable-obsolete-rpc")
                      ,flags))
            ((#:phases phases)
             `(modify-phases ,phases
                (add-before 'configure 'pre-configure
                  (lambda* (#:key inputs #:allow-other-keys)
                    ;; Don't clobber CPATH with the bootstrap libc.
                    (setenv "NATIVE_CPATH" (getenv "CPATH"))
                    (unsetenv "CPATH")

                    ;; Tell 'libpthread' where to find 'libihash' on Hurd systems.
                    ,@(if (hurd-triplet? (%current-system))
                          `((substitute* "libpthread/Makefile"
                              (("LDLIBS-pthread.so =.*")
                               (string-append "LDLIBS-pthread.so = "
                                              (assoc-ref %build-inputs "kernel-headers")
                                              "/lib/libihash.a\n"))))
                          '())

                    ;; 'rpcgen' needs native libc headers to be built.
                    (substitute* "sunrpc/Makefile"
                      (("sunrpc-CPPFLAGS =.*" all)
                       (string-append "CPATH = $(NATIVE_CPATH)\n"
                                      "export CPATH\n"
                                      all "\n")))
                    #t)))))))
     (propagated-inputs `(("kernel-headers" ,(kernel-headers-boot0))))
     (native-inputs
      `(("bison" ,bison-boot0)
        ("texinfo" ,texinfo-boot0)
        ("perl" ,perl-boot0)))
     (inputs
      `(;; The boot inputs.  That includes the bootstrap libc.  We don't want
        ;; it in $CPATH, hence the 'pre-configure' phase above.
        ,@(%boot1-inputs)

        ;; A native MiG is needed to build Glibc on Hurd.
        ,@(if (hurd-triplet? (%current-system))
              `(("mig" ,mig-boot0))
              '())

        ;; A native GCC is needed to build `cross-rpcgen'.
        ("native-gcc" ,@(assoc-ref (%boot0-inputs) "gcc"))

        ;; Here, we use the bootstrap Bash, which is not satisfactory
        ;; because we don't want to depend on bootstrap tools.
        ("static-bash" ,@(assoc-ref (%boot0-inputs) "bash")))))))

(define (cross-gcc-wrapper gcc binutils glibc bash)
  "Return a wrapper for the pseudo-cross toolchain GCC/BINUTILS/GLIBC
that makes it available under the native tool names."
  (package (inherit gcc)
    (name (string-append (package-name gcc) "-wrapped"))
    (source #f)
    (build-system trivial-build-system)
    (outputs '("out"))
    (arguments
     `(#:guile ,%bootstrap-guile
       #:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))

                   (let* ((binutils (assoc-ref %build-inputs "binutils"))
                          (gcc      (assoc-ref %build-inputs "gcc"))
                          (libc     (assoc-ref %build-inputs "libc"))
                          (bash     (assoc-ref %build-inputs "bash"))
                          (out      (assoc-ref %outputs "out"))
                          (bindir   (string-append out "/bin"))
                          (triplet  ,(boot-triplet)))
                     (define (wrap-program program)
                       ;; GCC-BOOT0 is a libc-less cross-compiler, so it
                       ;; needs to be told where to find the crt files and
                       ;; the dynamic linker.
                       (call-with-output-file program
                         (lambda (p)
                           (format p "#!~a/bin/bash
exec ~a/bin/~a-~a -B~a/lib -Wl,-dynamic-linker -Wl,~a/~a \"$@\"~%"
                                   bash
                                   gcc triplet program
                                   libc libc
                                   ,(glibc-dynamic-linker))))

                       (chmod program #o555))

                     (mkdir-p bindir)
                     (with-directory-excursion bindir
                       (for-each (lambda (tool)
                                   (symlink (string-append binutils "/bin/"
                                                           triplet "-" tool)
                                            tool))
                                 '("ar" "ranlib"))
                       (for-each wrap-program '("gcc" "g++")))

                     #t))))
    (native-inputs
     `(("binutils" ,binutils)
       ("gcc" ,gcc)
       ("libc" ,glibc)
       ("bash" ,bash)))
    (inputs '())))

(define (gcc-boot0-intermediate-wrapped)
  ;; Make the cross-tools GCC-BOOT0 and BINUTILS-BOOT0 available under the
  ;; non-cross names.
  (cross-gcc-wrapper gcc-boot0 binutils-boot0
                     glibc-final-with-bootstrap-bash
                     (car (assoc-ref (%boot1-inputs) "bash"))))

(define static-bash-for-glibc
  ;; A statically-linked Bash to be used by GLIBC-FINAL in system(3) & co.
  (let ((bash (package
                (inherit static-bash)
                (arguments
                 (substitute-keyword-arguments
                     (package-arguments static-bash)
                   ((#:guile _ #f)
                    '%bootstrap-guile)
                   ((#:configure-flags flags '())
                    ;; Add a '-L' flag so that the pseudo-cross-ld of
                    ;; BINUTILS-BOOT0 can find libc.a.
                    `(append ,flags
                             (list (string-append "LDFLAGS=-static -L"
                                                  (assoc-ref %build-inputs
                                                             "libc:static")
                                                  "/lib")))))))))
    (package-with-bootstrap-guile
     (package-with-explicit-inputs
      bash
      (lambda _
        `(("gcc" ,(gcc-boot0-intermediate-wrapped))
          ("libc" ,glibc-final-with-bootstrap-bash)
          ("libc:static" ,glibc-final-with-bootstrap-bash "static")
          ,@(fold alist-delete (%boot1-inputs)
                  '("gcc" "libc"))))
      (current-source-location)
      #:guile %bootstrap-guile))))

(define gettext-boot0
  ;; A minimal gettext used during bootstrap.
  (let ((gettext-minimal
         (package (inherit gettext-minimal)
           (name "gettext-boot0")
           (inputs '())                           ;zero dependencies
           (arguments
            (substitute-keyword-arguments
                `(#:tests? #f
                  ,@(package-arguments gettext-minimal))
              ((#:phases phases)
               `(modify-phases ,phases
                  ;; Build only the tools.
                  (add-after 'unpack 'chdir
                             (lambda _
                               (chdir "gettext-tools")
                               #t))

                  ;; Some test programs require pthreads, which we don't have.
                  (add-before 'configure 'no-test-programs
                              (lambda _
                                (substitute* "tests/Makefile.in"
                                  (("^PROGRAMS =.*$")
                                   "PROGRAMS =\n"))
                                #t))

                  ;; Don't try to link against libexpat.
                  (delete 'link-expat)
                  (delete 'patch-tests))))))))
    (package-with-bootstrap-guile
     (package-with-explicit-inputs gettext-minimal
                                   %boot1-inputs
                                   (current-source-location)
                                   #:guile %bootstrap-guile))))

(define glibc-final
  ;; The final glibc, which embeds the statically-linked Bash built above.
  ;; Use 'package/inherit' so we get the 'replacement' of 'glibc', if any.
  (let ((glibc (package-with-bootstrap-guile glibc)))
    (package/inherit glibc
      (name "glibc")
      (inputs `(("static-bash" ,static-bash-for-glibc)
                ,@(alist-delete
                   "static-bash"
                   (package-inputs glibc-final-with-bootstrap-bash))))

      ;; This time we need 'msgfmt' to install all the libc.mo files.
      (native-inputs `(,@(package-native-inputs glibc-final-with-bootstrap-bash)
                       ("gettext" ,gettext-boot0)))

      (propagated-inputs
       (package-propagated-inputs glibc-final-with-bootstrap-bash))

      ;; The final libc only refers to itself, but the 'debug' output contains
      ;; references to GCC-BOOT0 and to the Linux headers.  XXX: Would be great
      ;; if 'allowed-references' were per-output.
      (arguments
       `(#:allowed-references
         ,(cons* `(,gcc-boot0 "lib") (kernel-headers-boot0)
                 static-bash-for-glibc
                 (package-outputs glibc-final-with-bootstrap-bash))

         ,@(package-arguments glibc-final-with-bootstrap-bash))))))

(define (gcc-boot0-wrapped)
  ;; Make the cross-tools GCC-BOOT0 and BINUTILS-BOOT0 available under the
  ;; non-cross names.
  (cross-gcc-wrapper gcc-boot0 binutils-boot0 glibc-final
                     (car (assoc-ref (%boot1-inputs) "bash"))))

(define (%boot2-inputs)
  ;; 3rd stage inputs.
  `(("libc" ,glibc-final)
    ("libc:static" ,glibc-final "static")
    ("gcc" ,(gcc-boot0-wrapped))
    ,@(fold alist-delete (%boot1-inputs) '("libc" "gcc" "linux-libre-headers"))))

(define binutils-final
  (package-with-bootstrap-guile
   (package (inherit binutils)
     (arguments
      `(#:guile ,%bootstrap-guile
        #:implicit-inputs? #f
        #:allowed-references ("out" ,glibc-final)
        ,@(package-arguments binutils)))
     (inputs (%boot2-inputs)))))

(define libstdc++
  ;; Intermediate libstdc++ that will allow us to build the final GCC
  ;; (remember that GCC-BOOT0 cannot build libstdc++.)
  (let ((lib (package-with-bootstrap-guile (make-libstdc++ gcc))))
    (package
      (inherit lib)
      (arguments
       `(#:guile ,%bootstrap-guile
         #:implicit-inputs? #f
         #:allowed-references ("out")

         ;; XXX: libstdc++.so NEEDs ld.so for some reason.
         #:validate-runpath? #f

         ;; All of the package arguments from 'make-libstdc++
         ;; except for the configure-flags.
         ,@(package-arguments lib)
         #:configure-flags `("--disable-shared"
                             "--disable-libstdcxx-threads"
                             "--disable-libstdcxx-pch"
                             ,(string-append "--with-gxx-include-dir="
                                             (assoc-ref %outputs "out")
                                             "/include"))))
      (outputs '("out"))
      (inputs (%boot2-inputs))
      (synopsis "GNU C++ standard library (intermediate)"))))

(define zlib-final
  ;; Zlib used by GCC-FINAL.
  (package-with-bootstrap-guile
   (package
     (inherit zlib)
     (arguments
      `(#:guile ,%bootstrap-guile
        #:implicit-inputs? #f
        #:allowed-references ("out" ,glibc-final)
        ,@(package-arguments zlib)))
     (inputs (%boot2-inputs)))))

(define (ld-wrapper-boot3)
  ;; A linker wrapper that uses the bootstrap Guile.
  (make-ld-wrapper "ld-wrapper-boot3"
                   #:binutils binutils-final
                   #:guile %bootstrap-guile
                   #:bash (car (assoc-ref (%boot2-inputs) "bash"))
                   #:guile-for-build %bootstrap-guile))

(define gcc-final
  ;; The final GCC.
  (package (inherit gcc-boot0)
    (name "gcc")

    ;; XXX: Currently #:allowed-references applies to all the outputs but the
    ;; "debug" output contains disallowed references, notably
    ;; linux-libre-headers.  Disable the debugging output to work around that.
    (outputs (delete "debug" (package-outputs gcc-boot0)))

    (arguments
     `(#:guile ,%bootstrap-guile
       #:implicit-inputs? #f

       #:allowed-references ("out" "lib" ,zlib-final
                             ,glibc-final ,static-bash-for-glibc)

       ;; Things like libasan.so and libstdc++.so NEED ld.so for some
       ;; reason, but it is not in their RUNPATH.  This is a false
       ;; positive, so turn it off.
       #:validate-runpath? #f

       ,@(substitute-keyword-arguments (package-arguments gcc)
           ((#:make-flags flags)
            ;; Since $LIBRARY_PATH is not honored, add the relevant flags.
            `(let ((zlib (assoc-ref %build-inputs "zlib")))
               (map (lambda (flag)
                      (if (string-prefix? "LDFLAGS=" flag)
                          (string-append flag " -L"
                                         (assoc-ref %build-inputs "libstdc++")
                                         "/lib -L" zlib "/lib -Wl,-rpath="
                                         zlib "/lib")
                          flag))
                    ,flags)))
           ;; Build again GMP & co. within GCC's build process, because it's hard
           ;; to do outside (because GCC-BOOT0 is a cross-compiler, and thus
           ;; doesn't honor $LIBRARY_PATH, which breaks `gnu-build-system'.)
           ((#:phases phases)
            `(modify-phases ,phases
                (add-after 'unpack 'unpack-gmp&co
                  (lambda* (#:key inputs #:allow-other-keys)
                    (let ((gmp  (assoc-ref %build-inputs "gmp-source"))
                          (mpfr (assoc-ref %build-inputs "mpfr-source"))
                          (mpc  (assoc-ref %build-inputs "mpc-source")))

                      ;; To reduce the set of pre-built bootstrap inputs, build
                      ;; GMP & co. from GCC.
                      (for-each (lambda (source)
                                  (invoke "tar" "xvf" source))
                                (list gmp mpfr mpc))

                      ;; Create symlinks like `gmp' -> `gmp-x.y.z'.
                      ,@(map (lambda (lib)
                               ;; Drop trailing letters, as gmp-6.0.0a unpacks
                               ;; into gmp-6.0.0.
                               `(symlink ,(string-trim-right
                                           (package-full-name lib "-")
                                           char-set:letter)
                                         ,(package-name lib)))
                             (list gmp-6.0 mpfr mpc))
                      #t))))))))

    ;; This time we want Texinfo, so we get the manual.  Add
    ;; STATIC-BASH-FOR-GLIBC so that it's used in the final shebangs of
    ;; scripts such as 'mkheaders' and 'fixinc.sh' (XXX: who cares about these
    ;; scripts?).
    (native-inputs `(("texinfo" ,texinfo-boot0)
                     ("perl" ,perl-boot0) ;for manpages
                     ("static-bash" ,static-bash-for-glibc)
                     ,@(package-native-inputs gcc-boot0)))

    (inputs `(("gmp-source" ,(bootstrap-origin (package-source gmp-6.0)))
              ("mpfr-source" ,(package-source mpfr))
              ("mpc-source" ,(package-source mpc))
              ("ld-wrapper" ,(ld-wrapper-boot3))
              ("binutils" ,binutils-final)
              ("libstdc++" ,libstdc++)
              ("zlib" ,zlib-final)
              ,@(%boot2-inputs)))))

(define (%boot3-inputs)
  ;; 4th stage inputs.
  `(("gcc" ,gcc-final)
    ("ld-wrapper" ,(ld-wrapper-boot3))
    ,@(alist-delete "gcc" (%boot2-inputs))))

(define bash-final
  ;; Link with `-static-libgcc' to make sure we don't retain a reference
  ;; to the bootstrap GCC.  Use "bash-minimal" to avoid an extra dependency
  ;; on Readline and ncurses.
  (let ((bash (package
                (inherit bash-minimal)
                (arguments
                 `(#:disallowed-references
                   ,(assoc-ref (%boot3-inputs) "coreutils&co")
                   ,@(package-arguments bash-minimal))))))
    (package-with-bootstrap-guile
     (package-with-explicit-inputs (static-libgcc-package bash)
                                   %boot3-inputs
                                   (current-source-location)
                                   #:guile %bootstrap-guile))))

(define (%boot4-inputs)
  ;; Now use the final Bash.
  `(("bash" ,bash-final)
    ,@(alist-delete "bash" (%boot3-inputs))))

(define-public guile-final
  ;; This package must be public because other modules refer to it.  However,
  ;; mark it as hidden so that 'fold-packages' ignores it.
  (package-with-bootstrap-guile
   (package-with-explicit-inputs (hidden-package guile-2.2/fixed)
                                 %boot4-inputs
                                 (current-source-location)
                                 #:guile %bootstrap-guile)))

(define glibc-utf8-locales-final
  ;; Now that we have GUILE-FINAL, build the UTF-8 locales.  They are needed
  ;; by the build processes afterwards so their 'scm_to_locale_string' works
  ;; with the full range of Unicode codepoints (remember
  ;; 'scm_to_locale_string' is called every time a string is passed to a C
  ;; function.)
  (package
    (inherit glibc-utf8-locales)
    (inputs `(("glibc" ,glibc-final)
              ("gzip"
               ,(package-with-explicit-inputs gzip %boot4-inputs
                                              (current-source-location)
                                              #:guile %bootstrap-guile))))))

(define-public ld-wrapper
  ;; The final 'ld' wrapper, which uses the final Guile and Binutils.
  (make-ld-wrapper "ld-wrapper"
                   #:binutils binutils-final
                   #:guile guile-final
                   #:bash bash-final))

(define (%boot5-inputs)
  ;; Now with UTF-8 locales.  Remember that the bootstrap binaries were built
  ;; with an older libc, which cannot load the new locale format.  See
  ;; <https://lists.gnu.org/archive/html/guix-devel/2015-08/msg00737.html>.
  `(("locales" ,glibc-utf8-locales-final)
    ,@(%boot4-inputs)))

(define gnu-make-final
  ;; The final GNU Make, which uses the final Guile.
  (package-with-bootstrap-guile
   (package-with-explicit-inputs gnu-make
                                 (lambda _
                                   `(("guile" ,guile-final)
                                     ,@(%boot5-inputs)))
                                 (current-source-location))))

(define coreutils-final
  ;; The final Coreutils.  Treat them specially because some packages, such as
  ;; Findutils, keep a reference to the Coreutils they were built with.
  (package-with-bootstrap-guile
   (package-with-explicit-inputs coreutils
                                 %boot5-inputs
                                 (current-source-location)

                                 ;; Use the final Guile, linked against the
                                 ;; final libc with working iconv, so that
                                 ;; 'substitute*' works well when touching
                                 ;; test files in Gettext.
                                 #:guile guile-final)))

(define grep-final
  ;; The final grep.  Gzip holds a reference to it (via zgrep), so it must be
  ;; built before gzip.
  (let ((grep (package-with-bootstrap-guile
               (package-with-explicit-inputs grep %boot5-inputs
                                             (current-source-location)
                                             #:guile guile-final))))
    (package/inherit grep
                     (inputs (alist-delete "pcre" (package-inputs grep)))
                     (native-inputs `(("perl" ,perl-boot0))))))

(define (%boot6-inputs)
  ;; Now use the final Coreutils.
  `(("coreutils" ,coreutils-final)
    ("grep" ,grep-final)
    ,@(%boot5-inputs)))

(define sed-final
  ;; The final sed.
  (let ((sed (package-with-bootstrap-guile
              (package-with-explicit-inputs sed %boot6-inputs
                                            (current-source-location)
                                            #:guile guile-final))))
    (package/inherit sed (native-inputs `(("perl" ,perl-boot0))))))

(define-public %final-inputs
  ;; Final derivations used as implicit inputs by 'gnu-build-system'.  We
  ;; still use 'package-with-bootstrap-guile' so that the bootstrap tools are
  ;; used for origins that have patches, thereby avoiding circular
  ;; dependencies.
  (let ((finalize (compose package-with-bootstrap-guile
                           (cut package-with-explicit-inputs <> %boot6-inputs
                                (current-source-location)))))
    `(,@(map (match-lambda
              ((name package)
               (list name (finalize package))))
             `(("tar" ,tar)
               ("gzip" ,gzip)
               ("bzip2" ,bzip2)
               ("xz" ,xz)
               ("file" ,file)
               ("diffutils" ,diffutils)
               ("patch" ,patch)
               ("findutils" ,findutils)
               ("gawk" ,gawk)))
      ("sed" ,sed-final)
      ("grep" ,grep-final)
      ("coreutils" ,coreutils-final)
      ("make" ,gnu-make-final)
      ("bash" ,bash-final)
      ("ld-wrapper" ,ld-wrapper)
      ("binutils" ,binutils-final)
      ("gcc" ,gcc-final)
      ("libc" ,glibc-final)
      ("libc:static" ,glibc-final "static")
      ("locales" ,glibc-utf8-locales-final))))

(define-public canonical-package
  (let ((name->package (fold (lambda (input result)
                               (match input
                                 ((_ package . outputs)
                                  (vhash-cons (package-full-name package)
                                              package result))))
                             vlist-null
                             `(("guile" ,guile-final)
                               ,@%final-inputs))))
    (lambda (package)
      "Return the 'canonical' variant of PACKAGE---i.e., if PACKAGE is one of
the implicit inputs of 'gnu-build-system', return that one, otherwise return
PACKAGE.

The goal is to avoid duplication in cases like GUILE-FINAL vs. GUILE-2.2,
COREUTILS-FINAL vs. COREUTILS, etc."
      ;; XXX: This doesn't handle dependencies of the final inputs, such as
      ;; libunistring, GMP, etc.
      (match (vhash-assoc (package-full-name package) name->package)
        ((_ . canon)
         ;; In general we want CANON, except if we're cross-compiling: CANON
         ;; uses explicit inputs, so it is "anchored" in the bootstrapped
         ;; process, with dependencies on things that cannot be
         ;; cross-compiled.
         (if (%current-target-system)
             package
             canon))
        (_ package)))))


;;;
;;; GCC toolchain.
;;;

(define (make-gcc-toolchain gcc)
  "Return a complete toolchain for GCC."
  (package
    (name "gcc-toolchain")
    (version (package-version gcc))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
                   (use-modules (ice-9 match)
                                (srfi srfi-26)
                                (guix build union))

                   (let ((out (assoc-ref %outputs "out")))

                     (match %build-inputs
                       (((names . directories) ...)
                        (union-build out directories)))

                     (union-build (assoc-ref %outputs "debug")
                                  (list (assoc-ref %build-inputs
                                                   "libc-debug")))
                     (union-build (assoc-ref %outputs "static")
                                  (list (assoc-ref %build-inputs
                                                   "libc-static")))
                     #t))))

    (native-search-paths (package-native-search-paths gcc))
    (search-paths (package-search-paths gcc))

    (license (package-license gcc))
    (synopsis "Complete GCC tool chain for C/C++ development")
    (description
     "This package provides a complete GCC tool chain for C/C++ development to
be installed in user profiles.  This includes GCC, as well as libc (headers
and binaries, plus debugging symbols in the 'debug' output), and Binutils.")
    (home-page "https://gcc.gnu.org/")
    (outputs '("out" "debug" "static"))

    ;; The main raison d'être of this "meta-package" is (1) to conveniently
    ;; install everything that we need, and (2) to make sure ld-wrapper comes
    ;; before Binutils' ld in the user's profile.
    (inputs `(("gcc" ,gcc)
              ("ld-wrapper" ,(car (assoc-ref %final-inputs "ld-wrapper")))
              ("binutils" ,binutils-final)
              ("libc" ,glibc-final)
              ("libc-debug" ,glibc-final "debug")
              ("libc-static" ,glibc-final "static")))))

(define-public gcc-toolchain-4.8
  (make-gcc-toolchain gcc-4.8))

(define-public gcc-toolchain-4.9
  (make-gcc-toolchain gcc-4.9))

(define-public gcc-toolchain
  (make-gcc-toolchain gcc-final))

(define-public gcc-toolchain-5
  gcc-toolchain)

(define-public gcc-toolchain-6
  (make-gcc-toolchain gcc-6))

(define-public gcc-toolchain-7
  (make-gcc-toolchain gcc-7))

(define-public gcc-toolchain-8
  (make-gcc-toolchain gcc-8))

;;; commencement.scm ends here
