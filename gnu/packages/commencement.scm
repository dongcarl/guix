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

(define mes-boot0
  (let ((version "0.19")
        (revision "0")
        (commit #f))
    (package
      (inherit mes)
      (name "mes-boot0")
      (version (if commit (string-append version "-" revision "." (string-take commit 7))
                   version))
      (source (origin
                (method url-fetch)
                (uri (string-append "mirror://gnu/mes/"
                                    "mes-" version ".tar.gz"))
                (sha256
                 (base32
                  "15h4yhaywdc0djpjlin2jz1kzahpqxfki0r0aav1qm9nxxmnp1l0"))))
      (native-inputs '())
      (propagated-inputs '()))))

(define nyacc-boot
  (let ((version "0.86.0")
        (revision "0")
        (commit #f))
    (package
      (inherit nyacc)
      (name "nyacc-boot")
      (version
       (if commit
           (string-append version "-" revision "." (string-take commit 7))
           version))
      (source
       (if commit
           (origin
             (method url-fetch)
             (uri (string-append "https://gitlab.com/janneke/nyacc"
                                 "/-/archive/" commit
                                 "/nyacc-" commit ".tar.gz"))
             (sha256
              (base32
               "0dlcqmchhl57nh7f0v6qb1kkbi7zbs3b185hcqv57fhb60b7rgcq")))
           (package-source nyacc))))))

(define mes-boot
  (package-with-bootstrap-guile
   (package
     (inherit mes-boot0)
     (name "mes-boot")
     (inputs '())
     (propagated-inputs '())
     (native-inputs
      `(("mescc-tools" ,%bootstrap-mescc-tools)
        ("nyacc-source" ,(package-source nyacc))

        ("coreutils" , %bootstrap-coreutils&co)
        ("bootstrap-mes" ,%bootstrap-mes)))
     (arguments
      `(#:implicit-inputs? #f
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
                   (mkdir-p "nyacc-source")
                   (invoke "tar" "--strip=1" "-C" "nyacc-source" "-xvf" nyacc-source)
                   (symlink (string-append bootstrap-mes "/share/mes/lib") "mes-seed")
                   #t)))))
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
                (setenv "MES_PREFIX" "mes")
               (invoke "sh" "check.sh"))))
          (replace 'install
            (lambda _
              (invoke "sh" "install.sh"))))))
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

(define tcc-boot0
  ;; Pristine tcc cannot be built by MesCC, we are keeping a delta of 11
  ;; patches.  In a very early and rough form they were presented to the
  ;; TinyCC developers, who at the time showed no interest in supporting the
  ;; bootstrappable effort; we will try again later.  These patches have been
  ;; ported to 0.9.27, alas the resulting tcc is buggy.  Once MesCC is more
  ;; mature, this package should use the 0.9.27 sources (or later).
  (let ((version "0.9.26")
        (revision "6")
        (commit "c004e9a34fb026bb44d211ab98bb768e79900eef"))
    (package-with-bootstrap-guile
     (package
       (inherit tcc)
       (name "tcc-boot0")
       (version (string-append version "-" revision "." (string-take commit 7)))
       (source (origin
                 (method url-fetch)
                 (uri (string-append "https://gitlab.com/janneke/tinycc"
                                     "/-/archive/" commit
                                     "/tinycc-" commit ".tar.gz"))
                 (sha256
                  (base32
                   "1hmzn1pq0x22ppd80hyrn5qzqq94mxd0ychzj6vrr2vnj2frjv5b"))))
       (build-system gnu-build-system)
       (supported-systems '("i686-linux" "x86_64-linux"))
       (inputs '())
       (propagated-inputs '())
       (native-inputs
        `(("mes" ,mes-boot)
          ("mescc-tools" ,%bootstrap-mescc-tools)
          ("nyacc-source" ,(package-source nyacc))

          ("coreutils" , %bootstrap-coreutils&co)
          ("bootstrap-mes" ,%bootstrap-mes)))
       (arguments
        `(#:implicit-inputs? #f
          #:guile ,%bootstrap-guile
          #:strip-binaries? #f ; binutil's strip b0rkes MesCC/M1/hex2 binaries
          #:phases
          (modify-phases %standard-phases
            (add-after 'unpack 'unpack-seeds
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((coreutils (assoc-ref %build-inputs "coreutils"))
                       (nyacc-source (assoc-ref %build-inputs "nyacc-source"))
                       (bootstrap-mes (assoc-ref %build-inputs "bootstrap-mes")))
                  (setenv "PATH" (string-append
                                  coreutils "/bin"))
                  (format (current-error-port) "PATH=~s\n" (getenv "PATH"))
                  (with-directory-excursion ".."
                    (and
                     (mkdir-p "nyacc-source")
                     (invoke "tar" "--strip=1" "-C" "nyacc-source"
                             "-xvf" nyacc-source)
                     (symlink (string-append bootstrap-mes "/share/mes/lib") "mes-seed")
                     #t)))))
            (replace 'configure
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref %outputs "out"))
                       (dir (with-directory-excursion ".." (getcwd)))
                       (coreutils (assoc-ref %build-inputs "coreutils"))
                       (mes (assoc-ref %build-inputs "mes"))
                       (mescc-tools (assoc-ref %build-inputs "mescc-tools"))
                       (libc (assoc-ref %build-inputs "libc"))
                       (interpreter (if libc
                                        ;; also for x86_64-linux, we are still on i686-linux
                                        (string-append libc ,(glibc-dynamic-linker "i686-linux"))
                                        (string-append mes "/lib/mes-loader"))))
                  (setenv "PATH" (string-append
                                  coreutils "/bin"
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
                  (invoke "sh" "configure"
                          "--prefix=$PREFIX"
                          (string-append "--elfinterp=" interpreter)
                          "--crtprefix=."
                          "--tccdir=."))))
            (replace 'build
              (lambda _
                (substitute* "bootstrap.sh"
                  (("^    cmp") "#    cmp"))
                (invoke "sh" "bootstrap.sh")))
            (replace 'check
              (lambda _
                (setenv "DIFF" "diff.scm")
                (setenv "OBJDUMP" "true")
                ;; fail fast tests
                ;; (invoke "sh" "test.sh" "mes/scaffold/tests/30-strlen")
                ;; (invoke "sh" "-x" "test.sh" "mes/scaffold/tinycc/00_assignment")
                (setenv "TCC" "./tcc")
                (invoke "sh" "check.sh")))
            (replace 'install
              (lambda _
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

(define tcc-boot
  (package-with-bootstrap-guile
   (package
     (inherit tcc-boot0)
     (name "tcc-boot")
     (version "0.9.27")
     (source (origin
               (inherit (package-source tcc))
               (patches (search-patches "tcc-boot-0.9.27.patch"))))
     (build-system gnu-build-system)
     (inputs '())
     (propagated-inputs '())
     (native-inputs
      `(("mes" ,mes-boot)
        ("tcc" ,tcc-boot0)

        ("coreutils" , %bootstrap-coreutils&co)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:strip-binaries? #f            ; binutil's strip b0rkes MesCC/M1/hex2
                                        ; binaries, tcc-boot also comes with
                                        ; MesCC/M1/hex2-built binaries
        #:phases
        (modify-phases %standard-phases
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
                ;; unpack
                (setenv "PATH" (string-append
                                coreutils "/bin"
                                ":" tcc "/bin"))
                (format (current-error-port) "PATH=~s\n" (getenv "PATH"))
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

(define make-mesboot0
  (package-with-bootstrap-guile
   (package
     (inherit gnu-make)
     (name "make-mesboot0")
     (version "3.80")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/make/make-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "1pb7fb7fqf9wz9najm85qdma1xhxzf1rhj5gwrlzdsz2zm0hpcv4"))))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("tcc" ,tcc-boot)

                      ("bash" ,%bootstrap-coreutils&co)
                      ("coreutils" ,%bootstrap-coreutils&co)))
     (arguments
      `(#:implicit-inputs? #f
        #:tests? #f                     ; check depends on perl
        #:guile ,%bootstrap-guile
        #:configure-flags `("CC=tcc -DO_RDONLY=0"
                            "LD=tcc"
                            "--disable-nls")
        #:phases
        (modify-phases %standard-phases
          (add-after 'configure 'configure-fixup
            (lambda _
              (substitute* "build.sh"
                (("^REMOTE=.*") "REMOTE=stub\n")
                (("^extras=.*") "extras=getloadavg.c\n"))
              (substitute* "make.h"
                (("^extern long int lseek.*" all) (string-append "// " all)))
              #t))
          (delete 'patch-generated-file-shebangs) ; no perl
          (replace 'build
            (lambda _
              (invoke "sh" "./build.sh")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
                (install-file "make" bin))))))))))

(define diffutils-mesboot
  (package-with-bootstrap-guile
   (package
     (inherit diffutils)
     (name "diffutils-mesboot")
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
     (native-inputs `(("mes" ,mes-boot)
                      ("tcc" ,tcc-boot)

                      ("bash" ,%bootstrap-coreutils&co)
                      ("coreutils" ,%bootstrap-coreutils&co)
                      ("make" ,make-mesboot0)))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:parallel-build? #f
        #:tests? #f           ; check is naive, also checks non-built PROGRAMS
        #:strip-binaries? #f  ; no strip yet
        #:phases
        (modify-phases %standard-phases
          ;; diffutils-2.7 needs more traditional configure
          (replace 'configure
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (bash (assoc-ref %build-inputs "bash")))
                (setenv "CONFIG_SHELL" (string-append bash "/bin/sh"))
                (setenv "CC" "tcc")
                (setenv "LD" "tcc")
                (invoke "./configure" (string-append "--prefix=" out)))))
          (add-before 'configure 'remove-diff3-sdiff
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "Makefile.in"
                (("PROGRAMS = .*" all) "PROGRAMS = cmp diff"))))))))))


(define binutils-mesboot0
  (package-with-bootstrap-guile
   (package
     (inherit binutils)
     (name "binutils-mesboot0")
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
     (native-inputs `(("tcc" ,tcc-boot)

                      ("bash" ,%bootstrap-coreutils&co)
                      ("coreutils" ,%bootstrap-coreutils&co)
                      ("diffutils" ,diffutils-mesboot)
                      ("make" ,make-mesboot0)))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:tests? #f                     ; runtest: command not found
        #:parallel-build? #f
        #:strip-binaries? #f            ; no strip yet
        #:configure-flags
        (let ((cppflags (string-append " -D __GLIBC_MINOR__=6"
                                       " -D MES_BOOTSTRAP=1"))
              (bash (assoc-ref %build-inputs "bash")))
          `(,(string-append "CONFIG_SHELL=" bash "/bin/sh")
            ,(string-append "CPPFLAGS=" cppflags)
            "AR=tcc -ar"
            "CXX=false"
            "RANLIB=true"
            ,(string-append "CC=tcc" cppflags)
            "--disable-nls"
            "--disable-shared"
            "--disable-werror"
            "--build=i686-unknown-linux-gnu"
            "--host=i686-unknown-linux-gnu"
            "--with-sysroot=/")))))))

(define gcc-core-mesboot
  ;; Gcc-2.95.3 is the most recent GCC that is supported by what the Mes C
  ;; Library v0.16 offers.  Gcc-3.x (and 4.x) place higher demands on a C
  ;; library, such as dir.h/struct DIR/readdir, locales, signals...  Also,
  ;; with gcc-2.95.3, binutils-boot-2.20.1a and glibc-2.2.5 we found a GNU
  ;; toolchain triplet "that works".
  (package-with-bootstrap-guile
   (package
     (inherit gcc)
     (name "gcc-core-mesboot")
     (version "2.95.3")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/gcc/gcc-2.95.3/gcc-core-"
                                   version
                                   ".tar.gz"))
               (patches (search-patches "gcc-boot-2.95.3.patch"))
               (sha256
                (base32
                 "1xvfy4pqhrd5v2cv8lzf63iqg92k09g6z9n2ah6ndd4h17k1x0an"))))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("binutils" ,binutils-mesboot0)
                      ("tcc" ,tcc-boot)

                      ("bash" ,%bootstrap-coreutils&co)
                      ("coreutils" ,%bootstrap-coreutils&co)
                      ("diffutils" ,diffutils-mesboot)
                      ("make" ,make-mesboot0)))
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
        #:make-flags (list
                      "CC=tcc -static -D __GLIBC_MINOR__=6"
                      "OLDCC=tcc -static -D __GLIBC_MINOR__=6"
                      "CC_FOR_BUILD=tcc -static -D __GLIBC_MINOR__=6"
                      "AR=ar"
                      "RANLIB=ranlib"
                      (string-append "LIBGCC2_INCLUDES=-I "
                                     (assoc-ref %build-inputs "tcc")
                                     "/include")
                      "LANGUAGES=c"
                      (string-append "BOOT_LDFLAGS="
                                     " -B" (assoc-ref %build-inputs "tcc")
                                     "/lib/"))
        #:modules ((guix build gnu-build-system)
                   (guix build utils)
                   (srfi srfi-1))
        #:phases
        (modify-phases %standard-phases
          ;; gcc-2.95.3 needs more traditional configure
          (add-before 'configure 'setenv
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (bash (assoc-ref %build-inputs "bash"))
                    (tcc (assoc-ref %build-inputs "tcc"))
                    (cppflags " -D __GLIBC_MINOR__=6"))
                (setenv "CONFIG_SHELL" (string-append bash "/bin/sh"))
                (setenv "CPPFLAGS" cppflags)
                (setenv "CC" (string-append "tcc" cppflags))
                (setenv "CC_FOR_BUILD" (string-append "tcc" cppflags))
                (setenv "CPP" (string-append "tcc -E" cppflags))
                (with-output-to-file "config.cache"
                  (lambda _
                    (display "
ac_cv_c_float_format='IEEE (little-endian)'
"))))))
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

(define mesboot-headers
  (package-with-bootstrap-guile
   (package
     (inherit mes-boot)
     (name "mesboot-headers")
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("coreutils" ,%bootstrap-coreutils&co)
                      ("headers" ,%bootstrap-linux-libre-headers)))
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

(define glibc-mesboot0
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
               (patches (search-patches "glibc-boot-2.2.5.patch"))
               (sha256
                (base32
                 "1vl48i16gx6h68whjyhgnn1s57vqq32f9ygfa2fls7pdkbsqvp2q"))))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (inputs '())
     (propagated-inputs '())
     (native-inputs `(("binutils" ,binutils-mesboot0)
                      ("gcc" ,gcc-core-mesboot)

                      ("bash" ,%bootstrap-coreutils&co)
                      ("coreutils" ,%bootstrap-coreutils&co)
                      ("diffutils" ,diffutils-mesboot)
                      ("headers" ,mesboot-headers)
                      ("make" ,make-mesboot0)))
     (outputs '("out"))
     (arguments
      `(#:implicit-inputs? #f
        #:guile ,%bootstrap-guile
        #:tests? #f
        #:strip-binaries? #f
        #:parallel-build? #f    ; gcc-2.95.3 ICEs on massively parallel builds
        #:make-flags (list (string-append
                            "SHELL="
                            (assoc-ref %build-inputs "bash")
                            "/bin/sh"))
        #:configure-flags
        (let ((out (assoc-ref %outputs "out"))
              (headers (assoc-ref %build-inputs "headers")))
          (list
           "--disable-shared"
           "--enable-static"
           "--disable-sanity-checks"
           "--build=i686-unknown-linux-gnu"
           "--host=i686-unknown-linux-gnu"
           (string-append "--with-headers=" headers "/include")
           "--enable-static-nss"
           "--without-__thread"
           "--without-cvs"
           "--without-gd"
           "--without-tls"
           (string-append "--prefix=" out)))
        #:phases
        (modify-phases %standard-phases
          (add-before 'configure 'setenv
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bash (assoc-ref %build-inputs "bash"))
                     (gcc (assoc-ref %build-inputs "gcc"))
                     (headers (assoc-ref %build-inputs "headers"))
                     (cppflags (string-append
                                ;;" -D __STDC__=1"
                                " -D MES_BOOTSTRAP=1"
                                " -D BOOTSTRAP_GLIBC=1"))
                     (cflags (string-append " -L " (getcwd))))
                (setenv "CONFIG_SHELL" (string-append bash "/bin/sh"))
                (setenv "SHELL" (getenv "CONFIG_SHELL"))
                (setenv "CPP" (string-append gcc "/bin/gcc -E " cppflags))
                (setenv "CC" (string-append gcc "/bin/gcc " cppflags cflags))
                #t)))
          ;; glibc-2.2.5 needs a more classic invocation of configure
          ;; configure: warning: CONFIG_SHELL=/gnu/store/kpxi8h3669afr9r1bgvaf9ij3y4wdyyn-bash-minimal-4.4.12/bin/bash: invalid host type
          (replace 'configure
            (lambda* (#:key configure-flags #:allow-other-keys)
              (format (current-error-port)
                      "running ./configure ~a\n" (string-join configure-flags))
              (apply invoke "./configure" configure-flags))))))
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
     (inherit gcc-core-mesboot)
     (name "gcc-mesboot0")
     (native-inputs `(("binutils" ,binutils-mesboot0)
                      ("gcc" ,gcc-core-mesboot)
                      ("libc" ,glibc-mesboot0)

                      ("bash" ,%bootstrap-coreutils&co)
                      ("coreutils" ,%bootstrap-coreutils&co)
                      ("diffutils" ,diffutils-mesboot)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)
                      ("make" ,make-mesboot0)))
     (arguments
      (substitute-keyword-arguments (package-arguments gcc-core-mesboot)
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

(define binutils-mesboot
  (package-with-bootstrap-guile
   (package
     (inherit binutils-mesboot0)
     (name "binutils-mesboot")
     (native-inputs `(("binutils" ,binutils-mesboot0)
                      ("libc" ,glibc-mesboot0)
                      ("gcc" ,gcc-mesboot0)

                      ("bash" ,%bootstrap-coreutils&co)
                      ("coreutils" ,%bootstrap-coreutils&co)
                      ("diffutils" ,diffutils-mesboot)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)
                      ("make" ,make-mesboot0)))
     (arguments
      (substitute-keyword-arguments (package-arguments binutils-mesboot0)
        ((#:configure-flags configure-flags)
         '(list "--disable-nls"
                "--disable-shared"
                "--disable-werror"
                "--build=i686-unknown-linux-gnu"
                "--host=i686-unknown-linux-gnu"
                "--with-sysroot=/")))))))

(define make-mesboot
  (package-with-bootstrap-guile
   (package
     (inherit make-mesboot0)
     (name "make-mesboot")
     (version "3.82")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/make/make-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "1rs2f9hmvy3q6zkl15jnlmnpgffm0bhw5ax0h5c7q604wqrip69x"))))
     (native-inputs `(("binutils" ,binutils-mesboot0)
                      ("libc" ,glibc-mesboot0)
                      ("gcc" ,gcc-mesboot0)
                      ("make" ,make-mesboot0)

                      ("bash" ,%bootstrap-coreutils&co)
                      ("coreutils" ,%bootstrap-coreutils&co)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)))
     (arguments
      (substitute-keyword-arguments (package-arguments make-mesboot0)
        ((#:configure-flags configure-flags)
         `(let ((out (assoc-ref %outputs "out")))
            `(,(string-append "--prefix=" out))))
        ((#:phases phases)
         `(modify-phases ,phases
            (delete 'configure-fixup)
            (add-before 'configure 'setenv
              (lambda _
                (setenv "LIBS" "-lc -lnss_files -lnss_dns -lresolv")
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

(define gcc-mesboot1
  (package-with-bootstrap-guile
   (package
     (inherit gcc-mesboot0)
     (name "gcc-mesboot1")
     (version "4.7.4")
     (source (origin (inherit (package-source gcc-4.7))
                     (patches (search-patches "gcc-boot-4.7.4.patch"))))
     (inputs `(("gmp-source" ,(package-source gmp-boot))
               ("mpfr-source" ,(package-source mpfr-boot))
               ("mpc-source" ,(package-source mpc-boot))))
     (native-inputs `(("binutils" ,binutils-mesboot)
                      ("gcc" ,gcc-mesboot0)
                      ("libc" ,glibc-mesboot0)

                      ("bash" ,%bootstrap-coreutils&co)
                      ("coreutils" ,%bootstrap-coreutils&co)
                      ("diffutils" ,diffutils-mesboot)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)
                      ("make" ,make-mesboot)))
     (arguments
      (substitute-keyword-arguments (package-arguments gcc-core-mesboot)
        ((#:make-flags make-flags)
         `(let* ((libc (assoc-ref %build-inputs "libc"))
                 (ldflags (string-append
                           "-B" libc "/lib "
                           "-Wl,-dynamic-linker "
                           "-Wl," libc
                           ,(glibc-dynamic-linker "i686-linux"))))
            (list (string-append "LDFLAGS=" ldflags)
                  (string-append "LDFLAGS_FOR_TARGET=" ldflags))))
        ((#:phases phases)
         `(modify-phases ,phases
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
            (delete 'remove-info)
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
                                            gcc "/lib/gcc-lib/i686-unknown-linux-gnu/2.95.3/include"
                                            ":" kernel-headers "/include"
                                            ":" glibc "/include"
                                            ":" (getcwd) "/mpfr/src"))
                  (setenv "LIBRARY_PATH" (string-append glibc "/lib"
                                                        ":" gcc "/lib"))
                  (format (current-error-port) "C_INCLUDE_PATH=~a\n" (getenv "C_INCLUDE_PATH"))
                  (format (current-error-port) "LIBRARY_PATH=~a\n" (getenv "LIBRARY_PATH"))
                  #t)))
            (delete 'install2)))
        ((#:configure-flags configure-flags)
         `(let ((out (assoc-ref %outputs "out"))
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
                  "--enable-languages=c,c++"

                  "--enable-static"
                  ;; libstdc++.so: error: depends on 'libgcc_s.so.1', which cannot be found in RUNPATH ()
                  "--disable-shared"
                  "--enable-threads=single"

                  ;; No pre-compiled libstdc++ headers, to save space.
                  "--disable-libstdcxx-pch"

                  ;; for libcpp ...
                  "--disable-build-with-cxx"))))))))

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
     (native-inputs `(("bash" ,%bootstrap-coreutils&co)
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
                   "gcc"
                   "g++"
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
     (native-inputs `(("binutils" ,binutils-mesboot)
                      ("libc" ,glibc-mesboot0)
                      ("gcc" ,gcc-mesboot1)
                      ("headers" ,mesboot-headers)

                      ("bash" ,%bootstrap-coreutils&co)
                      ("coreutils" ,%bootstrap-coreutils&co)
                      ("diffutils" ,diffutils-mesboot)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)
                      ("make" ,make-mesboot)))

     (arguments
      (substitute-keyword-arguments (package-arguments glibc-mesboot0)
        ((#:configure-flags configure-flags)
         `(let ((out (assoc-ref %outputs "out"))
                (headers (assoc-ref %build-inputs "headers")))
            (list
             (string-append "--prefix=" out)
             "--disable-obsolete-rpc"
             "--host=i686-unknown-linux-gnu"
             (string-append "--with-headers=" headers "/include")
             "--enable-static-nss"
             "--with-pthread"
             "--without-cvs"
             "--without-gd"
             "--enable-add-ons=nptl")))
        ((#:make-flags make-flags)
         `(let ((bash (assoc-ref %build-inputs "bash")))
            (list (string-append "SHELL=" bash "/bin/sh")
                  "install-bootstrap-headers=yes" "install-headers")))
        ((#:phases phases)
         `(modify-phases ,phases
            (replace 'setenv
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (headers (assoc-ref %build-inputs "headers"))
                       (bash (assoc-ref %build-inputs "bash"))
                       (coreutils (assoc-ref %build-inputs "coreutils"))
                       (libc (assoc-ref %build-inputs "libc"))
                       (gcc (assoc-ref %build-inputs "gcc"))
                       (cppflags (string-append
                                  " -I " (getcwd) "/nptl/sysdeps/pthread/bits"
                                  " -D BOOTSTRAP_GLIBC=1"))
                       (cflags (string-append " -L " (getcwd)
                                              " -L " libc "/lib")))
                  (setenv "libc_cv_friendly_stddef" "yes")
                  (setenv "CONFIG_SHELL" (string-append bash "/bin/sh"))
                  (setenv "SHELL" (getenv "CONFIG_SHELL"))
                  (format (current-error-port) "CONFIG_SHELL=~s\n" (getenv "CONFIG_SHELL"))

                  (setenv "CPP" (string-append gcc "/bin/gcc -E " cppflags))
                  (setenv "CC" (string-append gcc "/bin/gcc " cppflags cflags))

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
                (invoke "make" (string-append (getcwd) "/sysd-sorted" )
                        (string-append "SHELL=" (getenv "CONFIG_SHELL")))
                (substitute* "sysd-sorted"
                  ((" sunrpc") " ")
                  ((" nis") " "))
                ;; 'rpcgen' needs native libc headers to be built.
                (substitute* "../Makefile"
                  (("^SHELL := /bin/sh") (string-append "SHELL := " (getenv "CONFIG_SHELL"))))
                (substitute* "../Makeconfig"
                  (("^SHELL := /bin/sh") (string-append "SHELL := " (getenv "CONFIG_SHELL"))))
                (substitute* "../elf/Makefile"
                  (("^SHELL := /bin/sh") (string-append "SHELL := " (getenv "CONFIG_SHELL")))))))))))))

(define glibc-mesboot
  (package-with-bootstrap-guile
   (package
     (inherit glibc-headers-mesboot)
     (name "glibc-mesboot")
     (native-inputs `(("binutils" ,binutils-mesboot)
                      ("libc" ,glibc-mesboot0)
                      ("headers" ,glibc-headers-mesboot)
                      ("gcc" ,gcc-mesboot1)

                      ("bash" ,%bootstrap-coreutils&co)
                      ("coreutils" ,%bootstrap-coreutils&co)
                      ("diffutils" ,diffutils-mesboot)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)
                      ("make" ,make-mesboot)))

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

(define gcc-mesboot
  (package-with-bootstrap-guile
   (package
     (inherit gcc-mesboot1)
     (name "gcc-mesboot")
     (version "4.9.4")
     (source (package-source gcc-4.9))
     (native-inputs `(("binutils" ,binutils-mesboot)
                      ("gcc-wrapper" ,gcc-mesboot1-wrapper)
                      ("gcc" ,gcc-mesboot1)
                      ("libc" ,glibc-mesboot)

                      ("bash" ,%bootstrap-coreutils&co)
                      ("coreutils" ,%bootstrap-coreutils&co)
                      ("diffutils" ,diffutils-mesboot)
                      ("kernel-headers" ,%bootstrap-linux-libre-headers)
                      ("make" ,make-mesboot)))
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
                                                gcc "/lib/gcc-lib/i686-unknown-linux-gnu/4.7.4/include"
                                                ":" kernel-headers "/include"
                                                ":" glibc "/include"
                                                ":" (getcwd) "/mpfr/src"))
                      (setenv "CPLUS_INCLUDE_PATH" (string-append
                                                    gcc "/lib/gcc-lib/i686-unknown-linux-gnu/4.7.4/include"
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
     (native-inputs `(("bash" ,%bootstrap-coreutils&co)
                      ("libc" ,glibc-mesboot)
                      ("gcc" ,gcc-mesboot))))))

(define m4-mesboot
  (package-with-bootstrap-guile
   (package
     (inherit m4)
     (name "m4-mesboot")
     (version "1.4")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/m4/m4-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "1f9bxj176kf3pvs350w2dfs8jgwhminywri5pyn01b11yc4yhsjw"))))
     (supported-systems '("i686-linux" "x86_64-linux"))
     (native-inputs `(("mes" ,mes-boot)
                      ("tcc" ,tcc-boot)))
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (replace 'configure
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (setenv "CONFIG_SHELL" (string-append
                                        (assoc-ref %build-inputs "bash")
                                        "/bin/sh"))
                (setenv "CC" "tcc -static")
                (setenv "CPP" "tcc -E")
                (invoke "./configure" (string-append "--prefix=" out)))))))))))

(define (%bootstrap-inputs+toolchain)
  ;; The traditional bootstrap-inputs.  For the i686-linux Reduced Binary Seed
  ;; the actual reduced set with bootstrapped toolchain.
  (append (match (%current-system)
            ((or "i686-linux" "x86_64-linux")
             `(("libc" ,glibc-mesboot)
               ("binutils" ,binutils-mesboot)
               ("gcc-wrapper" ,gcc-mesboot-wrapper)
               ("gcc" ,gcc-mesboot)))
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
