;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017,2018 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages mes)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages man)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public nyacc
  (package
    (name "nyacc")
    (version "0.86.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/"
                                  name "-" version ".tar.gz"))
              (patches (search-patches "nyacc-binary-literals.patch"))
              (sha256
               (base32
                "0lkd9lyspvhxlfs0496gsllwinh62jk9wij6gpadvx9gwz6yavd9"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("guile" ,guile-2.2)))
    (synopsis "LALR(1) Parser Generator in Guile")
    (description
     "NYACC is an LALR(1) parser generator implemented in Guile.
The syntax and nomenclature should be considered not stable.  It comes with
extensive examples, including parsers for the Javascript and C99 languages.")
    (home-page "https://savannah.nongnu.org/projects/nyacc")
    (license (list gpl3+ lgpl3+))))

(define-public mes
  (let ((triplet "i686-unknown-linux-gnu"))
    (package
      (name "mes")
      (version "0.20")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "http://lilypond.org/janneke/mes/"
                      "mes-0.19-46-g4b921d1b.tar"))
                (sha256
                 (base32
                  "1ar217kzl59cx9bk0jw9xw0ql3jsrmps3f0qqbqidr9k0vaa7jmp"))))
      (build-system gnu-build-system)
      (supported-systems '("i686-linux" "x86_64-linux"))
      (propagated-inputs
       `(("mescc-tools" ,mescc-tools)
         ("nyacc" ,nyacc)))
      (native-inputs
       `(("guile" ,guile-2.2)
         ,@(if (not (string-prefix? "i686-linux" (or (%current-target-system)
                                                     (%current-system))))
               ;; Use cross-compiler rather than #:system "i686-linux" to get
               ;; MesCC 64 bit .go files installed ready for use with Guile.
               `(("i686-linux-binutils" ,(cross-binutils triplet))
                 ("i686-linux-gcc" ,(cross-gcc triplet)))
               '())
         ("graphviz" ,graphviz)
         ("help2man" ,help2man)
         ("perl" ,perl)                 ; build-aux/gitlog-to-changelog
         ("texinfo" ,texinfo)))
      (arguments
       `(#:strip-binaries? #f)) ; binutil's strip b0rkes MesCC/M1/hex2 binaries
      (synopsis "Scheme interpreter and C compiler for full source bootstrapping")
      (description
       "GNU Mes--Maxwell Equations of Software--brings the Reduced Binary Seed
bootstrap to Guix and aims to help create full source bootstrapping for
GNU/Linux distributions.  It consists of a mutual self-hosting Scheme
interpreter in C and a Nyacc-based C compiler in Scheme and is compatible with
Guile.")
      (home-page "https://gnu.org/software/mes")
      (license gpl3+))))

(define-public mescc-tools
  (let ((commit "bb062b0da7bf2724ca40f9002b121579898d4ef7")
        (revision "0")
        (version "0.5.2"))
    (package
      (name "mescc-tools")
      (version (string-append version "-" revision "." (string-take commit 7)))
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://git.savannah.nongnu.org/cgit/mescc-tools.git/snapshot/"
                      name "-" commit
                      ".tar.gz"))
                (sha256
                 (base32
                  "1h6j57wyf91i42b26f8msbv6451cw3nm4nmpl1fckp9c7vi8mwkh"))))
      (build-system gnu-build-system)
      (supported-systems '("i686-linux" "x86_64-linux"))
      (arguments
       `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
         #:test-target "test"
         #:phases (modify-phases %standard-phases
                    (delete 'configure))))
      (synopsis "Tools for the full source bootstrapping process")
      (description
       "Mescc-tools is a collection of tools for use in a full source
bootstrapping process.  It consists of the M1 macro assembler, the hex2
linker, the blood-elf symbol table generator, the kaem shell, exec_enable and
get_machine.")
    (home-page "https://savannah.nongnu.org/projects/mescc-tools")
    (license gpl3+))))
