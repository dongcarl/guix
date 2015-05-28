;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages astronomy)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages image)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages maths)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk))

(define-public cfitsio
  (package
    (name "cfitsio")
    (version "3.420")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://heasarc.gsfc.nasa.gov/FTP/software/fitsio/c/"
             name (string-replace-substring version "." "") ".tar.gz"))
       (sha256
        (base32 "1f0nmki45h9kw7vxpxiav9cb6vs3qqi6zrp2lpci5yhqc5isl43c"))))
    (build-system gnu-build-system)
    ;; XXX Building with curl currently breaks wcslib.  It doesn't use
    ;; pkg-config and hence won't link with -lcurl.
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda _
             (substitute* "Makefile.in" (("/bin/") ""))
             #t)))))
    (home-page "https://heasarc.gsfc.nasa.gov/fitsio/fitsio.html")
    (synopsis "Library for reading and writing FITS files")
    (description "CFITSIO provides simple high-level routines for reading and
writing @dfn{FITS} (Flexible Image Transport System) files that insulate the
programmer from the internal complexities of the FITS format. CFITSIO also
provides many advanced features for manipulating and filtering the information
in FITS files.")
    (license (license:non-copyleft "file://License.txt"
                          "See License.txt in the distribution."))))

(define-public wcslib
  (package
    (name "wcslib")
    (version "5.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "ftp://ftp.atnf.csiro.au/pub/software/wcslib/" name "-" version
             ".tar.bz2"))
       (sha256
        (base32 "16jh568k99c9p0y3qzcgps2rii933x9wlay7q1xm0lr59zqzp4xn"))))
    (inputs
     `(("cfitsio" ,cfitsio)))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'patch-/bin/sh
                    (lambda _
                      (substitute* "makedefs.in"
                        (("/bin/sh") "sh"))
                      #t)))))
    (home-page "https://www.atnf.csiro.au/people/mcalabre/WCS")
    (synopsis "Library which implements the FITS WCS standard")
    (description "The FITS \"World Coordinate System\" (@dfn{WCS}) standard
defines keywords and usage that provide for the description of astronomical
coordinate systems in a @dfn{FITS} (Flexible Image Transport System) image
header.")
    (license license:lgpl3+)))

(define-public gnuastro
  (package
    (name "gnuastro")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/gnuastro/gnuastro-"
                           version ".tar.gz"))
       (sha256
        (base32
         "10lxzxyrf30hj3bqdgprvaj9phzdi816khjmr0vmjf8pmsr8bqqr"))))
    (inputs
     `(("cfitsio" ,cfitsio)
       ("gsl" ,gsl)
       ("libjpeg" ,libjpeg)
       ("wcslib" ,wcslib)))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/gnuastro/")
    (synopsis "Astronomy utilities")
    (description "The GNU Astronomy Utilities (Gnuastro) is a suite of
programs for the manipulation and analysis of astronomical data.")
    (license license:gpl3+)))

(define-public stellarium
  (package
    (name "stellarium")
    (version "0.17.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/Stellarium/" name
                                 "/releases/download/v" version
                                 "/" name "-" version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "0d6b3fs5aify7i1lwgkcickppnj73cbh24g8qschnfs3ypdf48fc"))))
    (build-system cmake-build-system)
    (inputs
     `(("qtbase" ,qtbase)
       ("qtlocation" ,qtlocation)
       ("qtmultimedia" ,qtmultimedia)
       ("qtscript" ,qtscript)
       ("qtserialport" ,qtserialport)
       ("zlib" ,zlib)))
    (native-inputs
     `(("gettext" ,gettext-minimal) ; xgettext is used at compile time
       ("perl" ,perl) ; For pod2man
       ("qtbase" ,qtbase) ; Qt MOC is needed at compile time
       ("qttools" ,qttools)))
    (arguments
     `(#:test-target "test"
       #:configure-flags (list "-DENABLE_TESTING=1"
                               (string-append
                                "-DCMAKE_CXX_FLAGS=-isystem "
                                (assoc-ref %build-inputs "qtserialport")
                                "/include/qt5"))
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'set-offscreen-display
                    (lambda _
                      ;; make Qt render "offscreen", required for tests
                      (setenv "QT_QPA_PLATFORM" "offscreen")
                      (setenv "HOME" "/tmp")
                      #t)))))
    (home-page "http://www.stellarium.org/")
    (synopsis "3D sky viewer")
    (description "Stellarium is a planetarium.  It shows a realistic sky in
3D, just like what you see with the naked eye, binoculars, or a telescope.  It
can be used to control telescopes over a serial port for tracking celestial
objects.")
    (license license:gpl2+)))

(define-public celestia
  (package
    (name "celestia")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/" name
                                  "/Celestia-source/" version "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32 "1i1lvhbgllsh2z8i6jj4mvrjak4a7r69psvk7syw03s4p7670mfk"))
              (patches (map search-patch '("celestia-libpng-1.5.patch"
                                           "celestia-libpng-1.6.patch"
                                           "celestia-use-stdint.h.patch"
                                           "celestia-use-0-not-NULL.patch"
                                           "celestia-add-missing-includes.patch"
                                           "celestia-gcc-4.7.patch"
                                           "celestia-mips.patch")))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("lua" ,lua-5.1)
       ("gtk+" ,gtk+-2)
       ("gtkglext" ,gtkglext)
       ("mesa" ,mesa)
       ("glu" ,glu)
       ("libtheora" ,libtheora)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)))
    (arguments
     `(#:configure-flags
       (let ((lua (assoc-ref %build-inputs "lua")))
         (list "--with-gtk"
               (string-append "LUA_CFLAGS=-I" lua "/include")
               (string-append "LUA_LIBS=-L" lua "/libs -llua")))
       #:out-of-source? #f))
    (home-page "http://www.shatters.net/celestia/")
    (synopsis "Real-time visual space simulation")
    (description
     "Celestia is a free 3D astronomy program.  Based on the Hipparcos
Catalogue, it allows users to display objects ranging in scale from artificial
satellites to entire galaxies in three dimensions using OpenGL.  Unlike most
planetarium software, the user is free to travel about the Universe.")
    ;; XXX TODO: Investigate licenses of included images and models.
    (license license:gpl2+)))
