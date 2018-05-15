;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages bioconductor)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages bioinformatics))

(define-public r-bsgenome-dmelanogaster-ucsc-dm6
  (package
    (name "r-bsgenome-dmelanogaster-ucsc-dm6")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              ;; We cannot use bioconductor-uri here because this tarball is
              ;; located under "data/annotation/" instead of "bioc/".
              (uri (string-append "https://www.bioconductor.org/packages/"
                                  "release/data/annotation/src/contrib/"
                                  "BSgenome.Dmelanogaster.UCSC.dm6_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1bhj0rdgf7lspw4xby9y9mf7v7jxxz8001bc8vw8kf04rjsx6060"))))
    (properties
     `((upstream-name . "BSgenome.Dmelanogaster.UCSC.dm6")))
    (build-system r-build-system)
    ;; As this package provides little more than a very large data file it
    ;; doesn't make sense to build substitutes.
    (arguments `(#:substitutable? #f))
    (propagated-inputs
     `(("r-bsgenome" ,r-bsgenome)))
    (home-page
     "https://www.bioconductor.org/packages/BSgenome.Dmelanogaster.UCSC.dm6/")
    (synopsis "Full genome sequences for Fly")
    (description
     "This package provides full genome sequences for Drosophila
melanogaster (Fly) as provided by UCSC (dm6) and stored in Biostrings
objects.")
    (license license:artistic2.0)))

(define-public r-hpar
  (package
    (name "r-hpar")
    (version "1.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "hpar" version))
       (sha256
        (base32
         "0s5v79mgxdx862v1jrdf5pdap81nz5vjx25ni8s3sl97ldckf6j8"))))
    (build-system r-build-system)
    (home-page "https://bioconductor.org/packages/hpar/")
    (synopsis "Human Protein Atlas in R")
    (description "This package provides a simple interface to and data from
the Human Protein Atlas project.")
    (license license:artistic2.0)))
