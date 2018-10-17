;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
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

(define-module (guix build gx)
  #:use-module (guix build utils)
  #:use-module (ice-9 popen)
  #:export (gx-fetch))

;;; Commentary:
;;;
;;; This is the build-side support code of (guix gx-download).  It allows a
;;; gx hash to be fetched.
;;;
;;; Code:

(define* (gx-fetch hash directory
                   #:key (gx-command "gx"))
  "Fetch IPFS HASH into DIRECTORY.  HASH must be a valid IPFS hash.
Return #t on success, #f otherwise."

  (mkdir-p directory)

  (with-directory-excursion directory
    ;; TODO: Silence verbose output.

    ;; Initialization is interactive, but we can shut it up by piping it to
    ;; nothing.
    (let ((port (open-pipe* OPEN_WRITE gx-command "init")))
      (display "\n" port)
      (if (not (eqv? 0 (status:exit-val (close-pipe port))))
          (error "Cannot initialize gx package")))

    ;; Fetch to the "vendor" directory.
    (let ((port (open-pipe* OPEN_WRITE gx-command "import" "--local" hash)))
      (display "N\n" port)
      (if (not (eqv? 0 (status:exit-val (close-pipe port))))
          (error "Cannot import gx package")))

    (delete-file "package.json")
    (mkdir-p   "gx/ipfs")
    (rename-file (string-append  "vendor/gx/ipfs/" hash) (string-append "gx/ipfs/" hash))
    (delete-file-recursively "vendor")
    #t))

;;; gx.scm ends here
