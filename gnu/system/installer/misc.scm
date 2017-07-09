;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 John Darrington <jmd@gnu.org>
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

(define-module (gnu system installer misc)
  #:use-module (ncurses curses)
  #:use-module (gurses colors)
  #:use-module (gnu system shadow)

  #:export (time-zone)
  #:export (host-name)
  #:export (config-file)
  #:export (key-map)
  #:export (system-role)
  #:export (users)
  #:export (installer-texinfo-markup)
  #:export (install-attempts)
  #:export (mount-points))

(define mount-points '())

(define time-zone "")

(define host-name "")
(define key-map #f)

(define config-file #f)

(define system-role #f)

(define installer-texinfo-markup
  `((bold         . ,bold)
    (samp         . ,normal)
    (code         . ,normal)
    (math         . ,normal)
    (kbd          . ,normal)
    (key          . ,inverse)
    (var          . ,normal)
    (env          . ,normal)
    (file         . ,normal)
    (command      . ,normal)
    (option       . ,normal)
    (dfn          . ,standout)
    (cite         . ,normal)
    (acro         . ,normal)
    (email        . ,normal)
    (emph         . ,dim)
    (strong       . ,(lambda (x) (color (color-index-by-symbol 'strong) x)))
    (sample       . ,normal)
    (sc           . ,normal)
    (titlefont    . ,normal)
    (asis         . ,normal)
    (b            . ,bold)
    (i            . ,normal)
    (r            . ,normal)
    (sansserif    . ,normal)
    (slanted      . ,normal)
    (t            . ,normal)))


(define users '())

(define install-attempts 0)
