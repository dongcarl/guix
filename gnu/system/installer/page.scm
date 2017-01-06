;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
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

(define-module (gnu system installer page)
  #:export (make-page)
  #:export (page-stack)
  #:export (page-surface)
  #:export (page-refresh)
  #:export (page-initialised?)
  #:export (page-set-initialised!)
  #:export (page-leave)
  #:export (page-set-wwin!)
  #:export (page-wwin)
  #:export (page-title)
  #:export (page-datum)
  #:export (page-set-datum!)
  #:export (page-key-handler)

  #:use-module (srfi srfi-9))

(define page-stack '())

(define-record-type <page>
  (make-page' surface title inited refresh key-handler data)
  page?
  (title page-title)
  (surface page-surface)
  (inited  page-initialised? page-set-initialised!)
  (refresh page-refresh)
  (key-handler page-key-handler)
  (wwin page-wwin page-set-wwin!)
  (data page-data page-set-data!))

(define (make-page surface title refresh key-handler)
  (make-page' surface title #f refresh key-handler '()))

(define (page-set-datum! page key value)
  (page-set-data! page (acons key value (page-data page))))

(define (page-datum page key)
  (assq-ref (page-data page) key))

(define* (page-leave #:optional (return-point #f))
  (set! page-stack
    (or return-point (cdr page-stack))))
