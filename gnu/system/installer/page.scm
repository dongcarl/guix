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

(define-module (gnu system installer page)
  #:export (make-page)
  #:export (page-surface)
  #:export (page-refresh)
  #:export (page-initialised?)
  #:export (page-set-initialised!)
  #:export (page-enter)
  #:export (page-leave)
  #:export (page-set-wwin!)
  #:export (page-wwin)
  #:export (page-cursor-visibility)
  #:export (page-title)
  #:export (page-datum)
  #:export (page-set-datum!)
  #:export (page-key-handler)
  #:export (page-mouse-handler)

  #:use-module (gnu system installer utils)
  #:use-module (gnu system installer levelled-stack)
  #:use-module (srfi srfi-9))

(define-record-type <page>
  (make-page' surface title inited refresh cursor-visibility key-handler mouse-handler data)
  page?
  (title page-title)
  (surface page-surface)
  (inited  page-initialised? page-set-initialised!)
  (refresh page-refresh)
  (cursor-visibility page-cursor-visibility)
  (key-handler page-key-handler)
  (mouse-handler page-mouse-handler)
  (wwin page-wwin page-set-wwin!)
  (data page-data page-set-data!))

(define (make-page surface title refresh cursor-visibility key-handler mouse-handler)
  (make-page' surface title #f refresh cursor-visibility key-handler mouse-handler '()))

(define (page-set-datum! page key value)
  (page-set-data! page (acons key value (page-data page))))

(define (page-datum page key)
  (assq-ref (page-data page) key))

(define (page-leave)
  (pop-cursor)
  (page-pop))

(define (page-enter p)
  (page-push p)
  ((page-refresh p) p))

