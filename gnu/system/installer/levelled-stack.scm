;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 John Darrington <jmd@gnu.org>
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

(define-module (gnu system installer levelled-stack)
  #:export (page-push)
  #:export (page-pop)
  #:export (page-top)
  #:export (page-ppush)
  #:export (page-ppop)
  #:export (stack)
  #:export (page-uniquify))

;; This module provides a naive stack, from which either a single item
;; may  be page-pushed or a "major" item.  When page-popping, either a single
;; item may be page-popped or all the items down to just before the last
;; major item.  The implementation uses #f to delimit major items, so
;; that cannot be used as a regular item.

(define stack '())


(define (uniquify' in)
  "Remove duplicates from the list IN. Keep the items which are closest to the
tail of the list."
  (let loop ((l (reverse in))
             (acc '()))
    (if (null? l)
        acc
        (loop (cdr l)
              (if (and (car l) (member (car l) (cdr l)))
                  acc
                  (cons (car l) acc))))))

(define (page-uniquify)
  (set! stack (uniquify' stack)))


(define (page-push x)
  (set! stack (cons x stack)))

(define (page-ppush x)
  (set! stack (cons #f stack))
  (page-push x))


(define (page-pop)
  (set! stack (cdr stack))
  (when (and (not (null? stack))
	     (not (car stack)))
	;; If the top item is #f then page-pop again
	(page-pop)))

(define (page-top)
  (if (car stack)
      (car stack)
      (car (cdr stack))))

(define (page-ppop)
  (set! stack (cdr stack))
  (when (not (null? stack))
	(let ((head (car stack)))
	  (if head
	      (page-ppop)
	      (page-pop)))))
