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

(define-module (gurses stexi)

  #:export (render-stexi)
  #:use-module (ncurses curses)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define default-markup-table
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
    (dfn          . ,underline)
    (cite         . ,normal)
    (acro         . ,normal)
    (email        . ,normal)
    (emph         . ,dim)
    (strong       . ,blink)
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


(define* (render-stexi win stexi #:key (y-start 0) (markup-table default-markup-table))
  "Render STEXI to WIN"
  (let loop ((y y-start)
	     (lines (stexi->curses stexi (getmaxx win) markup-table)))
    (when (not (null? lines))
	  (addchstr win
		    (car lines)
		    #:y y #:x 0)
	  (loop (1+ y) (cdr lines)))))

(define (stexi->curses stxi line-length table)
  "Return a list of `complex strings' justified to LINE-LENGTH comprising the text
described by the stexi STXI"
  (define (parse-fragment frag out markup)
    (match frag
	   (() out)
	   (('para . rest)
	    (let ((par (parse-fragment rest '() normal)))
	      (append out
		      (justify (append par (list (normal #\newline))) line-length))))
	   ((first . second)
	    (parse-fragment
	     second
	     (match
	      first
	      ((? string? s)
	       (append out (markup s)))

	      (((? symbol? x) . rest)
	       (append out
		       (parse-fragment
			rest '()
			(assq-ref table x))))) markup))))

  (map-in-order
   (lambda (line)
     (if (null? line)
	 line
	 (pad-complex-string line line-length)))
   (match stxi
	  (('*fragment* . rest)
	   (let loop ((in rest)
		      (acc '()))
	     (if (null? in)
		 acc
		 (loop (cdr in)
		       (parse-fragment (car in) acc normal))))))))

(define (offset-to-end-of-word ccs)
  "Return the number of xchars until the end of the current word."

  (define (offset-to-end-of-word' cs dist)
    (match
     cs
     ('() dist)
     (((? xchar? first) . rest)
      (if (char-set-contains? char-set:blank (car (xchar-chars first)))
          dist
          (offset-to-end-of-word' rest (1+ dist))))))

  (offset-to-end-of-word' ccs 0))

(define (remove-leading-whitespace cs)
  (if (char-set-contains? char-set:blank (car (xchar-chars (car cs))))
      (cdr cs)
      cs))

(define (line-split cs line-length)
  "Return a pair whose car is the first LINE-LENGTH elements of cs and whose
cdr is the rest"
  (let loop ((in cs)
	     (count 0)
	     (line0 '())
	     (rest '()))
    (if (null? in)
	(let* ((trimmed-line (remove-leading-whitespace line0))
	       (len (length trimmed-line)))
	  (cons (reverse trimmed-line)
		(reverse rest)))

	(if (< (+ (offset-to-end-of-word in) count) line-length)
	    (loop (cdr in) (1+ count) (cons  (car in) line0) rest)
	    (loop (cdr in) (1+ count) line0 (cons (car in) rest))))))

(define (paragraph-format cs line-length)
  (let loop ((pr (line-split cs line-length))
	     (acc '()))
    (if (null? (cdr pr))
	(cons (car pr) acc)
	(loop (line-split (cdr pr) line-length) (cons (car pr) acc)))))

(define (justify text line-length)
  (reverse (paragraph-format text line-length )))


(define (pad-complex-string str len)
  "Return a complex string based on STR but with interword padding to make the
string of length LEN"

  (define (count-words str)
    (let loop ((in str)
	       (x 0)
	       (n 0)
	       (prev-white #t))
      (if (null? in)
	  n
	  (let ((white (char-set-contains? char-set:blank
					   (car (xchar-chars (car in))))))
	    (loop (cdr in) (1+ x) (if (and prev-white (not white))
				      (1+ n)
				      n) white)))))

  (let* ((underflow (- len (length str)))
	 (word-count (count-words str))
	 (inter-word-space-count (1- word-count)))

    (if (zero? inter-word-space-count)
        str
        (begin
          (when (negative? underflow)
                (error
                 (format
                  #f
                  "You asked to pad to ~a but the string is already ~a characters long."
                  len (length str))))

          (if (eqv? (car (xchar-chars (last str))) #\newline)
              str ; Don't justify the last line of a paragraph
              (let loop ((in str)
                         (out '())
                         (words 0)
                         (spaces 0)
                         (prev-white #t))
                (if (null? in)
                    (reverse out)
                    (let* ((white (char-set-contains? char-set:blank
                                                      (car (xchar-chars (car in)))))
                           (end-of-word (and white (not prev-white)))
                           (words-processed (if end-of-word (1+ words) words))
                           (spaces-inserted (if end-of-word
                                                (truncate (- (*
                                                              (/ underflow inter-word-space-count)
                                                              words-processed)
                                                             spaces))
                                                0)))
                      (loop (cdr in)
                            ;; FIXME: Use a more intelligent algorithm.
                            ;; (prefer spaces at sentence endings for example)
                            (append
                             (make-list spaces-inserted (normal #\space))
                             (cons (car in) out))
                            words-processed
                            (+ spaces spaces-inserted)
                            white)))))))))
