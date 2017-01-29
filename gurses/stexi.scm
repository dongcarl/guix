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



(define (xchar->char ch)
  (car (xchar-chars ch)))

(define (xchar-blank? ch)
  "Return #f if ch is not a blank character"
  (char-set-contains? char-set:blank (xchar->char ch)))

(define (offset-to-end-of-word ccs)
  "Return the number of xchars until the end of the current word."

  (define (offset-to-end-of-word' cs dist)
    (match
     cs
     ('() dist)
     (((? xchar? first) . rest)
      (if (xchar-blank? first)
          dist
          (offset-to-end-of-word' rest (1+ dist))))))

  (offset-to-end-of-word' ccs 0))

(define (remove-leading-whitespace cs)
  (if (xchar-blank? (car cs))
      (cdr cs)
      cs))

(define (line-split cs line-length)
  "Return a pair whose car is the first LINE-LENGTH elements of cs and whose
cdr is the remainder"
  (let loop ((in cs)
	     (count 0)
	     (line0 '())
	     (remainder '()))
    (match
     in
     (() (let ((trimmed-line (remove-leading-whitespace line0)))
           (cons (reverse trimmed-line) (reverse remainder))))

     ((first . rest)
      (if (< (+ (offset-to-end-of-word in) count) line-length)
          (loop rest (1+ count) (cons  first line0) remainder)
          (loop rest (1+ count) line0 (cons first remainder)))))))


(define-public (insert-space line index)
  (call-with-values  (lambda () (split-at line index))
    (lambda (x y) (append x (normal " ") y))))

(define (paragraph-format cs line-length)
  (let loop ((pr (line-split cs line-length))
	     (acc '()))
    (match pr
           ((only) (cons only acc))
           ((first . rest)
            (loop (line-split rest line-length) (cons first acc))))))

(define (justify text line-length)
  (reverse (paragraph-format text line-length )))


(define-public (word-endings str)
  "Return a list of all the indicies of all the word endings in STR.  The list is sorted in order of prefered padding location."
  (let loop ((in str)
             (x 0)
             (n '())
             (prev-white #t)
             (prev-char #f)
             )
    (match
     in
     (() (map-in-order
          (lambda (x) (car x))
          (sort n (lambda (x y)
                    (if (eqv? (cadr x) (cadr y))
                        (> (caddr x) (caddr y))
                        (> (cadr x) (cadr y)))))))
     ((first . rest)
      (let ((white (xchar-blank? first)))
        (loop rest (1+ x) (if (and (not prev-white)  white)
                              (cons (list x
                                          (case (xchar->char prev-char)
                                            ((#\.) 3)
                                            ((#\,) 2)
                                            (else 1))
                                          (random 1.0)) n)
                              n) white first))))))

(define (pad-complex-string str len)
  "Return a complex string based on STR but with interword padding to make the
string of length LEN"
  (let ((how-many (- len (length str)))
	(endings (word-endings str)))
    (if (null? endings)
        str
        (let ((rem  (remainder how-many (length endings)))
              (quot (quotient  how-many (length endings))))
          (if (eqv? (xchar->char (last str)) #\newline)
              str ; Don't justify the last line of a paragraph
              (begin
                ;; FIXME: If quot is non zero, then we must pad EVERY space with
                ;; quot additional spaces.
                (when (positive? quot)
                      (error "Quotient is positive"))

                (let loop ((in str)
                           (ips
                            (sort (take endings rem) (lambda (x y) (> x y)))))
                  (if (null? ips)
                      in
                      (loop
                       (insert-space in (car ips))
                       (cdr ips))))))))))
