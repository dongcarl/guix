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

(define-module (gnu system installer utils)
  #:export (justify
	    justify*
	    addstr*
	    slurp
	    quit-key?

	    push-cursor
	    pop-cursor

	    make-window-port
	    new-nav-form
	    standard-menu-keystrokes

	    make-boxed-window
            inner
            outer
	    
	    open-input-pipe-with-fallback

	    find-mount-device

	    window-pipe
            pipe-cmd

	    N_
	    
	    select-key?))

(use-modules (ice-9 popen)
	     (ice-9 rdelim)
             (ice-9 match)
	     (ncurses menu)
	     (gnu system installer misc)
	     (ncurses form)
             (ncurses curses))

(define-syntax N_
  (syntax-rules ()
    ((N_ str)
     str)))

(define (make-window-port win)
  "Return a port which writes to the curses window WIN"
  (make-soft-port
   (vector
    (lambda (c) (addch win c))
    (lambda (s) (addstr win s))
    (lambda () (refresh win))
    #f
    #f)
   "w"))

(define* (window-pipe win cmd #:rest args)
  "Run CMD ARGS ... sending stdout and stderr to WIN.  Returns the exit status of CMD."
  (clear win)
  (let* ((windowp (make-window-port win))
         (result (apply pipe-cmd windowp cmd args)))
    (close-port windowp)
    result))

(define* (pipe-cmd ipipe cmd #:rest args)
  "Run CMD ARGS ... sending stdout and stderr to IPIPE.  Returns the exit status of CMD."
  (let* ((pipep (pipe))
	 (pid (primitive-fork)))
    (if (zero? pid)
	(begin
          (close-port (car pipep))
	  (redirect-port (cdr pipep) (current-output-port))
	  (redirect-port (cdr pipep) (current-error-port))
	  (apply execlp cmd args))
	(begin
	  (close-port (cdr pipep))
	  (let loop ((c (read-char (car pipep))))
	    (unless (eof-object? c)
              (display c ipipe)
              (force-output ipipe)
              (loop (read-char (car pipep)))))
          (close-port (car pipep))))
    (cdr (waitpid pid))))

(define (justify text width)
  "A naive function to split a string into lines no more than width characters long."
  (define (justify' l n acc)
    (if (null? l)
	acc
	(let* ((word (car l))
	       (len (string-length word)))

	  (define (linefull? n w)
	    (> (+ len n) w))

	  (justify'
	   (if (linefull? n width)
	       l
	       (cdr l))
	   (if (linefull? n width)
	       0
	       (+ n (1+ len)))

	   (if (linefull? n width)
	       (string-append acc 
			      (make-string (- width len) #\space))
	       (string-append acc word " "))))))

  (justify' (string-split text char-set:blank) 0  ""))

(define (justify* text width)
  "A naive function to split a string into lines no more than width characters long.
This version assumes some external entity puts in the carriage returns."
  (define (justify' l n acc)
    (if (null? l)
	acc
	(let* ((word (car l))
	       (o (remainder n width))
	       (len (string-length word))
	       (appendage (cond ((zero? o)
				 (string-append word))

				((> (- width o) len)
				 (string-append " " word))
				
				(else
				 (string-append (make-string (- width o) #\space) word)))))

	  (justify'
	   (cdr l)

	   (+ n (string-length appendage))

	   (string-append acc appendage)))))
  
  (justify' (string-split text char-set:blank) 0  ""))


(define (addstr* win str)
  "Call the curses addstr procedure passing STR to justify to the width of WIN"
  (addstr win (justify* str (getmaxx win))))

(define (open-input-pipe-with-fallback cmd)
  "Kludge for testing"
  (let* ((subst (string-append (dirname (current-filename)) "/pipe-subst/"
	       (string-map (lambda (c) (case c
					 ((#\space) #\%)
					 ((#\/) #\,)
					 (else c)))
			   cmd))))
    (if (and (not (eqv? 0 (geteuid)))
	     (file-exists? subst))
	(open-input-pipe (string-append "cat " subst))
	(open-input-pipe cmd))))

(define (slurp cmd proc)
  (let ((port #f)
	(status #f)
	(result #f))
    (dynamic-wind (lambda () (set! port (open-input-pipe-with-fallback cmd)))
		  (lambda () (set! result (slurp-real port proc)))
		  (lambda () (set! status (close-pipe port))))
    (if (zero? (status:exit-val status))
	result
	#f)))

(define (slurp-real port proc)
  "Execute CMD in a shell and return a list of strings from its standard output,
one per line.  If PROC is not #f then it must be a procedure taking a string
which will process each string before returning it."
  (let lp ((line-list '()))
    (let  ((l (read-line port)))
      (if (eof-object? l)
	  (reverse line-list)
	  (lp (cons (if proc (proc l) l) line-list))))))



(define (quit-key? c)
  (or
   (eqv? c #\q)
   (eqv? c #\Q)
   (eqv? c #\esc)))

(define (select-key? c)
  (or
   (eqv? c #\nl)
   (eqv? c #\cr)
   (eqv? c KEY_ENTER)))




(define cursor-stack '())

(define (push-cursor c)
  (curs-set c)
  (set! cursor-stack (cons c cursor-stack)))

(define (pop-cursor)
  (set! cursor-stack (cdr cursor-stack))
  (curs-set (car cursor-stack)))




(define (standard-menu-keystrokes ch menu)
  (let ((win (menu-win menu)))
    (cond
	((eqv? ch KEY_DOWN)
	 (menu-driver menu REQ_DOWN_ITEM)
	 )

	((eqv? ch KEY_UP)
	 (menu-driver menu REQ_UP_ITEM)
	 ))
    
    (refresh win)))



(define (new-nav-form button-fields)
  (new-form (let usr ((ef button-fields)
		      (xpos 0)
		      (acc '()))
	      (if (null? ef)
		  (reverse acc)
		  (let* ((ff (cdr (car ef)))
			 (label (car ff))
			 (nf (new-field 1 (string-length label) 1 xpos 0 0)))
		    (list-set! ff 1 nf)
		    (set-field-buffer! nf 0 label)
		    (field-opts-off! nf O_EDIT)
		    (usr (cdr ef)
			 (+ xpos (string-length label) 1)
			 (cons nf acc)))))))





(define (inner boxed-window)
  (match boxed-window
    ((inside . _)
     (if (not (window? inside))
         (error "~s is not a window" inside))
     inside)))

(define (outer boxed-window)
  (match boxed-window
    ((_ . outside)
     (if (not (window? outside))
         (error "~s is not a window" outside))
     outside)))


(define* (make-boxed-window orig height width starty startx #:key (title #f))
  "Create a window with a frame around it, and optionally a TITLE.  Returns a
pair whose car is the inner window and whose cdr is the frame."
  (let* ((win  (if orig
		   (derwin orig height width starty startx #:panel #f)
		   (newwin      height width starty startx #:panel #f)
	       ))
	 (ystart (if title 3 1))
	 (sw (derwin win (- (getmaxy win) ystart 1)
		     (- (getmaxx win) 2)
		     ystart 1 #:panel #f)))
    (clear win)
    (box win (acs-vline) (acs-hline))

    (if title
	(begin
	  (move win 2 1)
	  (hline win (acs-hline) (- (getmaxx win) 2))
	  (color-set! win livery-title)
	  (addstr win title #:y 1
		  #:x (round (/ (- (getmaxx win) (string-length title)) 2)))))

    (refresh sw)
    ;; Return the inner and outer windows
    (cons sw win)))


(define (find-mount-device in mp)
  "Given the list of (device . mount-point) pairs MP which indicates intended
mounts return the device on which the path IN would be mounted."
  (define dir-sep #\/)

  (define (normalise-directory-path p)
    ;; Drop the last character if it is #\/
    ;; !!!even if that is the ONLY character!!!
    (if (positive? (string-length p))
	(let* ((last (1- (string-length p))))
	  (if (eqv? dir-sep (string-ref p last))
	      (string-drop-right p 1)
	      p))
	p))

  (if (not (absolute-file-name? in))
      (error (format #f "Path is not absolute")))

  (let ((target	 (string-split (normalise-directory-path in) dir-sep))
	(paths
	 (map-in-order
	  (lambda (p)
	    (cons (car p)
		  (string-split (normalise-directory-path (cdr p)) dir-sep)))
	  (sort mp (lambda (x y) (string> (cdr x) (cdr y)))))))
    
    (let loop ((pp paths))
      (if (null? pp)
	  #f
	  (let* ((subject (cdar pp))
		 (len (min (length subject)
			   (length target))))
	    (if (and
		 (<= (length subject) (length target))
		 (equal? (list-head target len)
			 (list-head subject len)))
		(caar pp)
		(loop (cdr pp))))))))
