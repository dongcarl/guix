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

(define-module (gurses buttons)

  #:export (make-buttons)
  #:export (buttons-post)
  #:export (buttons-select-next)
  #:export (buttons-select-prev)
  #:export (buttons-unselect-all)
  #:export (buttons-select)
  #:export (buttons-selected)
  #:export (buttons-selected-symbol)
  #:export (buttons-select-by-symbol)
  #:export (buttons-fetch-by-key)
  #:export (buttons-n-buttons)
  #:export (buttons-key-matches-symbol?)
  #:export (buttons-mouse-handler)

  #:use-module (ncurses curses)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9))

(define-record-type <buttons>
  (make-buttons' items bwindows selected active-color)
  buttons?
  (items         buttons-items  buttons-set-items!) ;; FIXME this need not be here
  (bwindows      buttons-bwindows buttons-set-bwindows!)
  (selected      buttons-selected buttons-set-selected!)
  (array         buttons-array  buttons-set-array!)
  (active-color  buttons-active-color))

(define (make-buttons items color)
  (make-buttons' items '()  -1 color))

(define (buttons-n-buttons buttons)
  (array-length (buttons-array buttons)))

(define (buttons-get-current-selection buttons)
  "Return the symbol of the button currently selected."
  (let ((sel (buttons-selected buttons)))
  (if (not (array-in-bounds? (buttons-array buttons) sel))
      #f
      (list-ref (array-ref (buttons-array buttons) sel) 2))))

(define (draw-button b color)
    (color-set! b color)
    (box b 0 0)
    (refresh b))

(define (buttons-unselect-all buttons)
  (let* ((arry (buttons-array buttons))
	 (current (buttons-selected buttons))
	 (old (if (array-in-bounds? arry current)
		  (cadr (array-ref arry current)) #f)))
  (if old
      (draw-button old 0))
  (buttons-set-selected! buttons -1)))

(define (buttons-fetch-by-key buttons c)
  (let loop ((idx 0)
	     (key #f))
    (if (or key (not (array-in-bounds? (buttons-array buttons) idx)))
	key
        (loop (1+ idx)
              (match (array-ref (buttons-array buttons) idx)
                     ((ch win sym)
                      (if (eq? ch c) sym #f)))))))

(define (buttons-select buttons which)
  (let ((arry (buttons-array buttons))
	(current (buttons-selected buttons)))
    (if (array-in-bounds? arry which)
	(let ((new (cadr (array-ref arry which)))
	      (old (if (array-in-bounds? arry current)
		       (cadr (array-ref arry current)) #f)))
	  (if (not (eqv? old new))
	      (begin
	      (draw-button new (buttons-active-color buttons))
	      (if old
		  (draw-button old 0))))
	  (buttons-set-selected! buttons which)))))

(define (buttons-select-prev buttons)
  (let ((current (buttons-selected buttons)))
    (buttons-select buttons (1- current))))

(define* (buttons-select-next buttons #:key (wrap #f))
  (let ((current (buttons-selected buttons)))
    (if (and wrap
	     (>= current
		 (1- (array-length (buttons-array buttons)))))
	(buttons-select buttons 0)
	(buttons-select buttons (1+ current)))))

(define (buttons-post buttons win)
  (define n (length (buttons-items buttons)))

  (buttons-set-array!
   buttons
   (list->array ;; FIXME: Populate the array directly instead of using temp list
    1
    (let loop ((bl (buttons-items buttons))
	       (i 0)
	       (alist '()))
      (match bl
             (()	  (reverse alist))
             (((key raw-label use-underscore) . _)
              (let* (
                     ;; Convert the raw-label into a "complex rendered string" which
                     ;; has the mnemonic character highlighted
                     (label.mark
                      (let mk-label ((us #f)
                                     (mark #f)
                                     (output '())
                                     (input (string->list raw-label)))
                        (if (null? input)
                            (cons (reverse output) mark)
                            (let ((c (car input)))
                              (mk-label (eq? c #\_)
                                        (if mark mark (if us c #f))
                                        (if (and (eq? c #\_) use-underscore)
                                            output
                                            (cons
                                             (if us
                                                 ;; On some terminals bold isn't visible
                                                 ;; Inverse is much clearer.
                                                 (inverse c)
                                                 (normal c))
                                             output))
                                        (cdr input))))))
                     (label (car label.mark))
                     (mark  (cdr label.mark))
                     (width (+ (length label) 2))
                     (w (derwin win 3 width 0
                                (round (- (* (1+ i) (/ (getmaxx win) (1+ n)))
                                          (/ width 2))) #:panel #f)))
                (buttons-set-bwindows! buttons (cons w (buttons-bwindows buttons)))
                (box w   0 0)
                (addchstr w label #:y 1 #:x 1)
                (loop (cdr bl) (1+ i) (acons mark (list w key) alist)))))))))



(define (buttons-key-matches-symbol? nav ch symbol)
  (if (char? ch)
      (or (eq? (buttons-fetch-by-key nav (char-downcase ch)) symbol)
          (eq? (buttons-fetch-by-key nav (char-upcase ch)) symbol)
	  (and (or (eq? ch #\newline)
		   (eq? ch #\space))
	       (and=> (buttons-get-current-selection nav)
		      (lambda (x) (eq? x symbol)))))
  #f))

(define (buttons-selected-symbol buttons)
  (let* ((arry (buttons-array buttons))
         (current (buttons-selected buttons)))
    (if (= current -1)
        #f
        (match (array-ref arry current)
          ((ch win sym)
           sym)))))

(define (buttons-select-by-symbol buttons sym)
  (let* ((arry (buttons-array buttons))
         (len (array-length arry)))
    (let loop ((i 0))
      (if (< i len)
          (match (array-ref arry i)
           ((ch win xsym)
            (if (eq? xsym sym)
              (buttons-set-selected! buttons i)
              (loop (1+ i)))))))))

(define (buttons-mouse-handler buttons device-id g-x g-y z button-state)
  (if (logtest BUTTON1_CLICKED button-state)
      (let* ((arry (buttons-array buttons))
             (len (array-length arry)))
        (let loop ((i 0))
          (if (< i len)
              (match (array-ref arry i)
               ((ch win sym)
                (match (mouse-trafo win g-y g-x #f)
                 ((y x)
                  (buttons-select buttons i)
                  'activated)
                  (_ (if (< i len)
                         (loop (1+ i))
                         'ignored)))))
              'ignored)))
      'ignored))
