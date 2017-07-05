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

(define-module (gurses form)
  #:export (form-get-value)
  #:export (form-set-value!)
  #:export (make-form)
  #:export (field-cursor-position)
  #:export (field-symbol)
  #:export (form-post)
  #:export (form-items)
  #:export (form-window)
  #:export (form-enter)
  #:export (form-set-enabled!)
  #:export (form-enabled?)
  #:export (form-update-cursor)
  #:export (form-set-current-field)
  #:export (get-current-field)

  #:use-module (ncurses curses)
  #:use-module (ncurses panel)
  #:use-module (gurses menu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9))

(define-record-type <field>
  (make-field symbol label size menu popup value cursor-position)
  field?
  (symbol          field-symbol)
  (label           field-label)
  (size            field-size)     ; The maximum length of values for this field
  (menu            field-menu)     ; A menu of acceptable values for this field
  (popup           field-popup     field-set-popup!)
  (value           field-value     field-set-value!)
  (cursor-position field-cursor-position field-set-cursor-position!))

(define-record-type <form>
  (make-form' current-item enabled callback)
  form?
  (current-item form-current-item form-set-current-item!)
  (enabled      form-enabled? form-set-enabled!)
  (items        form-items form-set-items!)
  (tabpos       form-tabpos form-set-tabpos!) ;; X Position of the entries
  (window       form-window form-set-window!)
  (callback     form-callback))

(define (form-update-cursor form)
  "Updates the cursor for FIELD in FORM"
  (let ((field (array-ref (form-items form) (form-current-item form))))
    (move (form-window form) (form-current-item form)
	  (+ (field-cursor-position field)
	     (form-tabpos form)))))

(define (draw-field-space win field y x)
  "Draws the template for FIELD at Y, X"
  (addchstr win
	    (make-list
             (if (list? (field-size field))
                 (fold (lambda (x prev) (max prev (string-length x))) 0
                       (field-size field))
                 (field-size field))
             (inverse #\space))
	    #:y y
	    #:x x))

(define (redraw-field form field n)
  "Redraw the FIELD in FORM"
  (draw-field-space (form-window form) field n (form-tabpos form))

  (addchstr (form-window form) (inverse (field-value field))
	  #:y n
	  #:x (form-tabpos form)))

(define (form-set-value! form n str)
  (cond
   ((integer? n)
    (let ((f (array-ref (form-items form) n)))
      (field-set-value! f str)
      (redraw-field form f n)))

   ((symbol? n)
    (let loop ((idx 0))
      (if (array-in-bounds? (form-items form) idx)
	  (let ((ff (array-ref (form-items form) idx)))
	    (if (eq? n (field-symbol ff))
		(begin
		  (field-set-value! ff str)
		  (redraw-field form ff idx))
		(loop (1+ idx))))))))
  (form-update-cursor form))



(define (form-get-value form n)
  "Retrieve a value from FORM.  If N is an integer then the value is
that of the Nth field.   If N is a symbol, then it is the field with the
label eq? to N"
  (cond ((integer? n)
	 (field-value (array-ref (form-items form) n)))
	
	((symbol? n)
	 (let loop ((idx 0))
	   (if (array-in-bounds? (form-items form) idx)
	       (let ((ff (array-ref (form-items form) idx)))
		 (if (eq? n (field-symbol ff))
		     (field-value ff)
		     (loop (1+ idx)))))))))

(define* (make-form items #:optional (callback #f))
  (let ((form (make-form' 0 #t callback)))
    (form-set-items! form
		     (list->array
		      1 (map-in-order
			 (lambda (x)
                           (match x
                                  ((symbol label (? list? things))
                                   (let ((width (apply max
                                                       (map (lambda (x)
                                                              (string-length x))
                                                            things)))
                                         (menu (make-menu things)))
                                     (make-field
                                      symbol label width menu
                                      (let ((p (newwin (length things)
                                                        width 0 0 #:panel #f)))
                                        (menu-post menu p)
                                        p)
                                      "" 0)))
                                  ((symbol label (? integer? size))
                                   (make-field symbol label size #f #f "" 0))))
			 items)))
    form))


(define (form-enter form ch)
  (define (redraw-current-field form field)
    (redraw-field form field (form-current-item form)))

  (define (cursor-move form field pos)
    "Move the cursor to POS and redraw FIELD"
    (field-set-cursor-position! field pos)
    (form-update-cursor form))

  (if (form-enabled? form)
      (let* ((f      (array-ref (form-items form) (form-current-item form)))
             (value  (field-value f))
             (len    (string-length value))
             (pos    (field-cursor-position f))
             (left   (substring value 0   (min pos len)))
             (centre (substring value pos (min (1+ pos) len)))
             (right  (substring value (min (1+ pos) len) len))
             (status (cond
                      ((and (char? ch)
                       (not (char-set-contains? char-set:iso-control ch)))

                       (field-set-value! f (string-join (list left right)
                                                        (make-string 1 ch)))

                       (field-set-cursor-position! f (1+ pos))
                       (addch (form-window form) (inverse ch))
                       'handled)

                      ((eq? ch KEY_DC)
                       (field-set-value! f (string-append left right))
                       (redraw-current-field form f)
                       (form-update-cursor form)
                       'handled)

                      ((eq? ch #\ack) ; Ctrl-F
                       (when (< pos len)
                         (field-set-cursor-position! f (1+ pos))
                         (redraw-current-field form f)
                         (form-update-cursor form))
                       'handled)

                      ((eq? ch #\stx) ; Ctrl-B
                       (when (positive? pos)
                         (field-set-cursor-position! f (1- pos))
                         (redraw-current-field form f)
                         (form-update-cursor form))
                       'handled)

                      ((eq? ch KEY_BACKSPACE)
                       (when (positive? pos)
                         (field-set-value! f (string-append
                                              (string-drop-right left 1)
                                              centre right))
                         (field-set-cursor-position! f (1- pos))
                         (redraw-current-field form f)
                         (form-update-cursor form))
                       'handled)

                      ((eq? ch #\vtab)
                       ;; Delete to end of line
                       (field-set-value! f (substring value 0 pos))
                       (redraw-current-field form f)
                       'handled)

                      ((or (eq? ch KEY_DOWN)
                           (eq? ch #\so)
                           (eq? ch #\tab)
                           (eq? ch #\newline))
                       (let ((status (form-next-field form)))
                         (cursor-move form f 0)
                         status))

                      ((or (eq? ch KEY_UP)
                           (eq? ch #\dle))
                       (let ((status (form-previous-field form)))
                         (cursor-move form f 0)
                         status))

                      ((eq? ch KEY_RIGHT)
                       (if (< pos len)
                         (cursor-move form f (1+ pos)))
                       'handled)

                      ((eq? ch KEY_LEFT)
                       (if (positive? pos)
                         (cursor-move form f (1- pos)))
                       'handled)

                      ((eq? ch #\soh) ; Ctrl-A
                       ;; Move to start of field
                       (cursor-move form f 0)
                       'handled)

                      ((eq? ch #\enq) ; Ctrl-E
                       ;; Move to end of field
                       (cursor-move form f len)
                       'handled)
                      (else 'ignored))))
          (when (form-callback form)
                ((form-callback form) form))

          (refresh (form-window form))
          status)
      'ignored))

(define (ensure-panel! win)
  (if (not (panel? win))
      (make-panel! win)))

(define (run-event-loop form menu end-status)
  "Run modal event loop for FORM until END-STATUS returns something other
than #f.  Return that to our caller."
  (let* ((win (form-window form))
         (ch (getch win)))
    (cond
     ((eqv? ch KEY_MOUSE)
      (match (getmouse)
       ((device-id g-x g-y z button-state)
        (if (eq? 'activated (std-menu-mouse-handler menu device-id g-x g-y z button-state))
          (end-status form #\newline)
          (run-event-loop form menu end-status)))
       (_ #f)))
     ((end-status form ch)
      (end-status form ch))
     (else
       (std-menu-key-handler menu ch)
       (menu-redraw menu)
       (menu-refresh menu)
       (update-panels)
       (doupdate)
       (run-event-loop form menu end-status)))))

(define (maybe-run-modal-popup form which)
  "Check whether the field at index WHICH has a popup menu.
If so, show it, run a modal popup menu, then hide it again.
Set the field value to the newly selected value."
  (let* ((new-field  (array-ref (form-items form) which))
         (popup (field-popup new-field))
         (menu (field-menu new-field))
         (win (form-window form)))
    (when popup
          (ensure-panel! popup)
          (show-panel popup)
          (keypad! win #t)
          (menu-refresh menu)
          (let ((ch (run-event-loop form menu
                                    (lambda (form ch)
                                    (if (or (eqv? ch #\newline)
                                            (eqv? ch #\tab))
                                        ch
                                        #f)))))
            (field-set-value! new-field (menu-get-current-item menu))
            (hide-panel popup)
            (redraw-field form new-field (form-current-item form))
            (move win which (form-tabpos form))
            (if (eq? ch #\tab)
              (form-next-field form))))))

(define (form-set-current-field form which)
  (let* ((old-field  (get-current-field form))
         (popup (field-popup old-field)))
    (when popup
          (hide-panel popup)))
  (form-set-current-item! form which)
  (maybe-run-modal-popup form which))

(define (form-next-field form)
  (if (< (form-current-item form) (1- (array-length (form-items form))))
      (begin
	(form-set-current-field form (1+ (form-current-item form)))
	(refresh (form-window form))
	'handled)
      'ignored))

(define (form-previous-field form)
  (if (> (form-current-item form) 0)
      (begin
	(form-set-current-field form (1- (form-current-item form)))
	(refresh (form-window form))
	'handled)
      'ignored))

(define (form-post form win)
  (form-set-window! form win)
  (let ((xpos
	 ;; Print the labels and return the length of the longest
	 (let loop ((fields (form-items form))
		    (pos 0)
		    (maxlen 0))
	   (if (not (array-in-bounds? fields pos))
	       (+ maxlen 2)
	       (let ((f (array-ref fields pos)))
		 ;; Print the label
		 (addstr win (format #f "~a:" (field-label f)) #:y pos #:x 0)
		 (loop fields (1+ pos) (max maxlen
					    (string-length (field-label f)))))))))

    (form-set-tabpos! form xpos)

    (let loop ((fields (form-items form))
               (pos 0))
      (when (array-in-bounds? fields pos)
            (let* ((f (array-ref fields pos))
                  (p (field-popup f)))
              (when p
                    (ensure-panel! win)
                    (mvwin p
                           (+ (getbegy win) pos)
                           (+ (form-tabpos form) (getbegx win))))
              (loop fields (1+ pos)))))

    ;; Print the field entry areas
    (let loop ((fields (form-items form))
	       (pos 0))
      (if (array-in-bounds? fields pos)
	  (let ((f (array-ref fields pos)))
            (draw-field-space win f pos xpos)
	    (loop fields (1+ pos))))))

    (form-update-cursor form))

(define (get-current-field form)
  (array-ref (form-items form) (form-current-item form)))
