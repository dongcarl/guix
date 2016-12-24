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

(define-module (gnu system installer network)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer ping)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (gnu system installer wireless)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (gurses menu)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (guix store)

  #:export (make-network-page))

(define (make-network-page parent  title)
  (make-page (page-surface parent)
	     title
	     network-page-refresh
	     network-page-key-handler))


(define (interfaces)
  "Return a alist of network interfaces. Keys include 'name, 'class and 'state"
  (slurp "ip -o link"
         (lambda (s)
           (match (string-split s #\space)
             ((_ interface-name _ _ _ _ _ _
                 state _ _ _ _ _ _ _ _ _ class . _)
              (let ((clean-name (string-trim-right interface-name #\:)))
              `((name .  ,clean-name)
                (state . ,state)
                (class . ,(cond
                           ((equal? class "link/loopback") 'loopback)
                           ((equal? class "link/ether")
                            (if (zero? (system* "iw" "dev" clean-name "info"))
                                 'wireless
                                 'ethernet))
                           (else 'other))))))))))

(define my-buttons `((continue ,(N_ "_Continue") #t)
		     (test     ,(N_ "_Test") #t)))

(define (network-page-key-handler page ch)
  (let ((menu (page-datum page 'menu))
	(nav  (page-datum page 'navigation)))

    (cond
     ((eq? ch KEY_RIGHT)
      (menu-set-active! menu #f)
      (buttons-select-next nav))

     ((eq? ch #\tab)
      (cond
       ((menu-active menu)
	  (menu-set-active! menu #f)
	  (buttons-select nav 0))
       
       ((eqv? (buttons-selected nav) (1- (buttons-n-buttons nav)))
	(menu-set-active! menu #t)
	(buttons-unselect-all nav))
       
       (else
	(buttons-select-next nav))))
     
     ((eq? ch KEY_LEFT)
      (menu-set-active! menu #f)
      (buttons-select-prev nav))

     ((eq? ch KEY_UP)
      (buttons-unselect-all nav)
      (menu-set-active! menu #t))

     ((and (select-key? ch)
           (eq? 'wireless (assq-ref (menu-get-current-item menu) 'class)))

      (let ((next (make-essid-page page (N_ "Wireless interface setup")
                                   (assq-ref (menu-get-current-item menu) 'name))))
        (set! page-stack (cons next page-stack))
        ((page-refresh next) next)))

     ((select-key? ch)
      (let ((item (menu-get-current-item menu)))
        (when (eq? (assq-ref item 'class) 'ethernet)
          (and (zero? (system* "ip" "link" "set" (assq-ref item 'name) "up"))
               (zero? (system* "dhclient" (assq-ref item 'name)))))))

     ((buttons-key-matches-symbol? nav ch 'test)
	(let ((next  (make-page (page-surface page)
				"Ping"
				ping-page-refresh
				ping-page-key-handler)))
	  
	       (set! page-stack (cons next page-stack))
	       ((page-refresh next) next)))

     ((buttons-key-matches-symbol? nav ch 'continue)
     (delwin (outer (page-wwin page)))
     (delwin (inner (page-wwin page)))
     (set! page-stack (cdr page-stack))))
    
    (std-menu-key-handler menu ch))
  #f)


(define (network-page-refresh page)
  (when (not (page-initialised? page))
    (network-page-init page)
    (page-set-initialised! page #t))
  (touchwin (outer (page-wwin page)))
  (refresh (outer (page-wwin page)))
  (refresh (inner (page-wwin page)))
  (menu-refresh (page-datum page 'menu)))


(define (network-page-init p)
  (let* ((s (page-surface p))
	 (pr (make-boxed-window  #f
	      (- (getmaxy s) 3) (- (getmaxx s) 2)
	      2 1
	      #:title (page-title p)))
	 (text-window (derwin
		       (car pr)
		       5 (getmaxx (inner pr))
		       0 0
		       #:panel #f))
			      
	 (bwin (derwin (inner pr)
		       3 (getmaxx (inner pr))
		       (- (getmaxy (inner pr)) 3) 0
			  #:panel #f))
	 (buttons (make-buttons my-buttons 1))

	 (mwin (derwin (inner pr)
		       (- (getmaxy (inner pr)) (getmaxy text-window) 3)
		       (- (getmaxx (inner pr)) 0)
		       (getmaxy text-window) 0 #:panel #f))
	 
	 (menu (make-menu
		(filter (lambda (i) (memq
                                     (assq-ref i 'class)
                                     '(ethernet wireless)))
                        (interfaces))
		#:disp-proc
		(lambda (datum row)
                  ;; Convert a network device name such as "enp0s25" to
		  ;; something more descriptive like
		  ;; "82567LM Gigabit Network Connection"
		  (let* ((name (assq-ref datum 'name))
                         (addr (string-tokenize name char-set:digit)))
                    (match addr
                      ((bus device . func)
                       (car (assoc-ref
                             (cdr
                              ;; It seems that lspci always prints an initial
                              ;; "Device: <bus>:<device>.<func> line.  We are
                              ;; not interested in this, and it conflicts with
                              ;; the "real" (descriptive) Device: line which we
                              ;; want.  Hence the above cdr strips the first line
                              ;; away.
                              (slurp (format #f "lspci -vm -s~x:~x.~x"
                                             (string->number bus 10)
                                             (string->number device 10)
                                             (if (null? func) 0
                                                 (string->number func 10)))
                                     (lambda (x)
                                       (string-split x #\tab))))
                             "Device:")))))))))
    

    (addstr*   text-window  (format #f
	      (gettext
	       "To install GuixSD a connection to one of ~s must be available.  The following network devices exist on the system.  Select one to configure or \"Continue\" to proceeed.") %default-substitute-urls))
    
    (page-set-wwin! p pr)
    (page-set-datum! p 'menu menu)
    (page-set-datum! p 'navigation buttons)
    (menu-post menu mwin)
    (buttons-post buttons bwin)
    (refresh (outer pr))
    (refresh text-window)
    (refresh bwin)))
