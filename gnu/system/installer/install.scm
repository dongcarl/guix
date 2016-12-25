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

(define-module (gnu system installer install)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer ping)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix build utils)

  #:export (make-install-page))


(define (make-install-page parent title)
  (let ((page (make-page (page-surface parent)
                         title
                         install-page-refresh
                         install-page-key-handler)))

    page))


(define my-buttons `((continue ,(N_ "_Continue") #t)
                     (back ,(N_ "_Back") #t)))

(define (install-page-key-handler page ch)
  (let ((nav  (page-datum page 'navigation))
        (config-window  (page-datum page 'config-window)))

    (cond
     ((eq? ch KEY_RIGHT)
      (buttons-select-next nav))

     ((eq? ch #\tab)
      (cond
       ((eqv? (buttons-selected nav) (1- (buttons-n-buttons nav)))
	(buttons-unselect-all nav))

       (else
	(buttons-select-next nav))))

     ((eq? ch KEY_LEFT)
      (buttons-select-prev nav))

     ((eq? ch KEY_UP)
      (buttons-unselect-all nav))


     ((buttons-key-matches-symbol? nav ch 'back)
      ;; Close the menu and return
      (delwin (outer (page-wwin page)))
      (delwin (inner (page-wwin page)))
      (set! page-stack (cdr page-stack)))

     ((buttons-key-matches-symbol? nav ch 'continue)
      (let ((target "/target")
            (tmp-config (port-filename %temporary-configuration-file-port))
            (window-port (make-window-port config-window))
            (root-device (find-mount-device "/" mount-points)))

        (catch #t
          (lambda ()
           (and
             ;; Undo any previous attempt to install ...
             (or (pipe-cmd window-port  "herd"
                              "herd" "stop" "cow-store")
                 #t)

             (or (pipe-cmd window-port "umount"
                              "umount" target) #t)

             (mkdir-p target)
             (zero? (pipe-cmd window-port "mount"
                                 "mount" "-t" "ext4" root-device target))
             (mkdir-p (string-append target "/etc"))
             (or (copy-file tmp-config
                            (string-append target "/etc/config.scm"))
                 #t)

             (file-exists? (string-append target "/etc/config.scm"))

             ;; Cow store seems to mess with temporary files.
             (zero? (pipe-cmd window-port  "herd"
                                 "herd" "start" "cow-store" target))

             (zero? (pipe-cmd window-port "guix" "guix" "system" "init"
                                 (string-append target "/etc/config.scm")
                                 target))))
          (lambda (key . args)
            (addstr* config-window
                     (gettext
                      (format #f "A \"~s\" exception occured: ~s" key args))))
          )
        (close-port window-port))))
       #f
     )
    )

(define (install-page-refresh page)
  (when (not (page-initialised? page))
    (install-page-init page)
    (page-set-initialised! page #t))
  (touchwin (outer (page-wwin page)))
  (refresh (outer (page-wwin page)))
  (refresh (inner (page-wwin page))))


(define (install-page-init p)
  (let* ((s (page-surface p))
	 (pr (make-boxed-window  #f
	      (- (getmaxy s) 3) (- (getmaxx s) 2)
	      2 1
	      #:title (page-title p)))
	 (text-window (derwin
		       (inner pr)
		       3 (getmaxx (inner pr))
		       0 0
		       #:panel #f))
			
	 (bwin (derwin (inner pr)
		       3 (getmaxx (inner pr))
		       (- (getmaxy (inner pr)) 3) 0
			  #:panel #f))
	 (buttons (make-buttons my-buttons 1))

         (config-window (make-boxed-window
                         (inner pr)
                         (- (getmaxy (inner pr))
                            (getmaxy bwin)
                            (getmaxy text-window))
                         (getmaxx (inner pr))
                         (getmaxy text-window)
                         0))
         )


    (addstr* text-window
             (gettext
              "Choose \"Continue\" to start installing the system."))

    (page-set-wwin! p pr)
    (page-set-datum! p 'navigation buttons)
    (page-set-datum! p 'config-window (inner config-window))
    (buttons-post buttons bwin)
    (refresh (outer pr))
    (refresh text-window)
    (refresh bwin)))
			
