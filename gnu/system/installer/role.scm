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

(define-module (gnu system installer role)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (gurses menu)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (srfi srfi-9)

  #:export (role-services)
  #:export (role-packages)
  #:export (role-package-modules)
  #:export (role-service-modules)
  #:export (role?)
  #:export (make-role-page))


(define-record-type <role>
  (make-role description packages package-modules services service-modules)
  role?
  (description role-description)
  (packages role-packages)
  (package-modules role-package-modules)
  (services role-services)
  (service-modules role-service-modules))


(define (make-role-page parent  title)
  (make-page (page-surface parent)
	     title
	     role-page-refresh
	     role-page-key-handler))


(define my-buttons `((back ,(N_ "_Back") #t)))

(define (role-page-key-handler page ch)
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


     ((select-key? ch)
      (set! system-role (menu-get-current-item menu))
      
      (delwin (outer (page-wwin page)))
      (delwin (inner (page-wwin page)))
      (set! page-stack (cdr page-stack)))

     ((buttons-key-matches-symbol? nav ch 'back)
      (delwin (outer (page-wwin page)))
      (delwin (inner (page-wwin page)))
      (set! page-stack (cdr page-stack))))

    (std-menu-key-handler menu ch))
  #f)


(define (role-page-refresh page)
  (when (not (page-initialised? page))
    (role-page-init page)
    (page-set-initialised! page #t))
  (touchwin (outer (page-wwin page)))
  (refresh (outer (page-wwin page)))
  (refresh (inner (page-wwin page)))
  (menu-refresh (page-datum page 'menu)))


(define roles `(,(make-role (N_ "Headless server")
                            `(tcpdump)
                            `(admin)
                            `((dhcp-client-service)
                              (lsh-service #:port-number 2222)
                              %base-services)
                            `(networking ssh))
                ,(make-role (N_ "Lightweight desktop or laptop")
                            `(ratpoison i3-wm xmonad nss-certs)
                            `(wm ratpoison certs)
                            `(%desktop-services)
                            `(desktop))
                ,(make-role (N_ "Heavy duty workstation")
                            `(nss-certs gvfs)
                            `(certs gnome)
                            `((gnome-desktop-service)
                              (xfce-desktop-service)
                              %desktop-services)
                            `(desktop))))

(define (role-page-init p)
  (let* ((s (page-surface p))
	 (pr (make-boxed-window  #f
	      (- (getmaxy s) 4) (- (getmaxx s) 2)
	      2 1
	      #:title (page-title p)))
	 (text-window (derwin
		       (inner pr)
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

	 (menu (make-menu roles
                          #:disp-proc (lambda (datum row)
                                        (role-description datum)))))
         
    (addstr*   text-window  (format #f
	      (gettext
	       "Select from the list below the role which most closely matches the purpose of the system to be installed.")))


    (page-set-wwin! p pr)
    (page-set-datum! p 'menu menu)
    (page-set-datum! p 'navigation buttons)
    (menu-post menu mwin)
    (buttons-post buttons bwin)
    (refresh (outer pr))
    (refresh text-window)
    (refresh bwin)))
