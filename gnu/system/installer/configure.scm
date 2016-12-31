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

(define-module (gnu system installer configure)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer ping)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module  (gnu system installer misc)
  #:use-module  (gnu system installer partition-reader)
  #:use-module  (gnu system installer disks)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (guix store)
  #:use-module (guix utils)


  #:export (make-configure-page))


(define (make-configure-page parent title)
  (let ((page (make-page (page-surface parent)
                         title
                         configure-page-refresh
                         configure-page-key-handler)))
    page))


(define my-buttons `((save ,(N_ "_Save") #t)
                     (back ,(N_ "_Back") #t)))


;; Kludge!  For testing.
(define tempdir
  (if
   ;; Try to infer whether we are running the installation image or
   ;; if we are just testing.
   (and (file-exists? "/etc/issue")
        (let loop ((p (open "/etc/issue" O_RDONLY)))
          (let ((l (read-line p)))
            (cond
             ((eof-object? l)
              (close p)
              #f)
             ((string-contains l "installation image")
              (close p)
              #t)
             (else
              (loop p))))))
   ;; In the installer image we cannot use /tmp because the cow-store
   ;; does not play nicely with it.  Bug 25286 refers.
   ""
   "/tmp"))

(define (configure-page-key-handler page ch)

  (let ((nav  (page-datum page 'navigation))
	(test-window  (page-datum page 'test-window)))

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



     ((buttons-key-matches-symbol? nav ch 'save)

      ;; Write the configuration and set the file name
      (let ((cfg-port (mkstemp! (string-copy
                                 (string-append tempdir "/guix-config-XXXXXX")))))
        (generate-guix-config cfg-port)
        (set! config-file (port-filename cfg-port))
        (close-port cfg-port))

      ;; Close the menu and return 
      (delwin (outer (page-wwin page)))
      (delwin (inner (page-wwin page)))
      (set! page-stack (cdr page-stack)))
     )

    #f))

(define (configure-page-refresh page)
  (when (not (page-initialised? page))
    (configure-page-init page)
    (page-set-initialised! page #t))
  (touchwin (outer (page-wwin page)))
  (refresh (outer (page-wwin page)))
  (refresh (inner (page-wwin page))))


(define (generate-guix-config p)
  (let ((grub-mount-point
         (find-mount-device "/boot/grub"
                            mount-points)))
    
    (pretty-print `(use-modules
                    (gnu)
                    ,(when grub-mount-point
                       `(gnu system grub))

                    (gnu system nss))
                  p)
    (newline p)

    (pretty-print 
     `(use-service-modules desktop) p)
    (newline p)

    (pretty-print
     `(use-package-modules certs) p)
    (newline p)

    (pretty-print
     `(operating-system
        (timezone ,time-zone)
        (host-name ,host-name)
        (locale "en_US.UTF-8")
        ,(when grub-mount-point
           `(bootloader
             (grub-configuration
              (device
               ,(disk-name
                 (assoc-ref
                  (partition-volume-pairs)
                  (find-partition grub-mount-point))))
              (timeout 2))))
        
        (file-systems
         ,(append (list 'cons*)
                  (map (lambda (x)
                         (let ((z (find-partition (car x))))
                           `(file-system
                              (device ,(car x))
                              (title 'device)
                              (mount-point ,(cdr x))
                              (type ,(partition-fs z))))) mount-points)
                  (list '%base-file-systems)))
        (users (cons* %base-user-accounts))
        (packages (cons* nss-certs %base-packages))
        (services (cons* %desktop-services))
        (name-service-switch %mdns-host-lookup-nss)) p)))


(define (configure-page-init p)
  (let* ((s (page-surface p))
	 (pr (make-boxed-window  #f
	      (- (getmaxy s) 4) (- (getmaxx s) 2)
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
                         0)))

    (addstr* text-window
             (gettext
              "The following configuration has been generated for you.  If you are satisfied with it you may save it and continue.  Otherwise go back and change some options."))

    (let ((p (make-window-port (inner config-window))))
      (generate-guix-config p)
      (force-output p))

    (page-set-wwin! p pr)
    (page-set-datum! p 'navigation buttons)
    (buttons-post buttons bwin)
    (refresh (outer pr))
    (refresh text-window)

    (refresh (outer config-window))

    (refresh bwin)))
