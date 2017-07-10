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

(define-module (gnu system installer configure)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer ping)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer role)
  #:use-module (gnu system installer partition-reader)
  #:use-module (gnu system installer filesystems)
  #:use-module (gnu system installer disks)
  #:use-module (gnu system shadow)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (guix store)
  #:use-module (guix utils)

  #:export (make-configure-page))

(include "i18n.scm")

(define (make-configure-page parent title)
  (let ((page (make-page (page-surface parent)
                         title
                         configure-page-refresh
                         0
                         #:activator configure-page-activate-item)))
    page))


(define my-buttons `((save ,(M_ "_Save") #t)
                     (cancel ,(M_ "Canc_el") #t)))


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

(define (configure-page-activate-item page item)
  (match item
    ('cancel
      ;; Close the menu and return
      (page-leave)
      'cancelled)

    ('save
      ;; Write the configuration and set the file name
      (let ((cfg-port (mkstemp! (string-copy
                                 (string-append tempdir "/guix-config-XXXXXX")))))
        (generate-guix-config cfg-port 79)
        (set! config-file (port-filename cfg-port))
        (close-port cfg-port))

      ;; Close the menu and return
      (page-leave)
      'handled)
    (_ 'ignored)))

(define (configure-page-refresh page)
  (when (not (page-initialised? page))
    (configure-page-init page)
    (page-set-initialised! page #t))
  (let ((text-window (page-datum page 'text-window)))
    (addstr* text-window
             (gettext
              "The following configuration has been generated for you.  If you are satisfied with it you may save it and continue.  Otherwise go back and change some options."))))

(define (generate-guix-config p width)
  (let ((grub-mount-point
         (find-mount-device "/boot/grub"
                                 mount-points)))

    (pretty-print `(use-modules
                    (gnu)
                    ,(when grub-mount-point
                       `(gnu bootloader grub))

                    (gnu system nss))
                  p #:width width)
    (newline p)

    (when system-role
          (pretty-print
           `(use-service-modules
             ,@(role-service-modules system-role)) p #:width width)
          (newline p))

    (when system-role
          (pretty-print
           `(use-package-modules
             ,@(role-package-modules system-role)) p #:width width)
          (newline p))

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
                         (let ((fss (cdr x)))
                           `(file-system
                              (device ,(file-system-spec-label fss))
                              (title 'label)
                              (mount-point ,(file-system-spec-mount-point fss))
                              (type ,(symbol->string (file-system-spec-type fss))))))
                       (filter (lambda (x)
                                 (let ((fss (cdr x)))
                                   (not (eq? 'swap (file-system-spec-type fss)))))
                                 mount-points))
                  (list '%base-file-systems)))
        (swap-devices '
         ,(map (lambda (x)
                 (car x))
               (filter (lambda (x)
                         (let ((fss (cdr x)))
                           (eq? 'swap (file-system-spec-type fss))))
                       mount-points)))
        (users (cons*
                ,@(map (lambda (account)
                        (list 'user-account
                              (list 'name (user-account-name account))
                              (list 'group (user-account-group account))
                              (list 'supplementary-groups
                                    `(quote ,(user-account-supplementary-groups account)))
                              (list 'comment (user-account-comment account))
                              (list 'home-directory (user-account-home-directory account))))
                      users)
                %base-user-accounts))
        (packages (cons*
                   ,@(if system-role
                         (role-packages system-role)
                         '())
                   %base-packages))
        (services (cons*
                   ,@(if key-map
                        `((console-keymap-service ,key-map))
                        `())
                   ,@(if system-role
                         (role-services system-role)
                         '())))
        (name-service-switch %mdns-host-lookup-nss)) p #:width width)))

(define (configure-page-init p)
  (let* ((s (page-surface p))
	 (text-window (derwin
		       s
		       3 (getmaxx s)
		       0 0
		       #:panel #t))

	 (bwin (derwin s
		       3 (getmaxx s)
		       (- (getmaxy s) 3) 0
			  #:panel #t))
	 (buttons (make-buttons my-buttons))

         (config-window (make-boxed-window
                         s
                         (- (getmaxy s)
                            (getmaxy bwin)
                            (getmaxy text-window))
                         (getmaxx s)
                         (getmaxy text-window)
                         0)))

    (let ((port (open-output-string)))
      (generate-guix-config port (getmaxx (inner config-window)))
      (force-output port)
      (page-set-datum! p 'config-window-port port))

    (push-cursor (page-cursor-visibility p))
    (page-set-datum! p 'navigation buttons)
    (page-set-datum! p 'text-window text-window)
    (page-set-datum! p 'config-window config-window)
    (buttons-post buttons bwin)))
