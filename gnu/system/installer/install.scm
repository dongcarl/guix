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

(define-module (gnu system installer install)
  #:use-module (gnu system installer page)
  #:use-module (gnu system installer ping)
  #:use-module (gnu system installer misc)
  #:use-module (gnu system installer utils)
  #:use-module (gnu system installer filesystems)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (gurses buttons)
  #:use-module (ncurses curses)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module ((guix build syscalls) #:select (mount umount))
  #:use-module (guix build utils)

  #:export (make-install-page))

(include "i18n.scm")

(define (make-install-page parent title)
  (let ((page (make-page (page-surface parent)
                         title
                         install-page-refresh
                         0
                         #:activator
                         install-page-activate-item)))
    page))


(define my-buttons `((continue ,(M_ "_Continue") #t)
                     (reboot ,(M_ "_Reboot") #t)
                     (cancel ,(M_ "Canc_el") #t)))


;; We have to use this "hard" way of rebooting, because
;; we have instructed the user to remove the device which
;; contains our root filesystem
(define (force-reboot)
  (catch
   #t
   (lambda ()
     (let ((p (open "/proc/sys/kernel/sysrq" O_WRONLY)))
       (display "1\n" p)
       (close p))

     (let ((p (open "/proc/sysrq-trigger" O_WRONLY)))
       (display "b\n" p)
       (close p)))

   (lambda (key . args)
     #f)
   (lambda (key subr message args . rest)
     #f)))


(define (install-page-mouse-handler page device-id x y z button-state)
  'ignored)

(define (install-page-activate-item page item)
  (let ((config-window  (page-datum page 'config-window)))
    (match item
     ('cancel
      ;; Close the menu and return
      (page-leave)
      'handled)

     ('reboot
      (force-reboot)
      'handled)

     ('continue
      (let ((target (format #f "/target-~a" install-attempts))
            (window-port (make-window-port config-window)))
        (catch #t
               (lambda ()
                 (force-output window-port)
                 (set! install-attempts (1+ install-attempts))
                 (and
                  (fold
                   (lambda (x prev)
                     (and prev
                          (let* ((device (car x))
                                 (fss (cdr x))
                                 (mp (file-system-spec-mount-point fss))
                                 (mpt (string-append target mp)))
                            (mkdir-p mpt)
                            (mount device mpt
                                   (symbol->string
                                    (file-system-spec-type fss))
                                   #:update-mtab? #f))))
                   #t
                   (sort
                    mount-points
                    (lambda (x y)
                      (< (string-length (file-system-spec-mount-point (cdr x)))
                         (string-length (file-system-spec-mount-point (cdr y)))))))

                 (zero? (pipe-cmd window-port  "herd"
                                  "herd" "start" "cow-store" target))

                 (mkdir-p (string-append target "/etc"))
                 (or (copy-file config-file
                                (string-append target "/etc/config.scm"))
                     #t)

                 (file-exists? (string-append target "/etc/config.scm"))

                 (display (gettext "Installing the system ...") window-port)
                 (force-output window-port)

                 (zero? (pipe-cmd window-port "guix" "guix" "system" "init" "--fallback"
                                  (string-append target "/etc/config.scm")
                                  target))

                 (display (gettext
                           "Installation is complete.  You should remove the device containing the installer image and reboot now.")
                          window-port)))
        (lambda (key . args)
          #f)
        (lambda (key subr message args . rest)
          (display-error (stack-ref (make-stack #t) 3)
                         window-port subr message args rest)))

      (close-port window-port))
      'handled)
     (_ 'ignored))))

(define (install-page-refresh page)
  (when (not (page-initialised? page))
    (install-page-init page)
    (page-set-initialised! page #t)))

(define (install-page-init p)
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
                         0))
         )

    (addstr* text-window
             (gettext
              "Choose \"Continue\" to start installing the system."))

    (push-cursor (page-cursor-visibility p))
    (page-set-datum! p 'navigation buttons)
    (page-set-datum! p 'config-window (inner config-window))
    (page-set-datum! p 'config-window-port (open-output-string))
    (buttons-post buttons bwin)))

