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

(define-module (gnu system installer new))

(use-modules (ncurses curses)
	     (gurses menu)
	     (gurses buttons)
	     (gnu system installer utils)
	     (gnu system installer misc)
	     (gnu system installer partition-reader)
	     (gnu system installer disks)
	     (gnu system installer filesystems)
	     (gnu system installer hostname)
	     (gnu system installer file-browser)
	     (gnu system installer time-zone)
	     (gnu system installer network)
             (gnu system installer install)
	     (gnu system installer page)
	     (gnu system installer dialog)

             (guix build utils)
             (guix utils)
             
	     (ice-9 format)
             (ice-9 match)
	     (ice-9 pretty-print)
             (srfi srfi-1)
	     (srfi srfi-9))


(define-record-type <task>
  (make-task title dependencies complete init)
  task?
  (title task-title)
  (dependencies task-dependencies)
  (complete task-complete?)
  (init task-init))

(define partition-menu-title    (N_ "Partition the disk(s)"))
(define filesystem-menu-title   (N_ "Allocate disk partitions"))
(define network-menu-title      (N_ "Setup the network"))
(define timezone-menu-title     (N_ "Set the time zone"))
(define hostname-menu-title     (N_ "Set the host name"))
(define installation-menu-title (N_ "Install the system"))

(define (size-of-largest-disk)
  (fold (lambda (disk prev) (max (disk-size disk) prev))
        0 (volumes)))

(define main-options
  `(
    (disk . ,(make-task partition-menu-title
                        '()
                        (lambda () (< 12000 (size-of-largest-disk)))
                        (lambda (page)
                          (make-disk-page
                           page
                           partition-menu-title))))

    (filesystems . ,(make-task filesystem-menu-title
                        '(disk)
                        (lambda () (not (null? mount-points)))
                        (lambda (page)
                          (make-filesystem-page
                           page
                           filesystem-menu-title))))

    (network . ,(make-task network-menu-title
                        '()
                        (lambda () #f)
                        (lambda (page)
                          (make-network-page
                           page
                           network-menu-title))))

    (timezone . ,(make-task timezone-menu-title
                        '()
                        (lambda () (not (equal? "" time-zone)))
                        (lambda (page)
                          (make-tz-browser
                           page
                           (or
                            (getenv "TZDIR")
                            (string-append (car (slurp "guix build tzdata" #f))
                                           "/share/zoneinfo"))
        	     page-stack))))

    (hostname . ,(make-task hostname-menu-title
                            '()
                            (lambda () (not (equal? "" host-name)))
                            (lambda (page)
                              (make-host-name-page
                               page
                               hostname-menu-title))))

    (generate .  ,(make-task
                   (N_ "Generate the configuration")
                   '(filesystems timezone hostname)
                   (lambda () #f)
                   (lambda (page)
                     (make-dialog
                      page
                      (delay
                        (generate-guix-config
                         `(operating-system
                            (timezone ,time-zone)
                            (host-name ,host-name)
                            (locale "POSIX")
                            ,(let ((grub-mount-point
                                    (find-mount-device "/boot/grub"
                                                       mount-points)))
                               (if grub-mount-point
                                   `(bootloader
                                     (grub-configuration
                                      (device
                                       ,(disk-name
                                         (assoc-ref
                                          (partition-volume-pairs)
                                          (find-partition grub-mount-point))))
                                      (timeout 2)))))

                            (file-systems
                             (cons*
                              ,(map (lambda (x)
                                      (let ((z (find-partition (car x))))
                                        `(filesystem
                                          (device ,(car x))
                                          (title 'device)
                                          (mount-point ,(cdr x))
                                          (type ,(partition-fs z)))))
                                    mount-points)
                              %base-file-systems))
                            (users (cons* %base-user-accounts))
                            (packages (cons* nss-certs %base-packages))
                            (services (cons* %desktop-services))
                            (name-service-switch %mdns-host-lookup-nss))))
                      #:justify #f))))

    (install .  ,(make-task installation-menu-title
                            ;;                            '(generate network)
                            '(filesystems)
                            (lambda () #f)
                            (lambda (page)
                              (make-install-page
                               page
                               installation-menu-title))))))

(define (generate-guix-config cfg)
  (call-with-output-string
    (lambda (p) (pretty-print cfg p))))


(define (base-page-key-handler page ch)
  (cond
   ((eqv? ch (key-f 1))

    (call-with-temporary-output-file
     (lambda (file-name port)
       (endwin)
       (format port "echo '~a'\n"
               (gettext "Type \"exit\" to return to the GuixSD installer."))
       (close port)
       (system* "bash" "--rcfile" file-name))))

   ((eqv? ch (key-f 9))
    (setlocale LC_ALL "de_DE.UTF-8")
    )

   ((eqv? ch (key-f 10))
    (match (which "loadkeys")
      (#f #f)  ;; Do nothing if loadkeys is not found
      (loadkeys-directory
       (let* ((keymap-directory
               (string-append (dirname loadkeys-directory) "/../share/keymaps"))
              (p (make-file-browser
                  page keymap-directory
                  page-stack)))
         (set! page-stack (cons p page-stack))
         ((page-refresh p) p)))))))

(define (do-task task-name page)
  "Queue the task whose name is TASK-NAME and any dependencies"
  (let ((task (assoc-ref main-options task-name)))
    (set! page-stack (cons ((task-init task) page) page-stack))
    (do-task-list (task-dependencies task) page)))

(define (do-task-list task-name-list page)
  "Queue the tasks whose names are the members of TASK-NAME-LIST"

  (for-each
   (lambda (task-name)
     (let ((task (assoc-ref main-options task-name)))
       (if (not ((task-complete? task)))
           (do-task task-name page))))
   task-name-list))

(define (main-page-key-handler page ch)
  (let ((main-menu (page-datum page 'menu)))
    (std-menu-key-handler main-menu ch)
    (cond
     ((eq? ch #\newline)
      (let ((item (menu-get-current-item main-menu)))
        (do-task (car item) page)
        ((page-refresh (car page-stack)) (car page-stack)))))))

(define (main-page-init page)
  (let* ((frame (make-boxed-window (page-surface page) (lines) (cols) 0 0
                                   #:title (page-title page)))
         (background (inner frame)))

    (let ((win (derwin background (- (getmaxy background) 3)
		       (- (getmaxx background) 2) 0 1 #:panel #f))
	  (main-menu (make-menu main-options
				#:disp-proc (lambda (datum row)
                                              (format #f "~a"
                                                      (task-title (cdr datum)))))))

      (page-set-wwin! page frame)
      (page-set-datum! page 'menu main-menu)
      (menu-post main-menu win))
    
    ;; Do the key action labels
    (let ((ypos (1- (getmaxy background)))
	  (str0 (gettext "Get a Shell <F1>"))
	  (str1 (gettext "Language <F9>"))
	  (str2 (gettext "Keyboard <F10>")))
      
      (addstr background str0 #:y ypos #:x 0)
      (addstr background str1 #:y ypos #:x
	      (truncate (/ (- (getmaxx background) 
			      (string-length str1)) 2)))
      (addstr background str2 #:y ypos #:x
	      (- (getmaxx background) (string-length str2))))))


(define (main-page-refresh page)
  (when (not (page-initialised? page))
    (main-page-init page)
    (page-set-initialised! page #t))
  
  (touchwin (outer (page-wwin page)))
  (refresh (outer (page-wwin page)))
  (refresh (inner (page-wwin page)))
  (menu-refresh (page-datum page 'menu))
  (menu-redraw (page-datum page 'menu)))



(define-public (guixsd-installer)
  (define stdscr (initscr))		; Start curses
  (cbreak!)				; Line buffering disabled
  (keypad! stdscr #t)			; Check for function keys
  (noecho!)

  (start-color!)

  (init-pair! livery-title COLOR_RED COLOR_BLACK)

  (curs-set 0)

  (let ((page (make-page
               stdscr (gettext "GuixSD Installer")
               main-page-refresh main-page-key-handler)))

    (set! page-stack (cons page page-stack))
    ((page-refresh page) (car page-stack))
    (let loop ((ch (getch stdscr)))
      (let ((current-page (car page-stack)))
        ((page-key-handler current-page) current-page ch)
        (base-page-key-handler current-page ch))
      ((page-refresh (car page-stack)) (car page-stack))
      (loop (getch stdscr)))

    (endwin)))
