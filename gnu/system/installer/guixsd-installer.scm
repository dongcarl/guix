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

(define-module (gnu system installer guixsd-installer))

(use-modules (ncurses curses)
	     (gurses menu)
	     (gurses buttons)
	     (gnu system installer utils)
	     (gnu system installer misc)
	     (gnu system installer partition-reader)
	     (gnu system installer disks)
	     (gnu system installer configure)
             (gnu system installer filesystems)
	     (gnu system installer hostname)
             (gnu system installer locale)
	     (gnu system installer key-map)
	     (gnu system installer time-zone)
             (gnu system installer role)
	     (gnu system installer network)
             (gnu system installer install)
             (gnu system installer format)
	     (gnu system installer page)
             (gnu system installer ping)
	     (gnu system installer dialog)

             (guix build utils)
             (guix utils)

	     (ice-9 format)
             (ice-9 match)
	     (ice-9 pretty-print)
             (srfi srfi-1)
	     (srfi srfi-9))

(include "i18n.scm")

(define-record-type <task>
  (make-task title dependencies complete init)
  task?
  (title task-title)
  (dependencies task-dependencies)
  (complete task-complete?)
  (init task-init))

(define partition-menu-title    (M_ "Partition the disk(s)"))
(define filesystem-menu-title   (M_ "Allocate disk partitions"))
(define format-menu-title       (M_ "Format the partitions"))
(define network-menu-title      (M_ "Set up the network"))
(define timezone-menu-title     (M_ "Set the time zone"))
(define hostname-menu-title     (M_ "Set the host name"))
(define installation-menu-title (M_ "Install the system"))
(define role-menu-title         (M_ "Select a role for the system"))
(define generate-menu-title     (M_ "Generate the configuration"))

(define (size-of-largest-disk)
  (fold (lambda (disk prev) (max (disk-size disk) prev))
        0 (volumes)))

(define main-options
  `((disk . ,(make-task partition-menu-title
                        '()
                        (lambda () (< minimum-store-size (size-of-largest-disk)))
                        (lambda (page)
                          (make-disk-page
                           page
                           partition-menu-title))))

    (filesystems . ,(make-task filesystem-menu-title
                               '(disk hostname)
                               filesystem-task-complete?
                               (lambda (page)
                                 (make-filesystem-page
                                  page
                                  filesystem-menu-title))))

    (format . ,(make-task format-menu-title
                          '(filesystems)
                          filesystems-are-current?
                          (lambda (page)
                            (make-format-page
                             page
                             format-menu-title))))

    (network . ,(make-task network-menu-title
                           '()
                           substitute-is-reachable?
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
                                               "/share/zoneinfo"))))))

    (hostname . ,(make-task hostname-menu-title
                            '()
                            (lambda () (valid-hostname? host-name))
                            (lambda (page)
                              (make-host-name-page
                               page
                               hostname-menu-title))))

    (role . ,(make-task role-menu-title
                            '()
                            (lambda () (and system-role (role? system-role)))
                            (lambda (page)
                              (make-role-page
                               page
                               role-menu-title))))

    (generate . , (make-task generate-menu-title
                             '(role filesystems timezone)
                             (lambda ()
                               (and config-file
                                    (file-exists? config-file)
                                    (positive? (stat:size (stat config-file)))))

                             (lambda (page)
                               (make-configure-page
                                page
                                generate-menu-title))))

    (install .  ,(make-task installation-menu-title
                            '(network generate format)
                            (lambda () #f)
                            (lambda (page)
                              (make-install-page
                               page
                               installation-menu-title))))))

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
    (let ((p (make-locale-page page (gettext "Change locale"))))
      (page-enter p)))

   ((eqv? ch (key-f 10))
    (match (which "loadkeys")
      (#f #f)  ;; Do nothing if loadkeys is not found
      (loadkeys-directory
       (let* ((keymap-directory
               (string-append (dirname loadkeys-directory) "/../share/keymaps"))
              (p (make-key-map
                  page keymap-directory)))
         (page-enter p)))))))

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


(define (uniquify in)
  "Remove duplicates from the list IN. Keep the items which are closest to the
tail of the list."
  (let loop ((l (reverse in))
             (acc '()))
    (if (null? l)
        acc
        (loop (cdr l)
              (if (member (car l) (cdr l))
                  acc
                  (cons (car l) acc))))))

(define (main-page-key-handler page ch)
  (let ((main-menu (page-datum page 'menu)))
    (std-menu-key-handler main-menu ch)
    (cond
     ((eq? ch #\newline)
      (let ((item (menu-get-current-item main-menu)))
        (do-task (car item) page)
        (set! page-stack (uniquify page-stack))
        ((page-refresh (car page-stack)) (car page-stack)))))))

(define (main-page-init page)
  (let* ((frame (make-boxed-window (page-surface page) (lines) (cols) 0 0
                                   #:title (page-title page)))
         (background (inner frame))


	 (text-window (derwin background 4 (getmaxx background)
			      0 0 #:panel #f))

         (win (derwin background (- (getmaxy background) (getmaxy text-window) 3)
                      (- (getmaxx background) 2) (getmaxy text-window) 1 #:panel #f))

         (main-menu (make-menu main-options
                               #:disp-proc (lambda (datum row)
                                             (gettext (task-title (cdr datum)))))))


    (page-set-wwin! page frame)
    (page-set-datum! page 'menu main-menu)
    (page-set-datum! page 'text-window text-window)
    (page-set-datum! page 'background background)
    (menu-post main-menu win)

    (push-cursor (page-cursor-visibility page))))


(define (main-page-refresh page)
  (when (not (page-initialised? page))
    (main-page-init page)
    (page-set-initialised! page #t))

  (let ((text-window (page-datum page 'text-window))
        (menu (page-datum page 'menu))
        (background (page-datum page 'background)))

    (clear background)

    (addstr*
     text-window
     (format
      #f
      (gettext
       "To start the complete installation process, choose ~s.  Alternatively, you may run each step individually for a slower, more controlled experience.")
      (gettext installation-menu-title)))


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
              (- (getmaxx background) (string-length str2))))

    (touchwin (outer (page-wwin page)))
    (refresh* (outer (page-wwin page)))
    (refresh* (inner (page-wwin page)))
    (menu-redraw menu)
    (menu-refresh menu)))


(define-public (guixsd-installer)
  (catch #t
    (lambda ()

      (define stdscr (initscr))		; Start curses

      ;; We don't want any nasty kernel messages damaging our beautifully
      ;; crafted display.
      (system* "dmesg" "--console-off")

      (cbreak!)				; Line buffering disabled
      (keypad! stdscr #t)			; Check for function keys
      (noecho!)

      (start-color!)

      (init-pair! livery-title COLOR_MAGENTA COLOR_BLACK)
      (init-pair! strong-colour COLOR_RED COLOR_BLACK)

      (curs-set 0)

      (let ((page (make-page
                   stdscr (gettext "GuixSD Installer")
                   main-page-refresh 0 main-page-key-handler)))

        (page-enter page)
        (let loop ((ch (getch stdscr)))
          (let ((current-page (car page-stack)))
            ((page-key-handler current-page) current-page ch)
            (base-page-key-handler current-page ch))
          ((page-refresh (car page-stack)) (car page-stack))
          (loop (getch stdscr)))

        (endwin)))
    (lambda (key . args)
      (system* "dmesg" "--console-on")
      (exit 2))
    (lambda (key subr message args rest)
      (let ((s (make-stack #t 3 primitive-load)))
        (endwin)
        (display-backtrace s (current-error-port))
        (display-error (stack-ref s 0)
                       (current-error-port) subr message args rest)))))
