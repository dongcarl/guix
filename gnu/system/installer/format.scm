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

(define-module (gnu system installer format)
   #:use-module (gnu system installer page)
   #:use-module (gnu system installer misc)
   #:use-module (gnu system installer filesystems)
   #:use-module (gnu system installer utils)
   #:use-module (ice-9 rdelim)
   #:use-module (ice-9 match)
   #:use-module (gurses buttons)
   #:use-module (ncurses curses)
   #:use-module (srfi srfi-1)

   #:export (filesystems-are-current?)
   #:export (make-format-page))

(define-syntax M_
  (syntax-rules ()
    ((M_ str)
     str)))

(define (device-fs-uuid dev)
  "Retrieve the UUID of the filesystem on DEV, where DEV is the name of the
device such as /dev/sda1"
  (match (assoc-ref
          (slurp (string-append "blkid -o export " dev)
                 (lambda (x)
                   (string-split x #\=))) "UUID")
         (() #f)
         ((? list? l)
          (car l))
         (_ #f)))

(define (filesystems-are-current?)
  "Returns #t iff there is at least one mount point AND all mount-points' uuids
match those uuids read from the respective partitions"
  (and (not (null? mount-points))
       (fold (lambda (mp prev)
               (and prev
                    (match mp
                           ((dev . (? file-system-spec? fss))
                            (equal? (device-fs-uuid dev)
                                    (file-system-spec-uuid fss))))))
             #t mount-points)))

(define (make-format-page parent title)
  (let ((page (make-page (page-surface parent)
                         title
                         format-page-refresh
                         0
                         format-page-key-handler)))
    page))


(define my-buttons `((format ,(M_ "_Format") #t)
                     (cancel ,(M_ "Canc_el") #t)))


(define (format-page-key-handler page ch)

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


     ((buttons-key-matches-symbol? nav ch 'cancel)
      ;; Close the menu and return
      (page-leave))


     ((buttons-key-matches-symbol? nav ch 'format)
      (let ((window-port (make-window-port config-window)))
        (for-each
         (lambda (x)
           (match x
                  ((dev . ($ <file-system-spec> mp label type uuid))
                   (let ((type-str (symbol->string type)))
                     (cond
                      ((string-prefix? "ext" type-str)
                       (let ((cmd (string-append "mkfs." type-str)))
                         (zero? (pipe-cmd window-port
                                          cmd cmd
                                          "-L" label
                                          "-U" uuid
                                          "-v"
                                          dev))))

                      ((eq? type 'btrfs)
                       (let ((cmd (string-append "mkfs.btrfs")))
                         (zero? (pipe-cmd window-port
                                          cmd cmd
                                          "-L" label
                                          "-U" uuid
                                          "-f"
                                          dev))))

                      ((eq? type 'swap)
                       (let ((cmd (string-append "mkswap")))
                         (zero? (pipe-cmd window-port
                                          cmd cmd
                                          "-L" label
                                          "-U" uuid
                                          "-f"
                                          dev))))

                      ))))) mount-points)

        (close-port window-port))

      (when (filesystems-are-current?)
            (page-leave))
      ))

    #f))

(define (format-page-refresh page)
  (when (not (page-initialised? page))
    (format-page-init page)
    (page-set-initialised! page #t))
  (touchwin (outer (page-wwin page)))
  (refresh* (outer (page-wwin page)))
  (refresh* (inner (page-wwin page))))


(define (format-page-init p)
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
               (format #f
                       "The partitions ~s will be formatted.  All data on these partitions will be destroyed if you continue."
                       (map (lambda (x)
                              (car x))
                            mount-points))))



    (push-cursor (page-cursor-visibility p))
    (page-set-wwin! p pr)
    (page-set-datum! p 'navigation buttons)
    (page-set-datum! p 'config-window (inner config-window))
    (buttons-post buttons bwin)
    (refresh* (outer pr))
    (refresh* text-window)

    (refresh* (outer config-window))

    (refresh* bwin)))
