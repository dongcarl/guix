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
   #:use-module (guix ui)
   #:use-module (ice-9 rdelim)
   #:use-module (ice-9 match)
   #:use-module (gurses buttons)
   #:use-module (ncurses curses)
   #:use-module (srfi srfi-1)
   #:use-module (texinfo)
   #:use-module (gurses stexi)

   #:export (filesystems-are-current?)
   #:export (make-format-page))

(include "i18n.scm")

(define (device-attributes dev)
  (key-value-slurp* "blkid" "-o" "export" dev))

(define (device-fs-uuid dev)
  "Retrieve the UUID of the filesystem on DEV, where DEV is the name of the
device such as /dev/sda1"
  (assq-ref (device-attributes dev) 'uuid))

(define (device-fs-label dev)
  "Retrieve the LABEL of the filesystem on DEV, where DEV is the name of the
device such as /dev/sda1"
  (assq-ref (device-attributes dev) 'label))

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
                         #:activator format-page-activate-item)))
    page))

(define my-buttons `((format ,(M_ "_Format") #t)
                     (cancel ,(M_ "Canc_el") #t)))

(define (format-page-activate-item page item)
  (let ((config-window  (page-datum page 'config-window)))
    (match item
     ('cancel
      ;; Close the menu and return
      (page-leave)
      'cancelled)
     ('format
      (let ((window-port (make-window-port (inner config-window))))
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
      'handled)
     (_ 'ignored))))

(define (format-page-refresh page)
  (when (not (page-initialised? page))
    (format-page-init page)
    (page-set-initialised! page #t))

  (let ((text-window (page-datum page 'text-window))
        (config-window (page-datum page 'config-window)))
    (erase text-window)
    (render-stexi
     text-window
     (texi-fragment->stexi
       (format #f
               (G_ "The partitions ~s will be formatted.  @strong{Any existing data on these partitions will be destroyed if you continue!!}")
               (map (lambda (x) (car x))
                    mount-points)))
     #:markup-table installer-texinfo-markup))
    ; TODO refresh inner config-window
)

(define (format-page-init p)
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
    (push-cursor (page-cursor-visibility p))
    (page-set-datum! p 'navigation buttons)
    (page-set-datum! p 'text-window text-window)
    (page-set-datum! p 'config-window config-window)
    (page-set-datum! p 'config-window-port (open-output-string))
    (buttons-post buttons bwin)
    (buttons-select-by-symbol buttons 'cancel)))
