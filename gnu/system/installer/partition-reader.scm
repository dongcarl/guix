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

(define-module (gnu system installer partition-reader) 
  #:export (disk?
	    partition?
	    disk-vendor
	    disk-size
	    disk-name
	    disk-type
	    disk-partitions

	    partition-number
	    partition-size
	    partition-fs
	    partition-name

	    partition-volume-pairs
	    
	    number->size

	    find-partition
	    
            volumes)
  
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (gnu system installer utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-9))

(define (number->size n)
  "Convert a number of megabytes into a human readable size representation"
  (let ((pr 
  (let loop ((q (* n 1000))
	     (m 0))
    (if (and (integer? (/ q 100))
	     (>= (/ q 1000.0) 1))
	(loop (/ q 1000) (1+ m))
	(cons q m)))))
    (format #f "~h~aB" (car pr)
	    (case (cdr pr)
	      ((0) #\K)
	      ((1) #\M)
	      ((2) #\G)
	      ((3) #\T)
	      ((4) #\P)
	      (else (error "Extremely large"))))))

(define (size->number size)
  "Convert a size string in the form 12.34[KMGT]B into a number, representing the
number of Megabytes"
  (let* ((threshold (1+ (string-index-right size char-set:digit)))
	 (quantity (string->number (substring size 0 threshold)))
	 (unit (substring size threshold))
	 (multiplier 
	  (cond
	   ((equal? "KB" unit)
	    0.001)
	   ((equal? "MB" unit)
	    1)
	   ((equal? "GB" unit)
	    1000)
	   ((equal? "TB" unit)
	    1000000)
	   )))

    (* multiplier quantity)))



(define-record-type <partition>
  (make-partition number start stop size fs type flags)
  partition?
  (number partition-number)
  (name  partition-name partition-set-name!)
  (start partition-start)
  (stop  partition-stop)
  (size  partition-size)
  (fs partition-fs)
  (type partition-type)
  (flags partition-flags))

(define-record-type <disk>
  (make-disk name size type logical-sector-size physical-sector-size table
	     vendor xx)
  disk?
  (name disk-name)
  (size disk-size)
  (type disk-type)
  (logical-sector-size disk-logical-sector-size)
  (physical-sector-size disk-physical-sector-size)
  (table disk-table)
  (vendor disk-vendor)
  (xx disk-xx) ; I have no idea what this field means
  (partitions disk-partitions disk-set-partitions!))


(define (read-line-drop-semi port)
  (let ((line (read-line port)))
    (if (eq? #\;
	     (string-ref line (1- (string-length line))))
	(string-drop-right line 1)
	line)))

(define (parse-disk port disk-list)
  (if (not (string=? "BYT" (read-line-drop-semi port)))
      (error "Expected BYT;"))
  
  (let ((line (read-line-drop-semi port)))
    (match (string-split line #\:)
      ((name size type logical physical table vendor xx)
       (cons
	(make-disk name (size->number size) type logical physical table vendor xx)
	disk-list)))))


(define (parse-partition port partition-list)
  (let ((line (read-line-drop-semi port)))
    (match (string-split line #\:)
      ((number start stop size fs type flags)
       (cons 
	(make-partition number start stop
			(size->number size)
			fs type flags)
	partition-list)))))

(define (read-partition-info)
  (define (read-partition-info' port l)
    (let ((line (read-line port)))
      (if (eof-object? line)
	  l
	  (if (or (zero? (string-length line))
		  (string-match "[\t ][\t ]*" line))
	      (read-partition-info' port l)
	      (begin
		(unread-string (string-append line "\n") port)
		(read-partition-info' port
			      (if (string=? "BYT;" line)
				  (parse-disk port l)
				  (parse-partition port l))))))))

  (let* ((port (open-input-pipe-with-fallback "parted -lm"))
	 (r (read-partition-info' port '())))
    (close-pipe port)
    r))

(define (assemble-partitions input disks partitions)
  (if (null? input)
      disks
      (if (disk? (car input))
	  (let ((current-disk (car input)))
	    (disk-set-partitions! current-disk partitions)
	    (map
	     (lambda (p) (partition-set-name! p (device-id (cons p current-disk))))
	     partitions)
	    (assemble-partitions (cdr input) (cons current-disk disks)  '()))
	  (assemble-partitions (cdr input) disks (cons (car input) partitions)))))

(define (disk-volumes)
  "Return a list of disk volumes on the current machine"
  (assemble-partitions (read-partition-info) '() '()))

(define (volumes)
  "Return a list of disk volumes on the current machine, excluding mappers"
  (filter (lambda (v) (not (equal? "dm" (disk-type v))))
	  (disk-volumes)))



(define (device-id  pr)
  "Given PR which is a (partition . volume) pair return the string
representing its name"
  (let ((volume (cdr pr))
	(part (car pr)))
    (string-append (disk-name volume)
		   (if (equal? "dm" (disk-type volume))
		       ""
		       (partition-number part)))))

;;  Return a list of pairs whose CAR is a partition and whose CDR is the volume
;;  on which that partition resides
(define (partition-volume-pairs)
  (let loop ((volumes (disk-volumes))
	     (partitions '()))
    (if (null? volumes)
	partitions
	(loop (cdr volumes)
	      (append partitions
		      (map-in-order (lambda (part) (cons part (car volumes)))
				    (disk-partitions (car volumes))))))))

(define (find-partition target)
  "Return the partition whose name is TARGET"
  (let loop ((p (partition-volume-pairs)))
    (if (not (null? p))
	(let* ((pr (car p))
	       (part (car pr))
	       (name (partition-name part)))
	  (if (equal? name target)
	      part
	      (loop (cdr p)))))))
