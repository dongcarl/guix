;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Andy Wingo <wingo@pobox.com>
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

(define-module (guix potluck host)
  #:use-module (guix config)
  #:use-module (guix base32)
  #:use-module (guix ui)
  #:use-module ((guix build utils)
                #:select (mkdir-p
                          delete-file-recursively
                          with-directory-excursion))
  #:use-module (guix git)
  #:use-module (guix utils)
  #:use-module (guix potluck packages)
  #:use-module (guix potluck build-systems)
  #:use-module (guix potluck licenses)
  #:use-module (guix scripts)
  #:use-module (guix scripts hash)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 q)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 threads)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-37)
  #:use-module (sxml simple)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:export (host-potluck))


;;;
;;; lossy log
;;;

(define-record-type <log>
  (%make-log buffer read-pos write-pos mutex)
  log?
  (buffer log-buffer) ; vector
  (read-pos log-read-pos set-log-read-pos!) ; natural
  (write-pos log-write-pos set-log-write-pos!) ; natural
  (mutex log-mutex))

(set-record-type-printer!
 <log>
 (lambda (log port)
   (format port "<log ~a-~a ~a>" (log-read-pos log) (log-write-pos log)
           (object-address log))))

(define* (make-log #:key (capacity 100))
  (%make-log (make-vector capacity) 0 0 (make-mutex)))

(define (log-capacity log)
  (vector-length (log-buffer log)))

(define (log-pos->idx log pos)
  (modulo pos (log-capacity log)))

(define (add-log-entry log obj)
  (with-mutex (log-mutex log)
    (let ((pos (log-write-pos log))
          (entry (cons (gettimeofday) obj)))
      (vector-set! (log-buffer log) (log-pos->idx log pos) entry)
      (set-log-write-pos! log (1+ pos))
      (set-log-read-pos! log (max (log-read-pos log)
                                  (1+ (- pos (log-capacity log))))))))

(define (sxml-log log . args)
  (add-log-entry log (cons 'span args)))

(define (format-log log fmt . args)
  (add-log-entry log (apply format #f fmt args)))

(define (read-log-entries log)
  (with-mutex (log-mutex log)
    (let ((end (log-write-pos log))
          (buf (log-buffer log)))
      (let lp ((pos (log-read-pos log)) (out '()))
        (if (< pos end)
            (lp (1+ pos) (cons (vector-ref buf (log-pos->idx log pos)) out))
            out)))))


;;;
;;; async queues
;;;

(define-record-type <async-queue>
  (make-aq mutex condvar q)
  async-queue?
  (mutex aq-mutex)
  (condvar aq-condvar)
  (q aq-q))

(set-record-type-printer!
 <async-queue>
 (lambda (aq port)
   (format port "<async-queue ~a ~a>" (object-address aq)
           (q-length (aq-q aq)))))

(define* (make-async-queue)
  (make-aq (make-mutex)
           (make-condition-variable)
           (make-q)))

(define* (async-queue-push! aq item)
  (with-mutex (aq-mutex aq)
    (enq! (aq-q aq) item)
    (signal-condition-variable (aq-condvar aq))))

(define* (async-queue->list aq)
  (with-mutex (aq-mutex aq)
    (list-copy (car (aq-q aq)))))

(define* (async-queue-pop! aq)
  (with-mutex (aq-mutex aq)
    (let lp ()
      (cond
       ((q-empty? (aq-q aq))
        (wait-condition-variable (aq-condvar aq) (aq-mutex aq))
        (lp))
       (else
        (q-pop! (aq-q aq)))))))


;;;
;;; backend
;;;

(define (bytes-free-on-fs filename)
  (let* ((p (open-pipe* "r" "df" "-B1" "--output=avail" filename))
         (l1 (read-line p))
         (l2 (read-line p))
         (l3 (read-line p)))
    (close-pipe p)
    (cond
     ((and (string? l1) (string? l2) (eof-object? l3)
           (equal? (string-trim-both l1) "Avail"))
      (string->number l2))
     (else
      (error "could not get free space for file system containing" filename)))))

(define (delete-directory-contents-recursively working-dir)
  (for-each (lambda (file)
              (delete-file-recursively (in-vicinity working-dir file)))
            (scandir working-dir
                     (lambda (file)
                       (and (string<> "." file)
                            (string<> ".." file))))))

;; 1GB minimum free space.
(define *mininum-free-space* #e1e9)

(define (scm-files-in-dir dir)
  (map (lambda (file)
         (in-vicinity dir file))
       (scandir dir
                (lambda (file)
                  (and (not (file-is-directory? (in-vicinity dir file)))
                       (string-suffix? ".scm" file))))))

(define (copy-header-comments port file)
  (call-with-input-file file
    (lambda (in)
      (let lp ()
        (let ((line (read-line in)))
          (unless (eof-object? line)
            (let ((trimmed (string-trim line)))
              (when (or (string-null? trimmed) (string-prefix? ";" trimmed))
                (display trimmed port)
                (newline port)
                (lp)))))))))

(define (process-update log host working-dir source-checkout target-checkout
                        remote-git-url branch)
  (when (< (bytes-free-on-fs working-dir) *mininum-free-space*)
    (delete-directory-contents-recursively working-dir)
    (when (< (bytes-free-on-fs working-dir) *mininum-free-space*)
      (error "not enough free space")))
  (chdir working-dir)
  (let* ((repo-dir (uri-encode remote-git-url))
         (repo+branch-dir (in-vicinity repo-dir (uri-encode branch))))
    (cond
     ((file-exists? repo-dir)
      (chdir repo-dir)
      (git-fetch))
     (else
      (git-clone remote-git-url repo-dir)
      (chdir repo-dir)))
    (git-reset #:ref (string-append "origin/" branch) #:mode 'hard)
    (unless (file-is-directory? "guix-potluck")
      (error "repo+branch has no guix-potluck dir" remote-git-url branch))
    (let* ((files (scm-files-in-dir "guix-potluck"))
           ;; This step safely loads and validates the potluck package
           ;; definitions.
           (packages (map load-potluck-package files))
           (source-dir (in-vicinity source-checkout repo+branch-dir))
           (target-dir (in-vicinity target-checkout
                                    (in-vicinity "gnu/packages/potluck"
                                                 repo+branch-dir))))
      ;; Clear source and target repo entries.
      (define (ensure-empty-dir filename)
        (when (file-exists? filename)
          (delete-file-recursively filename))
        (mkdir-p filename))
      (define (commit-dir dir)
        (with-directory-excursion dir
          (git-add ".")
          (git-commit #:message
                      (format #f "Update ~a branch ~a."
                              remote-git-url branch)
                      #:author-name "Guix potluck host"
                      #:author-email (string-append "host@" host))
          (git-push)))
      (ensure-empty-dir source-dir)
      (ensure-empty-dir target-dir)
      ;; Add potluck files to source repo.
      (for-each (lambda (file)
                  (copy-file file (in-vicinity source-dir (basename file))))
                files)
      (commit-dir source-dir)
      ;; Add transformed files to target repo.
      (for-each (lambda (file package)
                  (call-with-output-file
                      (in-vicinity target-dir (basename file))
                    (lambda (port)
                      (define module-name
                        `(gnu packages potluck
                              ,repo-dir
                              ,(uri-encode branch)
                              ,(substring (basename file) 0
                                          (- (string-length (basename file))
                                             (string-length ".scm")))))
                      ;; Preserve copyright notices if possible.
                      (copy-header-comments port file)
                      (lower-potluck-package-to-module port module-name
                                                       package))))
                files packages)
      (commit-dir target-dir)
      packages)))

(define (service-queue host working-dir source-checkout target-checkout
                       queue package-log debug-log)
  (let lp ()
    (match (async-queue-pop! queue)
      ((remote-git-url . branch)
       (sxml-log debug-log "Processing update request of "
                 `(tt ,remote-git-url) " branch " `(tt ,branch) ".")
       (catch #t
         (lambda ()
           (let ((packages (process-update debug-log host working-dir
                                           source-checkout target-checkout
                                           remote-git-url branch)))
             (for-each
              (lambda (package)
                (sxml-log package-log
                          "Added package "
                          `(tt ,(potluck-package-name package))
                          " version "
                          `(tt ,(potluck-package-version package))
                          " from package definition at "
                          `(tt ,remote-git-url) " branch " `(tt ,branch) "."))
              packages)
             (sxml-log debug-log "Success.")))
         (lambda (k . args)
           (sxml-log debug-log "Failure updating " `(tt ,remote-git-url)
                     " branch " `(tt ,branch) ": "
                     (call-with-output-string
                       (lambda (port)
                         (print-exception port #f k args))))))
       (lp)))))


;;;
;;; frontend
;;;

(define* (validate-public-uri str #:key (schemes '(http https)))
  (define (public-host? host)
    ;; There are other ways to spell "localhost" using raw IPv4 or IPv6
    ;; addresses; this is just a sanity check.
    (not (member host '("localhost" "127.0.0.1" "[::1]"))))
  (let ((uri (and (string? str) (string->uri str))))
    (unless (and uri
                 (memq (uri-scheme uri) schemes)
                 (not (uri-fragment uri))
                 (public-host? (uri-host uri)))
      (error "expected a public URI" str))))

(define (validate-branch-name str)
  (unless (git-check-ref-format str #:allow-onelevel? #t)
    (error "expected a valid git branch name" str)))

(define (enqueue-update params queue log)
  (let ((remote-git-url (hash-ref params "git-url"))
        (branch-name (hash-ref params "branch")))
    (validate-public-uri remote-git-url)
    (validate-branch-name branch-name)
    (sxml-log log "Enqueuing update request of "
              `(tt ,remote-git-url) " branch " `(tt ,branch-name) ".")
    (async-queue-push! queue (cons remote-git-url branch-name))))

(define (request-body-json request body)
  (cond
   ((string? body) (json-string->scm body))
   ((bytevector? body)
    (let* ((content-type (request-content-type request))
           (charset (or (assoc-ref (cdr content-type) "charset")
                        "utf-8")))
      (json-string->scm (bytevector->string body charset))))
   ((port? body) (json->scm body))
   (else (error "unexpected body" body))))

(define (queue-status queue)
  (match (async-queue->list queue)
    (() '(p "Queue empty."))
    (entries
     `(dl
       ,@(append-map (match-lambda
                       ((url . branch)
                        `((dt ,(basename url))
                          (dd "Repository " ,url ", " ,branch " branch."))))
                     entries)))))

(define (log-status log)
  (match (read-log-entries log)
    (()
     '(p "No log entries."))
    (entries
     `(dl
       ,@(append-map (match-lambda
                       (((secs . usecs) . sxml)
                        `((dt ,(strftime "%c" (gmtime secs)))
                          (dd ,sxml))))
                     entries)))))

;; FIXME: plumb host, etc through to here.
(define* (main-page queue package-log debug-log
                    #:key (host "guix-potluck.org")
                    (path-prefix "/git/")
                    (target-repo "/srv/git/target.git"))
  `(html
    (head
     (title "Guix Potluck -- A Decentralized Package Registry for Guix")
     (meta (@ (name "viewport")
              (content "width=device-width, initial-scale=1")))
     (style "
body {
  max-width: 600px;
  margin-left: auto;
  margin-right: auto;
  margin-top: 2em;
  margin-bottom: 3em;
  padding: 1em;
  line-height: 1.4;
  background: #ddd;
}
"))
    (body
     (h1 "Guix Potluck")
     (h2 "About")
     (p "This server hosts a registry of packages for
the " (a (@ (href "https://gnu.org/software/guix/")) "Guix package
manager") ".")
     (p "The name \"potluck\" refers to a kind of meal in which every
guest brings a dish.  The resulting assembly of plates is only very
loosely planned, if at all, and the dishes are of varying quality and
sometimes even duplicate each other.  Still, there is something lovely
about sharing home-cooked food with friends that makes these meals
very special.")
     (p "In the same way, the assembly of packages offered by this
potluck server is unplanned and completely up to what users choose to
provide.  Bring your dish to the feast by using the " (tt "guix
potluck") " command-line tool to create and upload your package.")
     (p "To use packages from this repository, clone the repo via:")
     (pre
      "git clone https://" ,host "/" ,(string-trim-both path-prefix #\/)
      "/" ,(basename target-repo))
     (p "That will give you a directory of packages that you can add
to the available set of packages Guix knows about by
adding " (tt "GUIX_PACKAGE_PATH=" (var "dir")) " to your environment,
or by passing " (tt "-L " (var "dir")) " to your " (tt "guix
package") " invocations.")
     (p "This will get easier in the future when Guix adds a
\"channel\" facility.  In any case, see \"Invoking guix potluck\" in
the Guix manual for full details.")
     (h2 "Recently Updated Packages")
     ,(log-status package-log)
     (h2 "Recent Log Entries")
     ,(log-status debug-log)
     (h2 "Queue Status")
     ,(queue-status queue))))

(define (handler request body queue package-log debug-log)
  (match (cons (request-method request)
               (split-and-decode-uri-path (uri-path (request-uri request))))
    (('GET)
     (values (build-response
              #:code 200
              #:headers `((content-type . (text/html (charset . "utf-8")))
                          (date . ,(current-date))))
             (lambda (port)
               (sxml->xml (main-page queue package-log debug-log) port))))
    (('POST "api" "enqueue-update")
     ;; An exception will cause error 500.
     (enqueue-update (request-body-json request body) queue debug-log)
     (values (build-response #:code 200)
             "ok"))
    (_
     (values (build-response #:code 404)
             ""))))

(define (host-potluck host local-port working-dir source-checkout
                      target-checkout)
  (let ((worker-thread #f)
        (queue (make-async-queue))
        (package-log (make-log))
        (debug-log (make-log)))
    (sxml-log debug-log "Potluck host started.")
    (dynamic-wind (lambda ()
                    (set! worker-thread
                      (make-thread
                       (service-queue host working-dir
                                      source-checkout target-checkout
                                      queue package-log debug-log))))
                  (lambda ()
                    (run-server
                     (lambda (request body)
                       (handler request body queue package-log debug-log))
                     ;; Always listen on localhost.
                     'http `(#:port ,local-port)))
                  (lambda ()
                    (cancel-thread worker-thread)))))
