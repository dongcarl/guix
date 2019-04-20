;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Caleb Ristvedt <caleb.ristvedt@cune.org>
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

;;; For building derivations.

(define-module (guix store build-derivations)
  #:use-module (guix derivations)
  #:use-module (guix store)
  #:use-module (guix store database)
  #:use-module (guix config)
  #:use-module (guix build syscalls)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 popen)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-11)
  #:use-module (gcrypt hash)
  #:use-module (guix serialization)
  #:use-module (guix base16)
  #:use-module (guix sets)
  #:use-module ((guix build utils) #:select (delete-file-recursively
                                             mkdir-p
                                             copy-recursively))
  #:use-module (guix build store-copy)
  #:use-module (gnu system file-systems)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (guix store environment)
  #:export (build-derivation))

(define (output-paths drv)
  "Return all store output paths produced by DRV."
  (match (derivation-outputs drv)
    (((outid . ($ <derivation-output> output-path)) ...)
     output-path)))

(define (get-output-specs drv possible-references)
  "Return a list of <store-info> objects, one for each output of DRV."
  (map (match-lambda
         ((outid . ($ <derivation-output> output-path))
          (let ((references
                 (scan-for-references output-path
                                      ;; outputs can reference
                                      ;; themselves or other outputs of
                                      ;; the same derivation.
                                      (append (output-paths drv)
                                              possible-references))))
            (store-info output-path (derivation-file-name drv) references))))
       (derivation-outputs drv)))

(define (builtin-download drv outputs)
  "Download DRV outputs OUTPUTS into the store."
  (setenv "NIX_STORE" %store-directory)
  ;; XXX: Set _NIX_OPTIONS once client settings are known
  (execl (string-append %libexecdir "/download")
         "download"
         (derivation-file-name drv)
         ;; We assume this has only a single output
         (derivation-output-path (cdr (first outputs)))))

;; if a derivation builder name is in here, it is a builtin. For normal
;; behavior, make sure everything starts with "builtin:". Also, the procedures
;; stored in here should take two arguments, the derivation and the list of
;; (output-name . <derivation-output>)s to be built.

(define builtins
  (let ((builtins-table (make-hash-table 10)))
    (hash-set! builtins-table
               "builtin:download"
               builtin-download)
    builtins-table))

(define %keep-build-dir? #t)

;; XXX: make this configurable. Maybe I should read some more about those
;; parameters I've heard about...
(define %build-group (false-if-exception (group:gid (getgrnam "guixbuild"))))
(define %build-user-pool (and %build-group
                              (group:mem (getgrgid %build-group))))


(define (get-build-user)
  (let ((user (getuid)))
    (or (and (zero? user)
             %build-user-pool
             ;; XXX: When implementing
             ;; scheduling, make it so this
             ;; searches for an unused
             ;; one.
             (passwd:uid
              (getpwnam
               (last %build-user-pool))))
        user)))

(define (get-build-group)
  (or (and (zero? (getuid)) %build-group)
      (getgid)))

(define-record-type <trie-node>
  (make-trie-node table string-exists?)
  trie-node?
  ;; TODO implement skip values. Probably not as big a speed gain as you think
  ;; it is, since this is I/O-bound.
  ;; (skip-value node-skip-value set-skip-value!)
  (table node-table set-node-table!)
  ;; Technically speaking, it's possible for both CAT and CATTLE to be in a
  ;; trie at once. Of course, for our purposes, this is 
  (string-exists? node-string-exists? set-string-exists?!))

(define* (add-to-trie trie string #:optional (new-tables-size 2))
  "Adds STR to TRIE."
  (let ((str (string->utf8 string)))
    (let next-node ((position 0)
                    (current-node trie))
      (if (= position (bytevector-length str))
          ;; this is it. This is where we need to register that this string is
          ;; present.
          (set-string-exists?! current-node #t)
          (let* ((current-table (node-table current-node))
                 (node (hash-ref current-table
                                 (bytevector-u8-ref str position))))
            (if node
                (next-node (1+ position)
                           node)
                (let ((new-node (make-trie-node (make-hash-table new-tables-size)
                                                #f)))
                  (hash-set! current-table
                             (bytevector-u8-ref str position)
                             new-node)
                  (next-node (1+ position)
                             new-node))))))))

(define (make-search-trie strings)
  ;; TODO: make the first few trie levels non-sparse tables to avoid hashing
  ;; overhead. 
  (let ((root (make-trie-node (make-hash-table) #f)))
    (for-each (cut add-to-trie root <>)
              strings)
    root))


(define (remove-from-trie! trie sequence)
  "Removes SEQUENCE from TRIE. This means that any nodes that are only in the
path of SEQUENCE are removed. It's an error to use this with a sequence not
already in TRIE."
  ;; Hm. Looks like we'll have to recurse all the way down, find where it
  ;; ends, then stop at the first thing on the way back up that has anything
  ;; with the same prefix. Or I could do this the right way with an explicit
  ;; stack. Hm...
  
  (define (node-stack)
    (let next ((nodes '())
               (i 0)
               (current-node trie))
      (if (= (bytevector-length sequence) i)
          (begin
            ;; it's possible that even though this is the last node of this
            ;; sequence it can't be deleted. So mark it as not denoting a
            ;; string.
            (set-string-exists?! current-node #f)
            (cons current-node nodes))
          (let ((next-node (hash-ref (node-table current-node)
                                     (bytevector-u8-ref sequence i))))
            (next (cons current-node nodes)
                  (1+ i)
                  next-node)))))

  (let maybe-delete ((visited-nodes (node-stack))
                     (i (1- (bytevector-length sequence))))
    (match visited-nodes
      ((current parent others ...)
       (when (zero? (hash-count (const #t)
                                (node-table current)))

         (hash-remove! (node-table parent)
                       (bytevector-u8-ref sequence i))
         (maybe-delete (cdr visited-nodes)
                       (1- i))))
      ((current)
       #f))))

(define (scanning-wrapper-port output-port paths)
  "Creates a wrapper port which passes through bytes to OUTPUT-PORT and
returns it as well as a procedure which, when called, returns a list of all
references out of the possibilities enumerated in PATHS that were
detected. PATHS must not be empty."
  ;; Not sure if I should be using custom ports or soft ports...
  (let* ((strings (map store-path-hash-part paths))
         (string->path (fold (lambda (current prev)
                               (vhash-cons (store-path-hash-part current)
                                           current
                                           prev))
                             vlist-null
                             paths))
         (lookback-size (apply max (map (compose bytevector-length string->utf8)
                                        strings)))
         (smallest-length (apply min (map (compose bytevector-length
                                                   string->utf8)
                                          strings)))
         (lookback-buffer (make-bytevector lookback-size))
         (search-trie (make-search-trie strings))
         (buffer-pos 0)
         (references '()))
    
    (values
     (make-custom-binary-output-port
      "scanning-wrapper"
      ;; write
      (lambda (bytes offset count)
        (define (in-lookback? n)
          (< n buffer-pos))
        ;; the "virtual" stuff provides a convenient interface that makes it
        ;; look like we magically remember the end of the previous buffer.
        (define (virtual-ref n)
          (if (in-lookback? n)
              (bytevector-u8-ref lookback-buffer n)
              (bytevector-u8-ref bytes (+ (- n buffer-pos)
                                          offset))))
        

        (let ((total-length (+ buffer-pos count)))
          
          (define (virtual-copy! start end target)
            (let* ((copy-size (- end start)))
              (let copy-next ((i 0))
                (unless (= i copy-size)
                  (bytevector-u8-set! target
                                      i
                                      (virtual-ref (+ start i)))
                  (copy-next (1+ i))))
              target))

          ;; the gritty reality of that magic
          (define (remember-end)
            (let* ((copy-amount (min total-length
                                     lookback-size))
                   (start (- total-length copy-amount))
                   (end total-length))
              (virtual-copy! start end lookback-buffer)
              (set! buffer-pos copy-amount)))
          
          (define (attempt-match n trie)
            (let test-position ((i n)
                                (current-node trie))
              (if (node-string-exists? current-node)
                  ;; MATCH
                  (virtual-copy! n i (make-bytevector (- i n)))
                  (if (>= i total-length)
                      #f
                      (let ((next-node (hash-ref (node-table current-node)
                                                 (virtual-ref i))))
                        (if next-node
                            (test-position (1+ i)
                                           next-node)
                            #f))))))


                   
          (define (scan)
            (let next-char ((i 0))
              (when (< i (- total-length smallest-length))
                (let ((match-result (attempt-match i search-trie)))
                  (if match-result
                      (begin
                        (set! references
                          (let ((str-result
                                 (cdr (vhash-assoc (utf8->string match-result)
                                                   string->path))))
                            (format #t "Found reference to: ~a~%" str-result)
                            (cons str-result
                                  references)))
                        ;; We're not interested in multiple references, it'd
                        ;; just slow us down.
                        (remove-from-trie! search-trie match-result)
                        (next-char (+ i (bytevector-length match-result))))
                      (next-char (1+ i)))))))
          (format #t "Scanning chunk of ~a bytes~%" count)
          (scan)
          (remember-end)
          (put-bytevector output-port bytes offset count)
          count))
      #f ;; get-position
      #f ;; set-position
      (lambda ()
        (close-port output-port)))
     (lambda ()
       references))))


;; There are two main approaches we can use here: we can look for the entire
;; store path of the form "/gnu/store/hashpart-name", which will yield no
;; false positives and likely be faster due to being more quickly able to rule
;; out sequences, and we can look for just hashpart, which will be faster to
;; lookup and may both increase false positives and decrease false negatives
;; as stuff that gets split up will likely still have the hash part all
;; together, but adds a chance that 32 random base-32 characters could cause a
;; false positive, but the chances of that are extremely slim, and an
;; adversary couldn't really use that.
(define (scan-for-references file possibilities)
  "Scans for literal references in FILE as long as they happen to be in
POSSIBILITIES. Returns the list of references found, the sha256 hash of the
nar, and the length of the nar."
  (let*-values (((scanning-port get-references)
                 (scanning-wrapper-port (%make-void-port "w") possibilities)))
    (write-file file scanning-port)
    (force-output scanning-port)
    (get-references)))

(define (copy-outputs drv environment)
  "Copy output paths produced in ENVIRONMENT from building DRV to the store if
a fake store was used."
  (let ((store-dir (assoc-ref (environment-temp-dirs environment)
                              'store-directory)))
    (when store-dir
      (for-each
       (match-lambda
         ((outid . ($ <derivation-output> output-path))
          (copy-recursively
           (string-append store-dir "/" (basename output-path)) output-path)))
       (derivation-outputs drv)))))

(define (topologically-sorted store-infos)
  "Returns STORE-INFOS in topological order or throws CYCLE-DETECTED if no
such order exists."
  (define path->store-info
    (let loop ((infos store-infos)
               (mapping vlist-null))
      (match infos
        ((($ (@@ (guix build store-copy) <store-info>)
             item deriver references) . tail)
         (loop tail (vhash-cons item (car infos) mapping)))
        (()
         (lambda (path)
           (let ((pair (vhash-assoc path mapping)))
             (and pair
                  (cdr pair))))))))

  (define (references-of store-info)
    ;; We need to pretend that self-references don't exist...
    (fold (lambda (current prev)
            (let ((info (path->store-info current)))
              (or (and (not (equal? info store-info))
                       info
                       (cons info prev))
                  prev)))
          '()
          (store-info-references store-info)))

  (reverse
   (let visit ((infos store-infos)
               (visited (set))
               (dependents (set))
               (result '()))
     (match infos
       ((head . tail)
        (if (set-contains? visited head)
            (if (set-contains? dependents head)
                (throw 'cycle-detected head)
                (visit tail visited dependents result))
            (call-with-values
                (lambda ()
                  (visit (references-of head)
                         (set-insert head visited)
                         (set-insert head dependents)
                         result))
              (lambda (result visited)
                (visit tail
                       visited
                       dependents
                       (cons head result))))))
       (()
        (values result visited))))))

(define (run-builder builder drv environment store-inputs)
  "Run the builder BUILDER for DRV in ENVIRONMENT, wait for it to finish, and
return the list of <store-info>s corresponding to its outputs."
  (match (status:exit-val (call-with-values
                              (lambda ()
                                (run-standard environment builder))
                            wait-for-build))
    (0
     ;; XXX: check that the output paths were produced.
     (copy-outputs drv environment)
     (delete-environment environment)
     (get-output-specs drv store-inputs))
    (exit-value
     (format #t "Builder exited with status ~A~%" exit-value)
     (if %keep-build-dir?
         (format #t "Note: keeping build directories: ~A~%"
                 (match (environment-temp-dirs environment)
                   (((sym . dir) ...)
                    dir)))
         (delete-environment environment))
     #f)))

(define* (builder+environment+inputs drv #:optional (chroot? #t))
  "Return a thunk that performs the build action, the environment it should be
run in, and the store inputs of that environment."
  (let*-values (((builtin) (hash-ref builtins (derivation-builder drv)))
                ((environment store-inputs)
                 ((if builtin
                      builtin-builder-environment
                      (if chroot?
                          chroot-build-environment
                          nonchroot-build-environment))
                  drv #:gid (get-build-group) #:uid (get-build-user)))
                ((builder) (or
                            (and builtin (lambda ()
                                           (builtin drv (derivation-outputs
                                                         drv))))
                            (lambda ()
                              (let ((prog (derivation-builder drv))
                                    (args (derivation-builder-arguments drv)))
                                (apply execl prog prog args))))))
    (values builder environment store-inputs)))

;; Note: used for testing mostly, daemon should be starting builds directly
;; and not just waiting for them to finish sequentially...
(define (%build-derivation drv) 
  "Given a <derivation> DRV, build the derivation unconditionally even if its
outputs already exist."
  ;; Make sure store permissions and ownership are intact (test-env creates a
  ;; store with wrong permissions, for example).
  (when (and (zero? (getuid)) %build-group)
    (chown %store-directory 0 %build-group)
    (chmod %store-directory #o1775))
  ;; Inputs need to exist regardless of how we're getting the outputs of this
  ;; derivation.
  (ensure-input-outputs-exist (derivation-inputs drv))
  (format #t "Starting build of derivation ~a~%~%" drv)
  (let*-values (((builder environment store-inputs)
                 (builder+environment+inputs drv (zero? (getuid))))
                ((output-specs)
                 (or (attempt-substitute drv)
                     (run-builder builder drv environment store-inputs))))
    (if output-specs
        (register-items (topologically-sorted output-specs))
        (throw 'derivation-build-failed drv))))

(define (ensure-input-outputs-exist inputs)
  "Call %build-derivation as necessary, recursively, to make the necessary
outputs of INPUTS exist."
  (for-each
   (lambda (input)
     (let ((input-drv-path (derivation-input-path input)))
       (unless (outputs-exist? input-drv-path
                               (derivation-input-sub-derivations input))
         (%build-derivation (read-derivation-from-file input-drv-path)))))
   inputs))

(define* (build-derivation drv #:optional (outputs (derivation-output-names drv)))
  "Given a <derivation> DRV with desired outputs OUTPUTS, build DRV if the
outputs don't already exist."
  (unless (outputs-exist? (derivation-file-name drv)
                          outputs)
    (%build-derivation drv)))



