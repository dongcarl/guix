;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
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

(define-module (gnu machines)
  #:use-module (guix records)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (deployment
            make-deployment
            deployment?
            deployment-name
            deployment-machines

            machine
            make-machine
            machine?
            machine-name
            machine-system
            machine-platform

            platform
            make-platform
            platform-name
            platform-description
            platform-provision
            platform-install
            platform-reconfigure
            platform-boot
            platform-reboot
            platform-halt
            platform-destroy

            machine-os-for-platform
            provision-machine
            boot-machine

            local-vm))

(define-record-type* <deployment> deployment
  make-deployment
  deployment?
  (name deployment-name) ; string
  (machines deployment-machines)) ; list of <machine>

(define-record-type* <machine> machine
  make-machine
  machine?
  (name machine-name) ; string
  (system machine-system) ; <operating-system>
  (platform machine-platform)) ; <platform>

(define-record-type* <platform> platform
  make-platform
  platform?
  (name platform-name) ; string
  (description platform-description) ; string
  (transform platform-transform) ; procedure
  (provision platform-provision) ; procedure
  ;; (install platform-install) ; procedure
  ;; (reconfigure platform-reconfigure) ; procedure
  (boot platform-boot) ; procedure
  ;; (reboot platform-reboot) ; procedure
  ;; (halt platform-halt) ; procedure
  ;; (destroy platform-destroy) ; procedure
  )

(define (machine-os-for-platform machine)
  ((platform-transform (machine-platform machine)) (machine-system machine)))

(define (provision-machine machine)
  (let ((os (machine-os-for-platform machine)))
    ((platform-provision (machine-platform machine)) os)))

(define (boot-machine machine state)
  ((platform-boot (machine-platform machine)) state))

(use-modules (guix monads)
             (guix derivations)
             (guix store)
             (gnu services networking))

(define* (local-vm #:key (ip-address "10.0.2.10")
                   (disk-image-size (* 32 (expt 2 20))))
  (platform
   (name "local-vm")
   (description "Local QEMU/KVM platform")
   (transform
    (lambda (os)
      (let ((os (operating-system (inherit os)
                  (services
                   (cons
                    (static-networking-service "eth0" ip-address
                                               #:name-servers '("10.0.2.3")
                                               #:gateway "10.0.2.2")
                    (operating-system-user-services os))))))
        (virtualized-operating-system os '()))))
   (provision
    (lambda (os)
      (mlet %store-monad
          ((vm-script (system-qemu-image/shared-store-script
                       os #:disk-image-size disk-image-size)))
        (mbegin %store-monad
          (built-derivations (list vm-script))
          (return (derivation-output-path
                   (assoc-ref (derivation-outputs vm-script) "out")))))))
   (boot
    (lambda (script)
      (match (primitive-fork)
        (0 (primitive-exit (system* script)))
        (pid #t))))))
