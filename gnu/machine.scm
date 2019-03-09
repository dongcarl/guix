(define-module (gnu machine)
  #:use-module ((gnu packages package-management) #:select (guix))
  #:use-module (gnu system)
  #:use-module (guix derivations)
  #:use-module (guix inferior)
  #:use-module (guix packages)
  #:use-module (guix ssh)
  #:use-module (guix store)
  #:use-module (oop goops)
  #:use-module (ssh session)
  #:export (<machine>
            system
            display-name
            build-os
            deploy-os
            remote-eval

            <sshable-machine>
            host-name
            ssh-port
            ssh-user))

(define-class <machine> ()
  (system #:getter system #:init-keyword #:system))

(define-method (display-name (machine <machine>))
  (operating-system-host-name (system machine)))

(define-method (build-os (machine <machine>) store)
  (let* ((guixdrv (run-with-store store (package->derivation guix)))
         (guixdir (and (build-derivations store (list guixdrv))
                       (derivation->output-path guixdrv)))
         (osdrv (run-with-store store (operating-system-derivation
                                       (system machine)))))
    (and (build-derivations store (list osdrv))
         (list (derivation-file-name osdrv)
               (derivation->output-path osdrv)))))

(define-method (deploy-os (machine <machine>) store osdrv)
  (error "not implemented"))

(define-method (remote-eval (machine <machine>) exp)
  (error "not implemented"))

(define-class <sshable-machine> (<machine>)
  (host-name #:getter host-name #:init-keyword #:host-name)
  (ssh-port #:getter ssh-port #:init-keyword #:ssh-port #:init-form 22)
  (ssh-user #:getter ssh-user #:init-keyword #:ssh-user #:init-form "root")
  ;; ??? - SSH key config?
  )

(define-method (deploy-os (machine <sshable-machine>) store osdrvs)
  (let ((session (open-ssh-session (host-name machine)
                                   #:user (ssh-user machine)
                                   #:port (ssh-port machine))))
    (with-store store (send-files store osdrvs
                                  (connect-to-remote-daemon session)
                                  #:recursive? #t))
    #t))
