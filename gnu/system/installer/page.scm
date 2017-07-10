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

(define-module (gnu system installer page)
  #:export (make-page)
  #:export (page-surface)
  #:export (page-refresh)
  #:export (page-initialised?)
  #:export (page-set-initialised!)
  #:export (page-enter)
  #:export (page-leave)
  #:export (page-set-wwin!)
  #:export (page-wwin)
  #:export (page-cursor-visibility)
  #:export (page-title)
  #:export (page-datum)
  #:export (page-set-datum!)
  #:export (page-key-handler)
  #:export (page-mouse-handler)
  #:export (page-default-key-handler)
  #:export (page-default-mouse-handler)
  #:export (page-getch)
  #:export (page-focused-widget)
  #:export (page-set-focused-widget)
  #:export (refresh-screen)

  #:use-module (gurses buttons)
  #:use-module (gurses menu)
  #:use-module (gurses form)
  #:use-module (ncurses curses)
  #:use-module (ncurses panel)
  #:use-module (gnu system installer utils)
  #:use-module (gnu system installer levelled-stack)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define-record-type <page>
  (make-page' surface title inited refresher cursor-visibility key-handler mouse-handler data)
  page?
  (title page-title)
  (surface page-surface)
  (inited  page-initialised? page-set-initialised!)
  (refresher page-refresher)
  (cursor-visibility page-cursor-visibility)
  (key-handler page-key-handler)
  (mouse-handler page-mouse-handler)
  (wwin page-wwin page-set-wwin!)
  (data page-data page-set-data!))

(define (page-activate-item page info)
  ((page-datum page 'activator) page info))

(define (page-focused-widget page)
  (let* ((menu (page-datum page 'menu))
         (nav  (page-datum page 'navigation))
         (form (page-datum page 'form))
         (config-window (and=> (page-datum page 'config-window)
                               (cut inner <>))))
    (cond
     ((and menu (menu-active menu))
      menu)
     ((and form (form-enabled? form))
      form)
     ((and nav (buttons-selected-symbol nav))
      nav)
     ((and config-window (page-datum page 'config-window-focused))
      config-window)
     (else
      #f))))

(define* (page-set-focused-widget page widget)
  (let* ((menu (page-datum page 'menu))
         (nav  (page-datum page 'navigation))
         (form (page-datum page 'form))
         (config-window (and=> (page-datum page 'config-window)
                               (cut inner <>)))
         (widgets (filter (lambda (entry)
                            (match entry
                             ((widget focused? set-focused!)
                               widget)))
                          (list (list menu menu-active menu-set-active!)
                                (list form form-enabled? form-set-enabled!)
                                (list config-window
                                      (lambda (w)
                                        (page-datum page 'config-window-focused))
                                      (lambda (w value)
                                        (page-set-datum! page 'config-window-focused value)))
                                (list nav buttons-selected-symbol (lambda (buttons value)
                                                                    (buttons-select buttons
                                                                      (if value
                                                                          0
                                                                          -1))))))))
    ;; Unfocus all widgets but this one
    (for-each (lambda (entry)
                (match entry
                 ((xwidget focused? set-focused!)
                  (set-focused! xwidget (eq? widget xwidget)))))
              widgets)
    widget))

(define* (page-focus-widget-relative page direction #:key (buttons? #f) (wrap? #f))
  (define (focused-widget-cons widgets)
    (if (null? widgets)
        '()
        (match (car widgets)
         ((xwidget focused? set-focused!)
          (if (focused? xwidget)
              widgets
              (focused-widget-cons (cdr widgets)))))))
  (let* ((menu (page-datum page 'menu))
         (nav  (page-datum page 'navigation))
         (form (page-datum page 'form))
         (config-window (and=> (page-datum page 'config-window) (cut inner <>)))
         (widgets (filter (lambda (entry)
                            (match entry
                             ((widget focused? set-focused!)
                               widget)))
                          (list (list menu menu-active menu-set-active!)
                                (list form form-enabled? form-set-enabled!)
                                (list config-window
                                      (lambda (w)
                                        (page-datum page 'config-window-focused))
                                      (lambda (w value)
                                        (page-set-datum! page 'config-window-focused value)))
                                (list nav buttons-selected-symbol
                                      (lambda (buttons value)
                                        (let ((index (buttons-selected buttons)))
                                          (buttons-unselect-all buttons)
                                          (if value
                                            (buttons-select buttons
                                                            (if (= index -1)
                                                                0
                                                                index)))))))))
         (c (focused-widget-cons widgets))
         (n (if (null? c) '() (cdr c)))
         (next-widget-entry (if (null? n)
                                (if wrap?
                                    (if (null? widgets)
                                        #f
                                        (car widgets))
                                    #f)
                                (car n))))
    (if (null? c)
        #f
        (match (car c)
         ((ywidget yfocused? yset-focused!)
          (match direction
           ('next
            (if (and buttons? nav (eq? ywidget nav)
                     (not (eqv? (buttons-selected nav)
                                (1- (buttons-n-buttons nav))))) ; last button
                (begin
                  (buttons-select-next nav)
                  nav)
                (begin
                  (match next-widget-entry
                   ((xwidget xfocused? xset-focused!)
                    (yset-focused! ywidget #f)
                    (xset-focused! xwidget #t)
                    xwidget)
                   (_ #f)))))
           ('prev
            (if (and buttons? nav (eq? ywidget nav)
                     (not (eqv? (buttons-selected nav)
                                0))) ; first button
                (begin
                  (buttons-select-prev nav)
                  nav)
                (begin
                  (let loop ((p widgets))
                    (cond
                     ((null? p) #f) ; TODO wrap.
                     ((eq? (cdr p) c) ; p in front of current
                      (let ((prev-widget-entry (car p)))
                        (match prev-widget-entry
                         ((xwidget xfocused? xset-focused!)
                          (yset-focused! ywidget #f)
                          (xset-focused! xwidget #t)
                          xwidget))))
                     (else
                      (loop (cdr p))))))))))))))

(define (page-default-mouse-handler page device-id x y z button-state)
  (let* ((menu (page-datum page 'menu))
         (buttons (page-datum page 'navigation))
         (form (page-datum page 'form))
         (status (or (let ((status (and menu (std-menu-mouse-handler menu device-id x y z button-state))))
                       (match status
                        (('menu-item-activated x)
                         (page-set-focused-widget page menu)
                         (list 'menu-item-activated x))
                        (('menu-item-selected x)
                         (page-set-focused-widget page menu)
                         (list 'menu-item-selected x))
                        (_ #f)))
                     (if buttons
                       (match (buttons-mouse-handler buttons device-id x y z button-state)
                        (#f #f)
                        ('ignored #f)
                        (x
                         (page-set-focused-widget page buttons)
                         ;(display x)
                         ;(if menu
                         ;  (menu-set-active! menu #f))
                         x))))))
    (if form
      (std-form-mouse-handler form device-id x y z button-state))
    (if status
        (begin
          (page-activate-item page status)
          'handled)
        'ignored)))

(define (page-default-key-handler page ch)
  "Handle keypresses in a commonly-used page.
The page is assumed to have only at most a menu, a form and a navigation.
If a form is used it's assumed that the menu is not used and vice versa."
  (let* ((menu (page-datum page 'menu))
         (nav  (page-datum page 'navigation))
         (form (page-datum page 'form)))
    (cond
     ((and form (form-enabled? form) (not (eq? 'ignored (form-enter form ch))))
     'handled)

     ((and menu (menu-active menu) (std-menu-key-handler menu ch))
      'handled)

     ((eq? ch KEY_RIGHT)
      (page-focus-widget-relative page 'next #:buttons? #t)
      'handled)

     ((eq? ch KEY_LEFT)
      (if (and nav (eq? nav (page-focused-widget page)))
          ;; Don't go to other widgets.
          (buttons-select-prev nav)
          (page-focus-widget-relative page 'prev #:buttons? #t))
      'handled)

     ((eq? ch #\tab)
      (page-focus-widget-relative page 'next #:buttons? #t #:wrap? #t)
      'handled)

     ((select-key? ch)
      (page-activate-item page
                          (if (and menu (menu-active menu))
                            (list 'menu-item-activated
                                  (menu-get-current-item menu))
                            (if (and form (form-enabled? form))
                              'default
                              (if nav
                                (buttons-selected-symbol nav)
                                'default)))))

     ((eq? ch KEY_UP)
      (page-focus-widget-relative page 'prev #:buttons? #f)
      'handled)

     ((eq? ch KEY_DOWN)
      (page-focus-widget-relative page 'next #:buttons? #f)
      'handled)

     ((and nav (char? ch)
               (or (buttons-fetch-by-key nav (char-upcase ch))
                   (buttons-fetch-by-key nav (char-downcase ch))))
      (let ((button (or (buttons-fetch-by-key nav (char-upcase ch))
                        (buttons-fetch-by-key nav (char-downcase ch)))))
        ;(if menu
        ;  (menu-set-active! menu #f)
        ;  (if form
        ;    (form-set-enabled! form #f)))
        ;(buttons-select-by-symbol nav button)
        (page-activate-item page button)))

     (else
       'ignored))))

(define* (make-page s title refresh cursor-visibility
                    #:optional
                    (key-handler page-default-key-handler)
                    (mouse-handler page-default-mouse-handler)
                    #:key
                    activator
                    (height-subtraction 0))
  (let* ((frame (make-boxed-window s
                                 (- (getmaxy s) height-subtraction) (- (getmaxx s) 0)
                                 0 0
                                 #:title title))
         (xsurface  (inner frame)))
    (let* ((result (make-page' xsurface title #f refresh cursor-visibility key-handler mouse-handler '())))
      (page-set-wwin! result frame)
      (page-set-datum! result 'config-window-focused #f)
      (if activator
        (page-set-datum! result 'activator activator))
      result)))

(define (page-set-datum! page key value)
  (page-set-data! page (acons key value (page-data page))))

(define (page-datum page key)
  (assq-ref (page-data page) key))

(define (page-leave)
  (pop-cursor)
  (let* ((frame (page-wwin (page-top)))
         (window (outer frame)))
    (hide-panel window)
    ;(hide-panel (page-surface (page-top)))
    (deep-visit-windows delwin window)
    ;(redrawwin stdscr)
    (update-panels)
    (doupdate)
    (page-pop)
    ;(deep-visit-windows touchwin (outer (page-wwin (page-top)))) ; FIXME
    ))

(define (page-refresh p)
  (let ((focused-window (or (page-focused-window p) (page-surface p))))
    (match (getyx focused-window)
     ((y x)
      (boxed-window-decoration-refresh (page-wwin p) (page-title p))
      (erase (page-surface p))
      ((page-refresher p) p)
      (let ((form (page-datum p 'form))
            (buttons (page-datum p 'navigation))
            (menu (page-datum p 'menu))
            (config-window (page-datum p 'config-window))
            (config-window-port (page-datum p 'config-window-port))
            (config-window-title (page-datum p 'config-window-title)))
        (if menu
          (begin
            (menu-redraw menu)
            (menu-refresh menu)))
        (if buttons
          (buttons-refresh buttons))
        (if form
          (form-refresh form))
        (if config-window
          (begin
            (boxed-window-decoration-refresh config-window
                                             (or config-window-title
                                                 "Output"))
            (erase (inner config-window))
            ; TODO scrolling...
            (if config-window-port
                (addstr (inner config-window) (get-output-string config-window-port)))))
        (move focused-window y x))))))

(define (page-enter p)
  (page-push p)
  ;(erase (page-surface p))
  (page-refresh p))

(define (page-focused-window p)
  (let* ((menu (page-datum p 'menu))
         (form (page-datum p 'form))
         (buttons (page-datum p 'navigation)))
    (cond
     ((and menu (menu-active menu))
      (menu-window menu))
     ((and form (form-enabled? form))
      (form-window form))
     (buttons
      (buttons-window buttons))
     (else ; shouldn't happen.
      #f))))

(define (page-getch p)
  (let ((window (page-focused-window p)))
    (keypad! window #t)
    (timeout! window 500) ; ms
    (getch window)))

(define (refresh-screen)
  (let* ((page (page-top))
         (window (page-focused-window page)))
    (match (getyx window)
     ((y x)
      (update-panels)
      (doupdate)
      (move window y x)))))
