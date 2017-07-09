(define-module (gurses colors)
  #:use-module (ncurses curses)
  #:use-module (ice-9 match))

(define colors
  (list (list 'normal COLOR_WHITE COLOR_BLACK)
        (list 'livery-title COLOR_MAGENTA COLOR_BLACK)
        (list 'strong COLOR_RED COLOR_BLACK)
        (list 'button COLOR_BLACK COLOR_GREEN)
        (list 'button-shadow COLOR_BLACK COLOR_BLACK)
        (list 'focused-button COLOR_CYAN COLOR_GREEN)))

(define-public (color-index-by-symbol color)
  (let loop ((i 0) (p colors))
    (if (null? p)
        (error "unknown color" color)
        (match (car p)
         ((color-symbol foreground background)
          (if (eq? color-symbol color)
              i
              (loop (1+ i) (cdr p))))))))

(define-public (register-color-palette!)
  (for-each (lambda (index entry)
              (match entry
               ((color-symbol foreground background)
                (init-pair! index foreground background))))
            (iota (length colors))
            colors))

(define-public (select-color! win color)
;  (color-set! win (color-index-by-symbol color))
1)
