(require racket/draw)

(define (print-pareja-draw pareja dc x y)
    (if (pair? pareja)
        (begin 
            (print-dato (car pareja) dc x y)
            (print-dato (cdr pareja) dc (+ x 30) y)
            )
        )
  )

(define (print-dato dato dc x y)
    (if (pair? dato)
        (begin
          (send dc draw-rectangle
                  x y    ; Top-left at (x, y), y pixels down from top-left
                  30 30) ; wide and high
          (print-pareja-draw dato dc x (+ 30 y))
          )
        (begin
          (send dc draw-rectangle
                  x y    ; Top-left at (x, y), y pixels down from top-left
                  30 30) ; wide and high
          (send dc draw-text (number->string dato) (+ x 5) (+ y 5))
          )
        )
  )

(define target (make-bitmap 200 200))
(define ddcc (new bitmap-dc% [bitmap target]))
(define (guarda cuadro) (send cuadro save-file "cuadro.png" 'png))

(print-pareja-draw (cons 1 (cons 2 3)) ddcc 0 0)
(guarda target)

;Pinta como cadena
(define (print-pareja pareja)
    (if (pair? pareja)
        (string-append "(" (print-pareja (car pareja)) " . "  (print-pareja (cdr pareja)) ")")
        (number->string pareja))
  )

