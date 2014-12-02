(require racket/draw)
(require racket/gui/base)

(define (print-pareja-draw pareja dc x y)
    (if (pair? pareja)
        (begin 
            (print-dato (car pareja) dc x y #t)
            (print-dato (cdr pareja) dc (+ x 30) y #f)
            )
        )
  )

(define (print-dato dato dc x y isCar)
  (cond
    ((and (pair? dato) isCar)
        (begin
          (send dc draw-rectangle
                  x y    ; Top-left at (x, y), y pixels down from top-left
                  30 30) ; wide and high
          (send dc draw-line
                  (+ x 15) (+ y 30)
                  (+ x 15) (+ 50 y))
          (print-pareja-draw dato dc x (+ 50 y))
          ))
    ((and (pair? dato))
     (begin
          (send dc draw-rectangle
                  x y    ; Top-left at (x, y), y pixels down from top-left
                  30 30) ; wide and high
          (send dc draw-line
                  (+ x 30) (+ y 15)
                  (+ x 50) (+ 15 y))
          (print-pareja-draw dato dc (+ 50 x) y)
          ))
    (else 
        (begin
          (send dc draw-rectangle
                  x y    ; Top-left at (x, y), y pixels down from top-left
                  30 30) ; wide and high
          (send dc draw-text (number->string dato) (+ x 5) (+ y 5))
          )
        )
    )
  )

;(define target (make-bitmap 300 300))
;(define drawing-context (new bitmap-dc% [bitmap target]))
;(define (guarda cuadro) (send cuadro save-file "cuadro.png" 'png))
;(guarda target)

(define frame (new frame%
                   [label "Box and Pointer"]
                   [width 300]
                   [height 300]))
(define drawing-context (send (new canvas% [parent frame]) get-dc))
(define aPintar '(print-pareja-draw (cons (cons (cons 1 1) (cons 2 (cons 2 3))) (cons 2 (cons 3 3))) drawing-context 0 0))


(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (eval aPintar))])
(send frame show #t)
