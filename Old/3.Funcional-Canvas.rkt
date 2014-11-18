(require racket/draw)
(require racket/gui/base)
(define TAM 30)
(define MARGEN 50)
(define PADDING 5)

;Creación de la lista de funciones
(define (crea-lista-draw pareja x y)
    (if (pair? pareja)
        (append (funcion-car (car pareja) x y) (funcion-cdr (cdr pareja) (+ x TAM) y))
        (funcion-dato pareja x y)
     )
)

;Creadores de funciones
(define (funcion-car dato x y)
  (if (pair? dato)     
      (append (list (lambda (dc)
                      (begin
                        (send dc draw-rectangle
                              x y       ; Top-left at (x, y), y pixels down from top-left
                              TAM TAM)  ; wide and high
                        (send dc draw-line
                              (+ x (/ TAM 2)) (+ y TAM)
                              (+ x (/ TAM 2)) (+ MARGEN y))
                        )
                      )) (crea-lista-draw dato x (+ MARGEN y)))
      (crea-lista-draw dato x y)
  )
)

(define (funcion-cdr dato x y)
  (if (pair? dato) 
      (append (list (lambda (dc)
                      (begin
                        (send dc draw-rectangle
                              x y       ; Top-left at (x, y), y pixels down from top-left
                              TAM TAM)  ; wide and high
                        (send dc draw-line
                              (+ x TAM ) (+ y (/ TAM 2))
                              (+ x MARGEN) (+ y (/ TAM 2))
                              )
                         )
                      )) (crea-lista-draw dato(+ MARGEN x) y))
      (crea-lista-draw dato x y)
   )
)
  
(define (funcion-dato dato x y)
  (list (lambda (dc)
          (begin
            (send dc draw-rectangle
                  x y    ; Top-left at (x, y), y pixels down from top-left
                  TAM TAM) ; wide and high
            (send dc draw-text (number->string dato) (+ x PADDING) (+ y PADDING))
            )
         )
    )
)
  
;Evaluador de la lista de funciones
(define (pinta-lista lista dc)
  (map (lambda(x) (x dc)) lista)
)

;Sentencias canvas 
(define frame (new frame%
                   [label "Box and Pointer"]
                   [width 300]
                   [height 300]))
(define drawing-context (send (new canvas% [parent frame]) get-dc))
(define listaFunc (crea-lista-draw (cons (cons (cons 1 1) (cons 2 (cons 2 3))) (cons 2 (cons 3 3))) 0 0))
;pintar solo un cuadrado
(define aPintar (caddr listaFunc))

(new canvas% [parent frame]
            [paint-callback
            (lambda (canvas dc)
               (pinta-lista listaFunc dc))])
(send frame show #t)

