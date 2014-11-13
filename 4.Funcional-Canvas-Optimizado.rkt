(require racket/draw)
(require racket/gui/base)
(require racket/format)

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
      (cons (lambda (dc)
              (list
               (send dc draw-rectangle
                     x y       ; Top-left at (x, y), y pixels down from top-left
                     TAM TAM)  ; wide and high
               (send dc draw-line
                     (+ x (/ TAM 2)) (+ y TAM)
                     (+ x (/ TAM 2)) (+ MARGEN y))
               )
              ) (crea-lista-draw dato x (+ MARGEN y)))
      (crea-lista-draw dato x y)
      )
  )

(define (funcion-cdr dato x y)
  (if (pair? dato) 
      (cons (lambda (dc)
              (list
               (send dc draw-rectangle
                     x y       ; Top-left at (x, y), y pixels down from top-left
                     TAM TAM)  ; wide and high
               (send dc draw-line
                     (+ x TAM ) (+ y (/ TAM 2))
                     (+ x MARGEN) (+ y (/ TAM 2)))
               )
              ) (crea-lista-draw dato(+ MARGEN x) y))
      (crea-lista-draw dato x y)
      )
  )

(define (funcion-dato dato x y)
  (list (lambda (dc)
          (list
           (send dc draw-rectangle
                 x y    ; Top-left at (x, y), y pixels down from top-left
                 TAM TAM) ; wide and high
           (send dc draw-text (~a dato) (+ x PADDING) (+ y PADDING))
           )
          )
        )
  )

;Evaluador de la lista de funciones
(define (evaluador sentencias)
  (map (lambda(x) x) sentencias)
  )

(define (pinta-lista lista dc)
  (map (lambda(x) (evaluador (x dc))) lista)
  )

;Sentencias canvas 
(define frame (new frame%
                   [label "Box and Pointer"]
                   [width 300]
                   [height 300]))

(define lista (cons (cons (cons "hola" 1) (cons #t (cons 'a 3))) (cons 2 (cons '() #\A))))
(define listaFunc (crea-lista-draw lista 0 0))

(new canvas% [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (pinta-lista listaFunc dc))])
(send frame show #t)

