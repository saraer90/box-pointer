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
              (cons
               (cons x y)
               (list
                (send dc draw-rectangle
                      x y       ; Top-left at (x, y), y pixels down from top-left
                      TAM TAM)  ; wide and high
                (send dc draw-line
                      (+ x (/ TAM 2)) (+ y TAM)
                      (+ x (/ TAM 2)) (+ MARGEN y))
                )
               )
             )(crea-lista-draw dato x (+ MARGEN y)))
      (crea-lista-draw dato x y)
      )
  )

(define (funcion-cdr dato x y)
  (if (pair? dato) 
      (cons (lambda (dc)
              (cons
               (cons x y)
               (list
                (send dc draw-rectangle
                      x y       ; Top-left at (x, y), y pixels down from top-left
                      TAM TAM)  ; wide and high
                (send dc draw-line
                      (+ x TAM ) (+ y (/ TAM 2))
                      (+ x MARGEN) (+ y (/ TAM 2)))
                )
               )
             )(crea-lista-draw dato(+ MARGEN x) y))
      (crea-lista-draw dato x y)
      )
  )

(define (funcion-dato dato x y)
  (list (lambda (dc)
          (cons 
           (cons x y)
           (list
            (send dc draw-rectangle
                 x y    ; Top-left at (x, y), y pixels down from top-left
                 TAM TAM) ; wide and high
            (send dc draw-text (~a dato) (+ x PADDING) (+ y PADDING))
           )
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
       
;Multiple canvas
(define (pinta-lista-multicanvas lista)
  (map 
   (lambda(x) 
    (new my-canvas% [parent frame]
     [style (list 'border)]
     [stretchable-width #t]
     [stretchable-height #t]
     [paint-callback
      (lambda (canvas dc)
        evaluador (x dc))])
    ) 
    lista)
 )

;Sentencias canvas 
(define frame-indiv (new frame%
                   [label "Box and Pointer - Indiv."]
                   [width 300]
                   [height 300]))

(define frame (new frame%
                   [label "Box and Pointer"]
                   [width 300]
                   [height 300]))

(define my-canvas%
  (class canvas%
    (super-new)
    (inherit get-dc)
    
    (define/override (on-event e)
           (if (equal? (send e get-event-type) 'left-down)
               (let ([my-dc (get-dc)])
                 (send my-dc set-background "black")
                 (send my-dc clear)
                 ;(send e get-x) (send e get-y)
                )
             )
     )   
   )
 )

(define lista (cons (cons (cons "hola" 1) (cons #t (cons 'a 3))) (cons 2 (cons '() #\A))))
(define listaFunc (crea-lista-draw lista 0 0))
(new my-canvas% [parent frame-indiv]
     [paint-callback
      (lambda (canvas dc)
        (pinta-lista listaFunc dc))]
)

(pinta-lista-multicanvas listaFunc)

(send frame show #t)
(send frame-indiv show #t)

(define c (new canvas% [parent frame]))