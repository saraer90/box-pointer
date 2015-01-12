(#%require  racket/draw)
(#%require  racket/gui/base)
(#%require  racket/format)

(define TAM 30)
(define MARGEN (+ 20 TAM))
(define PADDING 5)

;Creación de la lista de funciones: visible siempre a true en el inicio y genera coordenadas
(define (draw list) (list-creator list (if (list? list)
                                           (cons 0 0)
                                           (cons (* MARGEN (car-levels list)) 0))))

(define (car-levels list)
  (count-car-levels list 0)
  )

(define (count-car-levels list levels)
  (if (and (pair? list) (not (list? list)))
      (count-car-levels (car list) (+ levels 1))
      levels
   )
)

(define (list-creator pair coord)
  (if (pair? pair)
      (cons (list-recursion pair coord 'car) 
            (list-recursion pair (cons (+ (car coord) TAM) (cdr coord)) 'cdr) 
            )
      )
  )

(define (list-recursion data coord type)
  (cons (function-creator data coord type #f #t)
        (if (pair? ((eval type) data))
            (list-creator ((eval type) data) (coord-locator data coord type))
            (function-creator data coord type #t #t)
        )     
  )
 )

(define (coord-locator data coord type)
  (if (pair? data)
      (let ((x (car coord))
            (y (cdr coord)))
        (if (list? data)
            (cond ((eq? type 'car) (cons x (+ MARGEN y)))
                  ((eq? type 'cdr) (cons (+ MARGEN x) y))
                  )
            (cond ((eq? type 'car) (cons (- x MARGEN) (+ MARGEN y)))
                  ((eq? type 'cdr) (cons (+ (/ MARGEN 2) x) (+ MARGEN y)))
                  )
            )
        )
      coord
      )
  )


;Creador de funciones
(define (function-creator pair coord tipo es-dato visible)
  (let ((x (car coord))
        (y (cdr coord)))
    (lambda (dc msg)
      (let ((dato ((eval tipo) pair)))
      (cond ((string=? msg "dato") pair)
            ((string=? msg "coord") coord)
            ((string=? msg "tipo") tipo)
            ((string=? msg "visible") visible)
            ((string=? msg "es-dato") es-dato)
            ((string=? msg "dibuja")
             (if visible
                 (if es-dato
                     (list (send dc draw-text (~a dato) (+ x PADDING) (+ y PADDING)))
                     (list 
                      (send dc draw-rectangle
                            x y       ; Top-left at (x, y), y pixels down from top-left
                            TAM TAM)  ; wide and high
                      (if (pair? dato)
                          (let ((child-coords (coord-locator pair coord tipo)))
                            (cond ((eq? tipo 'car)
                                   (send dc draw-line
                                         (+ x (/ TAM 2)) (+ y TAM)
                                         (+ (car child-coords) TAM) (cdr child-coords))
                                   )
                                  ((eq? tipo 'cdr)
                                     (if (list? dato)
                                         (send dc draw-line
                                               (+ x TAM ) (+ y (/ TAM 2))
                                               (+ x MARGEN) (+ y (/ TAM 2)))
                                         (send dc draw-line
                                               (+ x (/ TAM 2)) (+ y TAM)
                                               (+ (car child-coords) TAM) (cdr child-coords))
                                         )
                                     )
                                  )
                            )
                          )
                      )
                  )
                 ;Cuando no es visible mostramos el indicador para que se expanda
                 (list
                  (send dc draw-rectangle
                        x y       
                        TAM TAM)  
                  (send dc draw-text (~a "+") (+ x PADDING) (+ y PADDING))
                  )
             )
            )
      ))
    )
  )
 )

;Detector de colisión
(define (intersectan? coord mouse-click) 
  (let ((x-ini (car coord))
        (y-ini (cdr coord))
        (x-fin (+ (car coord) TAM))
        (y-fin (+ (cdr coord) TAM))
        (x-click (car mouse-click))
        (y-click (cdr mouse-click)))
    (and (<= x-ini x-click) (<= y-ini y-click) (>= x-fin x-click) (>= y-fin y-click))
    )
  )

(define (detecta-colision funciones mouse-click dc)
  (if (pair? (car funciones))
      (detector-colisiones funciones mouse-click dc)
      (let ((funcion (car funciones)))
        (if (intersectan? (funcion dc "coord") mouse-click)
            (if (pair? ((eval (funcion dc "tipo")) (funcion dc "dato")))
                (cons (function-creator (funcion dc "dato") (funcion dc "coord") (funcion dc "tipo") (funcion dc "es-dato") (not (funcion dc "visible")))
                      (cdr funciones))
                funciones
            )
            (cons (function-creator (funcion dc "dato") (funcion dc "coord") (funcion dc "tipo") (funcion dc "es-dato") (funcion dc "visible"))
                  (detector-colisiones (cdr funciones) mouse-click dc)
                  )
            )
        )
      )
  )

(define (detector-colisiones funciones mouse-click dc)
  (if (pair? funciones)
      (cons (detecta-colision (car funciones) mouse-click dc) (detecta-colision (cdr funciones) mouse-click dc))
      funciones
      )
  )


;Pinta una lista de funciones
(define (pinta funciones dc)
  (if (pair? (car funciones))
      (pinta-lista funciones)
      (if ((car funciones) dc "visible")
          (cons ((car funciones) dc "dibuja") (pinta-lista (cdr funciones) dc))
          ((car funciones) dc "dibuja")
          )
      )
  )

(define (pinta-lista funciones dc)
  (if (pair? funciones)
      (cons (pinta (car funciones) dc) (pinta (cdr funciones) dc))
      (funciones dc "dibuja")
      )
  )


;Sentencias canvas 
(define my-canvas%
  (class canvas%
    (super-new)
    
    (inherit get-dc)
    (inherit refresh-now)
    
    (init-field pairs)
    (field [lista-funciones (draw pairs)])
    
    (define/override (on-event e)
      (if (equal? (send e get-event-type) 'left-down)
          (let ([my-dc (get-dc)])
            (send my-dc clear)
            (set! lista-funciones (detector-colisiones lista-funciones (cons (send e get-x) (send e get-y)) my-dc))
            (refresh-now)
            )
          )
      )
    
    (define/override (on-paint)
      (pinta-lista lista-funciones (get-dc))
      )
    )
  )

(define lista-parejas (cons (cons (cons (list 1 2 3 4 5) 1) 2) (cons 1 2)))

(define frame (new frame%
                   [label "Box and Pointer"]
                   [width 300]
                   [height 300]))
(new my-canvas% [pairs lista-parejas] [parent frame])   
(send frame show #t)


;aux
(define (print-pareja pareja)
  (if (pair? pareja)
      (begin 
        (display "(")
        (print-dato (car pareja))
        (display " . ")
        (print-dato (cdr pareja))
        (display ")"))))

(define (print-dato dato)
  (if (pair? dato)
      (print-pareja dato)
      (display dato)))
