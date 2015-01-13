(#%require  racket/draw)
(#%require  racket/gui/base)
(#%require  racket/format)
(#%require compatibility/mlist)

(require "lib/utils.rkt")
(require "lib/settings.rkt")

;Creación de la lista de funciones: visible siempre a true en el inicio y genera coordenadas

;Preparar margen izquierdo 
(define (draw list) (list-creator list (if (mlist? list)
                                           (cons 0 0)
                                           (cons (* MARGEN (car-levels list)) 0))))
;Según los niveles de Car en modo arbol
(define (car-levels list)
  (count-car-levels list 0)
  )

(define (count-car-levels list levels)
  (if (and (mpair? list) (not (mlist? list)))
      (count-car-levels (mcar list) (+ levels 1))
      levels
      )
  )

;Recorrido previo de la lista de parejas para preparar las funciones
(define (list-creator pair coord)
  (if (mpair? pair)
      (cons (list-recursion pair coord 'mcar) 
            (list-recursion pair (cons (+ (car coord) TAM) (cdr coord)) 'mcdr) 
            )
      )
  )

(define (list-recursion data coord type)
  (cons (function-creator data coord type #f #t)
        (if (mpair? ((eval type) data))
            (list-creator ((eval type) data) (coord-locator data coord type))
            (function-creator data coord type #t #t)
            )     
        )
  )

(define (coord-locator data coord type)
  (if (mpair? data)
      (let ((x (car coord))
            (y (cdr coord)))
        (if (mlist? data)
            (cond ((eq? type 'mcar) (cons x (+ MARGEN y)))
                  ((eq? type 'mcdr) (cons (+ MARGEN x) y))
                  )
            (cond ((eq? type 'mcar) (cons (- x MARGEN) (+ MARGEN y)))
                  ((eq? type 'mcdr) (cons (+ (/ MARGEN 2) x) (+ MARGEN y)))
                  )
            )
        )
      coord
      )
  )


;Creador de funciones
(define (function-creator pair coord tipo es-dato visible . lista)
  (let ((x (car coord))
        (y (cdr coord)))
    (lambda (dc msg)
      (let ((dato ((eval tipo) pair)))
        (cond ((string=? msg "dato") pair)
              ((string=? msg "coord") coord)
              ((string=? msg "tipo") tipo)
              ((string=? msg "visible") visible)
              ((string=? msg "es-dato") es-dato)
              ((string=? msg "lista") lista)
              ((string=? msg "dibuja")
               (if visible
                   (if es-dato
                       (list (send dc draw-text (~a dato) (+ x PADDING) (+ y PADDING)))
                       (list 
                        (send dc draw-rectangle
                              x y       ; Top-left at (x, y), y pixels down from top-left
                              TAM TAM)  ; wide and high
                        (if (mpair? dato)
                            (let ((child-coords (coord-locator pair coord tipo)))
                              (cond ((eq? tipo 'mcar)
                                     (send dc draw-line
                                           (+ x (/ TAM 2)) (+ y TAM)
                                           (+ (car child-coords) TAM) (cdr child-coords))
                                     )
                                    ((eq? tipo 'mcdr)
                                     (if (mlist? dato)
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
            (if (mpair? ((eval (funcion dc "tipo")) (funcion dc "dato")))
                (cons (function-creator (funcion dc "dato") (funcion dc "coord") (funcion dc "tipo") (funcion dc "es-dato") (not (funcion dc "visible")) (funcion dc "lista"))
                      (cdr funciones))
                funciones
                )
            (cons (function-creator (funcion dc "dato") (funcion dc "coord") (funcion dc "tipo") (funcion dc "es-dato") (funcion dc "visible") (funcion dc "lista"))
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
    (field (lista-funciones (draw pairs)))
    
    (define/override (on-event e)
      (if (equal? (send e get-event-type) 'left-down)
          (let ((my-dc (get-dc)))
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

(define lista-parejas (mcons (mcons 'a (mcons (mlist 1 2) 'b)) (mcons 1 2)))

(define frame (new frame%
                   (label "Box and Pointer")
                   (width 300)
                   (height 300)))
(new my-canvas% (pairs lista-parejas) (parent frame))   
(send frame show #t)