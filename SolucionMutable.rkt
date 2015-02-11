(#%require  racket/draw)
(#%require  racket/gui/base)
(#%require  racket/format)
(#%require compatibility/mlist)

(require "lib/utils.rkt")
(require "lib/settings.rkt")

;Creación de la lista de funciones: visible siempre a true en el inicio
;Añade margen superior e izquierdo para poder dibujar las lineas de los ciclos en caso de que fuese necesario
(define (draw list) (list-creator list (if (mlist? list)
                                           (cons CYCLE-MARGIN CYCLE-MARGIN)
                                           (cons (+ (* MARGEN (car-levels list)) CYCLE-MARGIN) CYCLE-MARGIN)) '()))

;Calcula cuantos niveles hay en cuanto a la parte car de las parejas, para calcular el margen izquierdo
(define (car-levels list)
  (count-car-levels list -1 '())
  )

(define (count-car-levels list levels ancestors)
  (if (and (mpair? list) (not (mlist? list)) (not (car (cycle-finder list ancestors))))
      (count-car-levels (mcar list) (+ levels 1) (cons (cons list ancestors) '())) ;No tenemos coordenadas
      levels
      )
  )

;Recorrido previo de la lista de parejas para preparar las funciones y sus coordenadas
(define (list-creator pair coord ancestors)
  (if (mpair? pair)
      (mcons (list-recursion pair coord 'mcar ancestors) 
             (list-recursion pair (cons (+ (car coord) TAM) (cdr coord)) 'mcdr ancestors) 
             )
      )
  )

(define (list-recursion data coord type old-ancestors)
  (let ((ancestors (cons (cons data coord) old-ancestors))) ;Añadimos el padre a la lista de antecesores
    (mcons (function-creator data coord type #f #t ancestors) ;Dibujamos la caja
           ;Si el hijo es una pareja movemos las coordenadas sino pintamos directamente dentro
           (if (mpair? ((eval type) data))
               ;Si es una pareja miramos los bucles
               (let ((cycle-data (cycle-finder ((eval type) data) ancestors)))
                 (if (car cycle-data)
                     (function-creator data coord type #t #t ancestors) ;Si es un ciclo se envía como datos que no se pintarán
                     (list-creator ((eval type) data) (coord-locator data coord type) ancestors) ;Recursion
                     )
                 )
               (function-creator data coord type #t #t ancestors)
               )
           )
    )
  )

;Calcula la posición de la siguiente caja
(define (coord-locator data coord type)
  (if (mpair? data)
      (let ((x (car coord))
            (y (cdr coord)))
        (if (mlist? data)
            (cond ((eq? type 'mcar) (cons x (+ MARGEN y)))  ;Cuando se trate de una lista descendemos verticalmente en linea
                  ((eq? type 'mcdr) (cons (+ MARGEN x) y))  ;recta, y si es cdr lo haremos horizontalmente
                  )
            (cond ((eq? type 'mcar) 
                   (if (mpair? (mcdr data)) (cons (- x MARGEN) (+ MARGEN y)) (cons (- x (/ TAM 2)) (+ MARGEN y))))
                  ((eq? type 'mcdr)
                   (if (mpair? (mcar data)) (cons (+ (/ MARGEN 2) x) (+ MARGEN y)) (cons (- x (/ TAM 2)) (+ MARGEN y))))
                  ) ;Si no tenemos una lista, comprobaremos que tipo de dato hay en la otra parte de la pareja
            )       ;si tambien es una pareja pondremos las flechas en diagonal, si no en vertical
        )
      coord ;En caso de ser un dato simple dejamos la coordenada igual que la de la caja que lo contiene
      )
  )

;Busca bucles según una lista de antecesores
;En la primera parte de la pareja tenemos el elemento y en el cdr la coordenada
(define (cycle-finder pair ancestors)
  (if (null? ancestors) 
      (cons #f '())
      (if (eq? (caar ancestors) pair) 
          (cons #t (cdar ancestors))
          (cycle-finder pair (cdr ancestors))
          )
      )
  )

;Pinta linea de bucles
;Final de la linea
(define (draw-cycle-ending coord cycle dc last-coord)
  (let ((x (car coord))
        (y (cdr coord)))
    (if (> x (car cycle))
        (send dc draw-line
              (- x CYCLE-MARGIN) (- (cdr cycle) CYCLE-MARGIN)
              (+ (cadr cycle) TAM) (- (cdr cycle) CYCLE-MARGIN))
        (send dc draw-line
              (+ (car cycle) TAM) (- (cdr cycle) CYCLE-MARGIN)
              (+ (car cycle) TAM) (cdr cycle)) 
        )
    )
  )

;Inicio de la linea
(define (draw-cycle coord tipo cycle dc)
  (let ((original-pen (send dc get-pen))
        (x (car coord))
        (y (cdr coord)))
    (send dc set-pen (new pen% [color "red"])) ;[style 'long-dash]
    (if (eq? tipo 'mcdr)
        (list (send dc draw-line
                    (+ x TAM) (+ y (/ TAM 2))
                    (+ (+ x TAM) CYCLE-MARGIN) (+ y (/ TAM 2)))
              (send dc draw-line
                    (+ (+ x TAM) CYCLE-MARGIN) (+ y (/ TAM 2))
                    (+ (+ x TAM) CYCLE-MARGIN) (- (cdr cycle) CYCLE-MARGIN))
              (send dc draw-line
                    (+ (+ x TAM) CYCLE-MARGIN) (- (cdr cycle) CYCLE-MARGIN)
                    (car cycle) (- (cdr cycle) CYCLE-MARGIN))
              (send dc draw-line
                    (car cycle) (- (cdr cycle) CYCLE-MARGIN)
                    (car cycle)(cdr cycle))
              )
        (list (send dc draw-line
                    x (+ y (/ TAM 2))
                    (- x CYCLE-MARGIN) (+ y (/ TAM 2)))
              (send dc draw-line
                    (- x CYCLE-MARGIN) (+ y (/ TAM 2))
                    (- x CYCLE-MARGIN) (- (cdr cycle) CYCLE-MARGIN))
              (draw-cycle-ending coord cycle dc)
              )
        )
    ;Common
    (send dc set-pen original-pen)
    )
  )

;Creador de funciones
(define (function-creator pair coord tipo es-dato visible ancestors)
  (let ((x (car coord))
        (y (cdr coord))
        (old-data (~a ((eval tipo) pair)))
        (old-data-reference ((eval tipo) pair)))
    (lambda (dc msg)
      (cond ((string=? msg "dato") pair)
            ((string=? msg "cambio") (not (equal? old-data (~a ((eval tipo) pair)))))
            ((string=? msg "coord") coord)
            ((string=? msg "tipo") tipo)
            ((string=? msg "visible") visible)
            ((string=? msg "es-dato") es-dato)
            ((string=? msg "ancestors") ancestors)
            ((string=? msg "dibuja")
             (let ((dato ((eval tipo) pair))
                   (cycle (cycle-finder ((eval tipo) pair) ancestors)))
               (if visible
                   (if es-dato
                       (if (not (car cycle)) 
                           (list (send dc draw-text (~a dato) (+ x PADDING) (+ y PADDING))) ;Data
                           ;Cycles wont be printed
                           )
                       (list 
                        (send dc draw-rectangle
                              x y       ; Top-left at (x, y), y pixels down from top-left
                              TAM TAM)  ; wide and high
                        (if (mpair? dato) ;Print lines
                            (let ((child-coords (if (car cycle) (cdr cycle) (coord-locator pair coord tipo))))
                              (cond ((car cycle) 
                                     (if (eq? tipo 'mcar)
                                         (draw-cycle coord tipo (cons (+ (cadr cycle) TAM) (cddr cycle)) dc) ;set X at pair's center
                                         (draw-cycle coord tipo (cdr cycle) dc))
                                     )
                                    ((eq? tipo 'mcar)
                                     (send dc draw-line
                                           (+ x (/ TAM 2)) (+ y TAM)
                                           (+ (car child-coords) TAM) (cdr child-coords))
                                     )
                                    ((eq? tipo 'mcdr)
                                     (if (mlist? dato) ;Start at box' right side
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
             )
            )
      )
    )
  )


;Llama al creador de funciones para representar los cambios realizados
(define (change-detector funcion dc)
  (list-recursion (funcion dc "dato") (funcion dc "coord") (funcion dc "tipo") (cdr (funcion dc "ancestors")))
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
  (if (mpair? (mcar funciones))
      (detector-colisiones-mutaciones funciones mouse-click dc)
      (let ((funcion (mcar funciones)))
        (if (funcion dc "cambio") 
            (detecta-colision (change-detector funcion dc) mouse-click dc)
            (if (intersectan? (funcion dc "coord") mouse-click)
                (if (mpair? ((eval (funcion dc "tipo")) (funcion dc "dato")))
                    (mcons (function-creator (funcion dc "dato") (funcion dc "coord") (funcion dc "tipo") 
                                             (funcion dc "es-dato") (not (funcion dc "visible")) (funcion dc "ancestors"))
                           (mcdr funciones))
                    funciones ;Datos simples no se esconden
                    )
                (mcons (function-creator (funcion dc "dato") (funcion dc "coord") (funcion dc "tipo") 
                                         (funcion dc "es-dato") (funcion dc "visible") (funcion dc "ancestors"))
                       (detector-colisiones-mutaciones (mcdr funciones) mouse-click dc)
                       )
                )
            )
        )
      )
  )

(define (detector-colisiones-mutaciones funciones mouse-click dc)
  (if (mpair? funciones)
      (mcons (detecta-colision (mcar funciones) mouse-click dc) (detecta-colision (mcdr funciones) mouse-click dc))
      funciones
      )
  )


;Pinta una lista de funciones
(define (pinta funciones dc)
  (if (mpair? (mcar funciones))
      (pinta-lista funciones dc)
      (if ((mcar funciones) dc "visible")
          (cons ((mcar funciones) dc "dibuja") (pinta-lista (mcdr funciones) dc))
          ((mcar funciones) dc "dibuja")
          )
      )
  )

(define (pinta-lista funciones dc)
  (if (mpair? funciones)
      (cons (pinta (mcar funciones) dc) (pinta (mcdr funciones) dc))
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
            ;(mprint-pareja lista-funciones)
            (set! lista-funciones (detector-colisiones-mutaciones lista-funciones (cons (send e get-x) (send e get-y)) my-dc))
            ;(display "despues")
            ;(mprint-pareja lista-funciones)
            (refresh-now)
            )
          )
      )
    
    (define/override (on-paint)
      (pinta-lista lista-funciones (get-dc))
      )
    
    (define/public (get-lista) lista-funciones)
    )
  )

(define lista-parejas (mcons (mcons 'a (mcons (mlist 1 2) 'b)) (mcons 1 2)))
;(set-mcdr! (mcar lista-parejas) (mcar lista-parejas))
;(set-mcdr! (mcdr lista-parejas) lista-parejas)

;(define lista-parejas (mcons 1 2))
;(set-mcdr! lista-parejas lista-parejas)

;(define x (mcons 'a (mcons 'b (mcons 'c null))))
;(set-mcdr! (mcdr (mcdr x)) x)

(define frame (new frame%
                   (label "Box and Pointer")
                   (width 300)
                   (height 300)))
(define canvas (new my-canvas% (pairs lista-parejas) (parent frame)))
(send frame show #t)