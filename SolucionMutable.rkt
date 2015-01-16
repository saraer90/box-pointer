(#%require  racket/draw)
(#%require  racket/gui/base)
(#%require  racket/format)
(#%require compatibility/mlist)

(require "lib/utils.rkt")
(require "lib/settings.rkt")

;Creación de la lista de funciones: visible siempre a true en el inicio
;Preparar margen izquierdo 
(define (draw list) (list-creator list (if (mlist? list)
                                           (cons 0 CYCLE-MARGIN)
                                           (cons (* MARGEN (car-levels list)) CYCLE-MARGIN)) (cons (cons '() (cons 0 0)) '())))

;Según los niveles de Car en modo arbol
(define (car-levels list)
  (count-car-levels (mcar list) 0)
  )

(define (count-car-levels list levels)
  (if (and (mpair? list) (not (mlist? list)))
      (count-car-levels (mcar list) (+ levels 1))
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
  (let ((ancestors (cons (cons data coord) old-ancestors))) ;Be carefull!! father cycle
    (mcons (function-creator data coord type #f #t ancestors) ;Creates the box, data is false
           ;If child is a pair: moves coords, else paints inside the box
           (if (mpair? ((eval type) data))
               ;If it's a pair, then check the cycles
               (let ((cycle-data (cycle-finder ((eval type) data) ancestors)))
                 (if (car cycle-data)
                     (function-creator data coord type #t #t ancestors) ;If it's a cycle send it as data that won't be printed
                     (list-creator ((eval type) data) (coord-locator data coord type) ancestors)
                     
                     )
                 )
               (function-creator data coord type #t #t ancestors)
               )
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
            (cond ((eq? type 'mcar) 
                   (if (mpair? (mcdr data)) (cons (- x MARGEN) (+ MARGEN y)) (cons (- x (/ TAM 2)) (+ MARGEN y))))
                  ((eq? type 'mcdr)
                   (if (mpair? (mcar data)) (cons (+ (/ MARGEN 2) x) (+ MARGEN y)) (cons (- x (/ TAM 2)) (+ MARGEN y))))
                  )
            )
        )
      coord
      )
  )

;Busca bucles
(define (cycle-finder pair ancestors)
  (if (null? ancestors) 
      (cons #f '())
      (if (eq? (caar ancestors) pair)
          (cons #t (cdar ancestors))
          (cycle-finder pair (cdr ancestors))
          )
      )
  )


;Creador de funciones
(define (function-creator pair coord tipo es-dato visible ancestors)
  (let ((x (car coord))
        (y (cdr coord)))
    (lambda (dc msg)
      (let ((dato ((eval tipo) pair))
            (cycle (cycle-finder ((eval tipo) pair) ancestors)))
        (cond ((string=? msg "dato") pair)
              ((string=? msg "coord") coord)
              ((string=? msg "tipo") tipo)
              ((string=? msg "visible") visible)
              ((string=? msg "es-dato") es-dato)
              ((string=? msg "ancestors") ancestors)
              ((string=? msg "dibuja")
               (if visible
                   (if es-dato
                       (if (not (car cycle)) (list (send dc draw-text (~a dato) (+ x PADDING) (+ y PADDING))))
                       (list 
                        (send dc draw-rectangle
                              x y       ; Top-left at (x, y), y pixels down from top-left
                              TAM TAM)  ; wide and high
                        (if (mpair? dato)
                            (let ((child-coords (if (car cycle) (cdr cycle) (coord-locator pair coord tipo))))
                              (cond ((car cycle) 
                                     (let ((original-pen (send dc get-pen)))
                                       (send dc set-pen (new pen% [color "red"])) ;[style 'long-dash]
                                       (send dc draw-line
                                             (+ x TAM) (+ y (/ TAM 2))
                                             (+ (+ x TAM) CYCLE-MARGIN) (+ y (/ TAM 2)))
                                       (send dc draw-line
                                             (+ (+ x TAM) CYCLE-MARGIN) (+ y (/ TAM 2))
                                             (+ (+ x TAM) CYCLE-MARGIN) (- (cddr cycle) CYCLE-MARGIN))
                                       (send dc draw-line
                                             (+ (+ x TAM) CYCLE-MARGIN) (- (cddr cycle) CYCLE-MARGIN)
                                             (cadr cycle) (- (cddr cycle) CYCLE-MARGIN))
                                       (send dc draw-line
                                             (cadr cycle) (- (cddr cycle) CYCLE-MARGIN)
                                             (cadr cycle)(cddr cycle))
                                       (send dc set-pen original-pen)
                                     )
                                     )
                                    ((eq? tipo 'mcar)
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
  (if (mpair? (mcar funciones))
      (detector-colisiones funciones mouse-click dc)
      (let ((funcion (mcar funciones)))
        (if (intersectan? (funcion dc "coord") mouse-click)
            (if (mpair? ((eval (funcion dc "tipo")) (funcion dc "dato")))
                (mcons (function-creator (funcion dc "dato") (funcion dc "coord") (funcion dc "tipo") 
                                         (funcion dc "es-dato") (not (funcion dc "visible")) (funcion dc "ancestors"))
                       (mcdr funciones))
                funciones
                )
            (mcons (function-creator (funcion dc "dato") (funcion dc "coord") (funcion dc "tipo") 
                                     (funcion dc "es-dato") (funcion dc "visible") (funcion dc "ancestors"))
                   (detector-colisiones (mcdr funciones) mouse-click dc)
                   )
            )
        )
      )
  )

(define (detector-colisiones funciones mouse-click dc)
  (if (mpair? funciones)
      (mcons (detecta-colision (mcar funciones) mouse-click dc) (detecta-colision (mcdr funciones) mouse-click dc))
      funciones
      )
  )


;Pinta una lista de funciones
(define (pinta funciones dc)
  (if (pair? (mcar funciones))
      (pinta-lista funciones)
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
(set-mcdr! (mcar lista-parejas) (mcar lista-parejas))
(set-mcdr! (mcdr lista-parejas) lista-parejas)

(define x (mcons 'a (mcons 'b (mcons 'c null))))
(set-mcdr! (mcdr (mcdr x)) x)

(define frame (new frame%
                   (label "Box and Pointer")
                   (width 300)
                   (height 300)))
(new my-canvas% (pairs lista-parejas) (parent frame))   
(send frame show #t)