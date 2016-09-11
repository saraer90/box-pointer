#lang racket
(#%require  racket/draw)
(#%require  racket/draw/arrow)
(require "settings.rkt")
(require "coordinates.rkt")

(provide paint-diagram)

;Prints the loops line
;Line ending
(define (draw-cycle-ending coord cycle dc)
  (let ((x-origin (get-x coord))
        (y-origin (get-y coord))
        (x-dest (get-x cycle))
        (y-dest (get-y cycle)))
    (send dc draw-line
          x-origin y-origin
          x-origin (- y-dest CYCLE-MARGIN))
    (send dc draw-line
          x-origin (- y-dest CYCLE-MARGIN)
          x-dest (- y-dest CYCLE-MARGIN))
    (draw-arrow dc	
          x-dest (- y-dest CYCLE-MARGIN)
          x-dest y-dest 0 0 #:arrow-head-size ARROW-HEAD-SIZE #:arrow-root-radius 0)
    )
  )

;Start
(define (draw-cycle coord type cycle dc)
  (let ((original-pen (send dc get-pen))
        (x-origin (get-x coord))
        (y-origin (get-y coord))
        )
    (send dc set-pen (new pen% [color "red"])) ;[style 'long-dash]
    (if (eq? type 'mcdr)
        (begin (send dc draw-line
                     (+ x-origin SIZE) (+ y-origin (/ SIZE 2)) ;Start at box' right-center
                     (+ (+ x-origin SIZE) CYCLE-MARGIN) (+ y-origin (/ SIZE 2)))
               (draw-cycle-ending (move-coord coord (+ SIZE CYCLE-MARGIN) (/ SIZE 2)) cycle dc)
               )
        (begin (send dc draw-line
                     x-origin (+ y-origin (/ SIZE 2))  ;Start at box' left-center
                     (- x-origin CYCLE-MARGIN) (+ y-origin (/ SIZE 2)))
               (draw-cycle-ending (move-coord coord (- CYCLE-MARGIN) (/ SIZE 2)) cycle dc)
               )
        )
    ;Common
    (send dc set-pen original-pen)
    )
  )

(define (print function dc)
  (let ((coord (function "coord"))
        (tipo (function "tipo"))
        (pair (function "dato"))
        (ancestors (function "ancestors"))
        (es-dato (function "es-dato"))
        (visible (function "visible")))
    (let ((x (get-x coord))
          (y (get-y coord))
          (cycle (function "cycle"))
          (dato ((eval tipo) pair))
          (parent-coord (if (null? (cdr ancestors)) '() (cdadr ancestors))))
               (if es-dato
                   (if (car cycle) 
                       ;Si como dato tenemos un ciclo, debemos pintar la linea
                       (draw-cycle coord tipo (cdr cycle) dc)
                       (if (eq? dato '())
                           (send dc draw-line x (+ y SIZE) (+ x SIZE) y)
                           (send dc draw-text (~a dato #:max-width MAX-CONTENT) (+ x PADDING) (+ y PADDING)) ;Dato simple
                       )
                    )
                   (begin 
                     (send dc draw-rectangle
                           x y       ; Top-left at (x, y), y pixels down from top-left
                           SIZE SIZE)  ; wide and high
                     (if (and (mpair? parent-coord) (eq? tipo 'mcar)) ;Pintamos las lineas hacia el padre, se encargará la parte car
                         (if (and (> x (+ (mcar parent-coord) SIZE)) (< y (+ (mcdr parent-coord) SIZE)))
                             (draw-arrow dc ;Si vamos en horizontal
                                   (+ (mcar parent-coord) SIZE ) (+ (mcdr parent-coord) (/ SIZE 2))
                                   x (+ y (/ SIZE 2)) 0 0 #:arrow-head-size ARROW-HEAD-SIZE #:arrow-root-radius ARROW-ROOT-RADIUS)
                             (draw-arrow dc ;En vertical
                                   (+ (mcar parent-coord) (/ SIZE 2)) (+ (mcdr parent-coord) SIZE)
                                   (+ x (/ SIZE 2)) y 0 0 #:arrow-head-size ARROW-HEAD-SIZE #:arrow-root-radius ARROW-ROOT-RADIUS)
                             )
                         null
                         )   
                     (if (not visible)
                         ;Cuando no es visible mostramos el indicador para que se expanda
                         (send dc draw-text (~a "+") (+ x PADDING) (+ y PADDING))
                         null
                         )
                     )
                   )
               )
    )
  )


;Prints a list of functions calling print 
;If finds an invisible box stops and doesn't print its childs.
(define (paint functions dc)
  (if (mpair? (mcar functions))
      (paint-diagram functions dc)
      (if ((mcar functions) "visible") ;Calls the function with parameter visible to know if it's visible or not.
          (cons (print (mcar functions) dc) (paint-diagram (mcdr functions) dc))
          (print (mcar functions) dc) ;If not, prints the element but stops recursion.
          )
      )
  )

(define (paint-diagram functions dc)
  (if (mpair? functions)
      (cons (paint (mcar functions) dc) (paint (mcdr functions) dc))
      (print functions dc)
      )
  )
