#lang racket
(#%require  racket/draw)
(require "settings.rkt")
(require "coordinates.rkt")

(provide draw-cycle-ending)
(provide draw-cycle)
(provide paint)
(provide paint-diagram)
(provide get-diagram-coords)

 ;;;;;; Get max and min x y coords.
(define (get-coords diagram)
  (if (mpair? diagram)
      (append (get-coords (mcar diagram)) (get-coords (mcdr diagram)))
      (list (diagram "coord"))))

(define (get-diagram-coords diagram)
  (let ((all-coords (get-coords diagram)))
    (let ((all-x-coords (map get-x all-coords))
          (all-y-coords (map get-y all-coords)))
      (cons (cons (apply min all-x-coords) (apply min all-y-coords))
            (cons (+ (apply max all-x-coords) SIZE (* 2 CYCLE-MARGIN)) (+ (apply max all-y-coords) SIZE (* 2 CYCLE-MARGIN))))
      )
    )
  )

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
    (send dc draw-line
          x-dest (- y-dest CYCLE-MARGIN)
          x-dest y-dest)
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

(define (cycle-finder pair ancestors)
  (if (null? ancestors) 
      (cons #f '())
      (if (eq? (caar ancestors) pair) 
          (cons #t (cdar ancestors))
          (cycle-finder pair (cdr ancestors))
          )
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
          (cycle (cycle-finder ((eval tipo) pair) ancestors))
          (dato ((eval tipo) pair))
          (parent-coord (if (null? (cdr ancestors)) '() (cdadr ancestors))))
               (if es-dato
                   (if (car cycle) 
                       ;Si en vez de dato tenemos un ciclo, debemos pintar la linea
                       (draw-cycle coord tipo (cdr cycle) dc)                              
                       (send dc draw-text (~a dato) (+ x PADDING) (+ y PADDING)) ;Dato simple
                       )
                   (begin 
                     (send dc draw-rectangle
                           x y       ; Top-left at (x, y), y pixels down from top-left
                           SIZE SIZE)  ; wide and high
                     (if (and (mpair? parent-coord) (eq? tipo 'mcar)) ;Pintamos las lineas hacia el padre, se encargará la parte car
                         (if (and (> x (+ (mcar parent-coord) SIZE)) (< y (+ (mcdr parent-coord) SIZE)))
                             (send dc draw-line
                                   (+ (mcar parent-coord) SIZE ) (+ (mcdr parent-coord) (/ SIZE 2))
                                   x (+ y (/ SIZE 2)))
                             (send dc draw-line
                                   (+ (mcar parent-coord) (/ SIZE 2)) (+ (mcdr parent-coord) SIZE)
                                   (+ x SIZE) y)
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
