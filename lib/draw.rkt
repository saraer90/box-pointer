#lang racket
(#%require  racket/draw)
(require "settings.rkt")
(require "coordinates.rkt")

(provide draw-cycle-ending)
(provide draw-cycle)
(provide paint)
(provide paint-list)

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


;Prints a list of functions calling print 
;If finds an invisible box stops and doesn't print its childs.
(define (paint functions dc)
  (if (mpair? (mcar functions))
      (paint-list functions dc)
      (if ((mcar functions) dc "visible") ;Calls the function with parameter visible to know if it's visible or not.
          (cons ((mcar functions) dc "dibuja") (paint-list (mcdr functions) dc))
          ((mcar functions) dc "dibuja") ;If not, prints the element but stops recursion.
          )
      )
  )

(define (paint-list functions dc)
  (if (mpair? functions)
      (cons (paint (mcar functions) dc) (paint (mcdr functions) dc))
      (functions dc "dibuja")
      )
  )
