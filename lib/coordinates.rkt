#lang racket
(require "settings.rkt")

(provide create-coord)
(provide get-x)
(provide get-y)
(provide incr-coord)
(provide move-coord)
(provide sum-coord)
(provide sub-coord)
(provide intersectan?)

;---------------------------------------------------
(define (create-coord x y) (mcons x y))

(define (get-x coord) (mcar coord))
(define (get-y coord) (mcdr coord))

(define (incr-coord coord inc) (move-coord coord inc inc))

(define (move-coord coord inc-x inc-y) 
  (mcons (+ (get-x coord) inc-x) (+ (get-y coord) inc-y))
)

(define (sum-coord coord1 coord2)
   (mcons (+ (get-x coord1) (get-x coord2)) (+ (get-y coord1) (get-y coord2)))
)

(define (sub-coord coord1 coord2) 
  (mcons (- (get-x coord1) (get-x coord2)) (- (get-y coord1) (get-y coord2)))
)

(define (intersectan? coord-box mouse-click) 
  (let ((x-ini (get-x coord-box))
        (y-ini (get-y coord-box))
        (x-click (get-x mouse-click))
        (y-click (get-y mouse-click)))
    (let ((x-fin (+ x-ini TAM))
          (y-fin (+ y-ini TAM)))
      (and (<= x-ini x-click) (<= y-ini y-click) (>= x-fin x-click) (>= y-fin y-click))
    )
  )
)
