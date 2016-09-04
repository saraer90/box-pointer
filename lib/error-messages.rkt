#lang racket
(provide get-error)

(define (get-error method text)
   (error 'Error (string-append "[" method "] " text))
)


