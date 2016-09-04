#lang racket
(provide get-error)

(define (get-error method text)
   (raise-user-error 'Error (string-append "[" method "] " text))
)


