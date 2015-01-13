#lang racket
(provide print-pareja)

(define (print-pareja pareja)
  (if (pair? pareja)
      (begin 
        (display "(")
        (print-dato (car pareja))
        (display " . ")
        (print-dato (cdr pareja))
        (display ")"))
       (display "No es una pareja")))

(define (print-dato dato)
  (if (pair? dato)
      (print-pareja dato)
      (display dato)))