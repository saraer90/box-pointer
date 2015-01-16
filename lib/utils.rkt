#lang racket
(provide print-pareja)
(provide mprint-pareja)

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


(define (mprint-pareja pareja)
  (if (mpair? pareja)
      (begin 
        (display "{")
        (mprint-dato (mcar pareja))
        (display " . ")
        (mprint-dato (mcdr pareja))
        (display "}"))
       (display "No es una pareja")))

(define (mprint-dato dato)
  (if (mpair? dato)
      (mprint-pareja dato)
      (display dato)))