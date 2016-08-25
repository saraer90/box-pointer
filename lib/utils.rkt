#lang racket
(provide print-pair)
(provide mprint-pair)
(provide mcopy)

(define (print-pair structure)
  (if (pair? structure)
      (begin 
        (display "(")
        (print-element (car structure))
        (display " . ")
        (print-element (cdr structure))
        (display ")"))
       (display "No es una pareja")))

(define (print-element element)
  (if (pair? element)
      (print-pair element)
      (display element)))


(define (mprint-pair structure)
  (if (mpair? structure)
      (begin 
        (display "{")
        (mprint-element (mcar structure))
        (display " . ")
        (mprint-element (mcdr structure))
        (display "}"))
       (display "No es una pareja")))

(define (mprint-element element)
  (if (mpair? element)
      (mprint-pair element)
      (display element)))

(define (mcopy obj)
 (if (mpair? obj)
     (mcons (mcopy (mcar obj))
            (mcopy (mcdr obj)))
     obj
  )
 )