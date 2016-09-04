#lang racket
(provide print-pair)
(provide mprint-pair)
(provide mcopy)
(provide get-diagram-string)
(provide get-diagram-definition)
(require "coordinates.rkt")

(define (data-representation dato)
  (if (number? dato) 
      (~a dato) 
      (if (eq? dato '())
          "'()"
          (string-append "\"" (~a dato) "\"")
      )
   )
)

;;;;;;;;;;;;;;;;;;;;;Gets an string to show the definition of pairs at info label
(define (get-element-string function)
  (let ((tipo (function "tipo"))
        (pair (function "dato"))
        (es-dato (function "es-dato"))
        (visible (function "visible")))
    (let ((cycle (function "cycle"))
          (dato ((eval tipo) pair)))
               (if es-dato
                   (if (car cycle) 
                       " [loop] "
                       (if (eq? tipo 'mcar)
                           (string-append (data-representation dato)  " ")
                           (data-representation dato)
                       )
                    ) 
                    (if (not visible)
                         " [+] "
                         ""
                         )
                     )
                   )
               )
  )

(define (get-pair-string functions)
  (if (mpair? (mcar functions))
      (get-diagram-string functions)
      (if ((mcar functions) "visible") ;Calls the function with parameter visible to know if it's visible or not.
          (string-append
            (get-element-string (mcar functions)) 
            (get-diagram-string (mcdr functions))
          )
          (get-element-string (mcar functions)) ;If not, prints the element but stops recursion.
          )
      )
  )

(define (get-diagram-string functions)
  (if (mpair? functions)
        (string-append "(mcons "
         (get-pair-string (mcar functions))
         (get-pair-string (mcdr functions))
        ") ")
      (get-element-string functions)
      )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;Genera toda la definición de los elementos del canvas,
;;busca los elementos que son bucles
(define (get-diagram-definition functions)
  (string-append 
   "(define pairs "
      (get-base-definition functions)
    ")\n"
    ;(get-loop-definitions functions functions "")
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Gets an string to declare the definition of pairs
;;Difference: prints no visible elements at canvas
(define (get-element-definition function)
  (let ((tipo (function "tipo"))
        (pair (function "dato"))
        (es-dato (function "es-dato")))
    (let ((cycle (function "cycle"))
          (dato ((eval tipo) pair)))
               (if es-dato
                   (if (car cycle) 
                       " '[loop] "
                       (if (eq? tipo 'mcar)
                           (string-append (data-representation dato) " ")
                           (data-representation dato)
                       )
                    ) 
                   ""
                   )
               )
  )
  )

(define (get-pair-definition functions)
  (if (mpair? (mcar functions))
      (get-base-definition functions)
      (string-append (get-element-definition (mcar functions)) (get-base-definition (mcdr functions)))
  )
)

(define (get-base-definition functions)
  (if (mpair? functions)
       (string-append "(mcons "
                      (get-pair-definition (mcar functions))
                      (get-pair-definition (mcdr functions))
                      ") ")
       (get-element-definition functions)
       )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;Search the referenced element at the loop
;Busca la ruta al elemento
(define (find-route-branch funciones coord)     
  (let ((funcion (mcar funciones)))
    (if (intersect? (funcion "coord") coord)
        (list 'mcar)
        (let ((list-cdr (find-route (mcdr funciones) coord)))
          (if (empty? list-cdr) '() (cons 'mcdr list-cdr))
        )
    )
  )
)

(define (find-route funciones coord)
  (if (mpair? funciones)
      (let ((list-cdr (find-route-branch (mcdr funciones) coord)))
        (if (empty? list-cdr)
            (let ((list-car (find-route-branch (mcar funciones) coord)))
              (if (empty? list-car)
                  '()
                  (cons 'mcar list-car)
              )
            )
            (cons 'mcdr list-cdr)
        )
      )
      '()
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Search if there's any loop at the definition
(define (get-loop-element function all-functions route)
  (let ((es-dato (function "es-dato"))
         (cycle (function "cycle"))
         (element (function "dato"))
         (tipo (function "tipo")))
      (if (and es-dato (car cycle))
          (find-route all-functions (cdr cycle))
          ""
      )
  )
)

(define (get-loop-string functions all-functions route)
  (if (mpair? (mcar functions))
      (get-loop-definitions functions all-functions route)
      (string-append (get-loop-element (mcar functions) all-functions route) (get-loop-definitions (mcdr functions) all-functions route))
  )
)

(define (get-loop-definitions functions all-functions route)
  (if (mpair? functions)
      (string-append (get-loop-string (mcar functions) all-functions (string-append "(mcar " route))
                     (get-loop-string (mcdr functions) all-functions (string-append "(mcdr " route)))
      (get-loop-element functions all-functions route)
      )
)




;;Utiles para depurar
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
