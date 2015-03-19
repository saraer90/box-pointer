(define (rep-loop) 
   (display "mi-interprete> ")  ; imprime un prompt
   (let ((expr (read)))            ; lee una expresión 
      (if (eq? expr 'adios)        ; el usuario quiere parar?
         (begin 
            (display "saliendo del bucle read-eval-print") 
            (newline))
         (begin                   ; expresión distinta de 'adios
            (write (eval expr))   ; evaluar e imprimir 
            (if (and (list? expr) (procedure? (car expr))) (scope-processor expr))   ; evaluar e imprimir 
            (newline) 
            (rep-loop)))))


(define (scope-processor expr)
  (display "procesar")
)

(rep-loop)