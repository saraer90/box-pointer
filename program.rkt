#lang racket
;La parte de la UI nos da 
;menu-bar: barra de menú del frame
;canvas: el canvas para que podamos interactuar
;h-panel-events: espacio extra para controles de nuestros eventos, en un inicio oculto
(require "lib/UI.rkt")
(require "lib/diagram.rkt")
(#%require  racket/gui/base)

(provide box-pointer)

(define text-field-car (new text-field%
                        (label "Car: ")
                        (parent h-panel-events)))

(define text-field-cdr (new text-field%
                        (label "Cdr: ")
                        (parent h-panel-events)))

;;Button
(new button% [label "Bucle"]	 
   	 [parent h-panel-events]	 
            [callback 
            (lambda (boton evento)
                      (send canvas set-event start-loop))])


(define menu-modo (new menu%
                  [label "&Modo"]
                  [parent menu-bar]))

(new menu-item%
     [label "&Añadir nuevos nodos"]
     [parent menu-modo]
     [callback (lambda (menu evento)
                        (send canvas set-event add-child)
                        (send h-panel-events show #t))])

(new menu-item%
     [label "&Contrae / Expande"]
     [parent menu-modo]
     [callback (lambda (menu evento)
                        (send canvas set-event contract-expand)
                        (send h-panel-events show #f))])


;;;;;;;;;;;;;;;;;Callbacks de cada modo de trabajo;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;Contract-expand
(define (contract-expand canvas diagram click . args)
  (let ((funciones (search-element diagram click)))
  (if (not (eq? funciones #f)) 
      (let ((funcion (mcar funciones)))
        (if (mpair? ((eval (funcion "tipo")) (funcion "dato"))) 
            ;Cambia el estado anterior de la visibilidad al contrario en caso de intersección
            (set-mcar! funciones (new-element (funcion "dato") (funcion "coord") (funcion "tipo") 
                                                     (funcion "es-dato") (not (funcion "visible")) (funcion "ancestors"))
                                   )
            ;Datos simples no se esconden, dejamos la lista tal cual
            #f
            )
        )
      #f
      )
    )
  )

;;;;;;;;;;;;;add-child: Añade un hijo vacío a una caja
(define (add-child canvas diagram click . args)
  (let ((funciones (search-element diagram click)))
    (if (not (eq? funciones #f))
        (create-child canvas funciones (mcons (send text-field-car get-value) (send text-field-cdr get-value)) #f)
        #f
    )
  )
)

(define (create-child canvas functions child loop)
  (let ((function (mcar functions)))
    (let ((dato (function "dato")))
        (begin 
          ((eval (string->symbol (string-append "set-" (symbol->string (function "tipo")) "!"))) dato child)
          (re-create functions)  
        )         
      )
    )
)
    

;;;;;'start-loop : Añade un hijo siendo una referencia a otra caja
(define (start-loop canvas funciones click . args)
  (if (not (eq? funciones #f))
      (send canvas set-event end-loop)
      #f
  )
)

(define (end-loop canvas diagram click . args)
    (let ((origen (search-element diagram (car args)))
          (destino (search-element diagram click)))
      (if (and (not (eq? origen #f)) (not (eq? destino #f)))
          (create-child canvas origen ((mcar destino) "dato") #t)
          #f
          )
      )
  (send canvas set-event add-child)
)


;Inicializa el canvas
(define (box-pointer . args)
  (if (empty? args)
      (begin
        (send canvas new)
        (send canvas set-event contract-expand)
        (show-ui)
      )
      (if (eq? (length args) 1)
          (let ((pairs (car args)))
            (if (mpair? pairs)
                (begin 
                  (send canvas set-pairs pairs)
                  (send canvas set-event contract-expand)
                  (show-ui)
                 )
                (raise-type-error 'box-pointer "mpair?" pairs)
            )
          )
          (raise-arguments-error 'box-pointer "the expected number of arguments (any or 1) does not match the given number" 
                                 "arguments" args)
     ) 
  )
)
