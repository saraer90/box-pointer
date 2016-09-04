#lang racket
;La parte de la UI nos da 
;menu-bar: barra de menú del frame
;canvas: el canvas para que podamos interactuar
;h-panel-events: espacio extra para controles de nuestros eventos, en un inicio oculto
(require "lib/diagram.rkt")
(require "lib/error-messages.rkt")
(require "lib/UI.rkt")

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
            (begin
              ;Cambia el estado anterior de la visibilidad al contrario en caso de intersección
              (set-mcar! funciones (new-element (funcion "dato") (funcion "coord") (funcion "tipo") 
                                                (funcion "es-dato") (not (funcion "visible")) (funcion "ancestors"))
                         )
              (send canvas refresh-now)
            )
            ;Datos simples no se esconden, dejamos la lista tal cual
            #f
            )
        )
      (get-error "Contrae / expande" "Debes seleccionar un elemento del diagrama.")
      )
    )
  )

;;;;;;;;;;;;;add-child: Añade un hijo vacío a una caja
(define (add-child canvas diagram click . args)
  (let ((funciones (search-element diagram click)))
    (if (not (eq? funciones #f))
        (begin
          (create-child canvas funciones (mcons (send text-field-car get-value) (send text-field-cdr get-value)))
          (send canvas refresh-now)
        )
        (get-error "Añadir nuevos nodos" "Debes seleccionar un elemento del diagrama.")
    )
  )
)

(define (create-child canvas functions child)
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
(define origin (mcons 0 0))

(define (start-loop canvas diagram click . args)
  (let ((origin-element (search-element diagram click)))
    (if (not (eq? origin-element #f))
        (begin
          (set! origin origin-element)
          (send canvas show-info (string-append "Se ha seleccionado el elemento: " 
                                                (~a ((eval ((mcar origin) "tipo")) ((mcar origin) "dato")))))
          (send canvas set-event end-loop)
        )
        (get-error "Añadir nuevos nodos - Bucle" "Debes seleccionar un elemento del diagrama para iniciar el bucle.")
        )
    )
)

(define (compare-ancestors element list)
  (if (empty? list)
      #f
      (if (eq? element (car list))
          #t
          (compare-ancestors element (cdr list))
       )
   )
)

(define (search-ancestors origin destiny)
  (let ((ancestors ((mcar origin) "ancestors"))
        (element ((mcar destiny) "dato")))
    (let ((data-ancestors (map car ancestors))) ;En la segunda parte están las coordenadas
      (compare-ancestors element data-ancestors)
    )
  )
)

(define (end-loop canvas diagram click)
    (let ((destiny (search-element diagram click)))
      (if (not (eq? destiny #f))
          (let ((cycle (cycle-finder ((mcar destiny) "dato") ((mcar origin) "ancestors"))))
            (if (car cycle)
                (begin
                  (create-child canvas origin ((mcar destiny) "dato"))
                  (send canvas set-event add-child)
                  (send canvas refresh-now)
                  )
                (get-error "Añadir nuevos nodos - Bucle" "Debes seleccionar un antecesor del origen del bucle")
                )
            )
          (get-error "Añadir nuevos nodos - Bucle" "Debes seleccionar un elemento del diagrama para finalizar el bucle")
          )
      )
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
