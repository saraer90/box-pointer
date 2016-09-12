;;;;;;;;;Creates the structure and the list of functions that emulates objects of elements boxes or data of the
;;;;;;;;;box and pointer diagram
#lang racket
(#%require compatibility/mlist)
(require "coordinates.rkt")
(require "settings.rkt")
(require "error-messages.rkt")

(provide new-diagram)
(provide new-element)
(provide re-create)
(provide search-element)
(provide move-diagram)
(provide get-diagram-coords)
(provide cycle-finder)


;Creación de la lista de funciones: visible siempre a true en el inicio
;Añade margen superior e izquierdo para poder dibujar las lineas de los ciclos en caso de que fuese necesario
;Si no es una lista mira los niveles de car para añadir margen a la izquierda y todo el gráfico sea visible.
(define (new-diagram list) (list-creator list (new-coord CYCLE-MARGIN CYCLE-MARGIN) '()))

(define calculados (mlist '*table))

(define (get key)
   (let ((record (massq key (mcdr calculados))))
      (if (not record)
         '()
         (mcdr record))))

(define (put key value)
   (let ((record (massq key (mcdr calculados))))
      (if (not record)
	      (set-mcdr! calculados (mcons (mcons key value)
                                 (mcdr calculados)))
	      (set-mcdr! record value)
      )
   )
)


;Calcula cuantos niveles hay en cuanto a la parte car de las parejas
;;Debe ser un pair, si es una lista dejamos de contar ya que se dibujará hacia la derecha.
;;Además si se encuentra un ciclo también se dejara de contar.
(define (count-car-levels list ancestors)
  (if (and (mpair? list) (or (mpair? (mcar list)) (mpair? (mcdr list))) (not (mlist? list)) (not (car (cycle-finder list ancestors))))
      ;(let ((calculado (get (~a list))))
        ;(if (null? calculado)
              ;(let ((result
                     (+ 1 (count-car-levels (mcar list) (cons (cons list '()) ancestors)) ;No tenemos coordenadas, para reutilizar el metodo añadimos una pareja vacía
                          (count-car-levels (mcdr list) (cons (cons list '()) ancestors)))
                     ;))
                ;(put (~a list) result)
                ;result
             ;calculado)
        0
        )
  )


;Recorrido previo de la lista de parejas para preparar las funciones y sus coordenadas
(define (list-creator pair coord ancestors)
  (if (mpair? pair)
      (mcons (list-recursion pair coord 'mcar ancestors) 
             (list-recursion pair (move-coord coord SIZE 0) 'mcdr ancestors) 
             )
      null
      )
  )

(define (list-recursion data coord type old-ancestors)
  (let ((ancestors (cons (cons data coord) old-ancestors)))   ;Añadimos el padre a la lista de antecesores
    (mcons (new-element data coord type #f #t ancestors) ;Dibujamos la caja
           ;Si el hijo es una pareja movemos las coordenadas sino pintamos directamente dentro
           (if (mpair? ((eval type) data))
               ;Si es una pareja miramos los bucles
               (let ((cycle-data (cycle-finder ((eval type) data) ancestors)))
                 (if (car cycle-data)
                     (new-element data coord type #t #t ancestors cycle-data) ;Si es un ciclo se envía como tipo dato y pintará la linea al antecesor
                     (list-creator ((eval type) data) (coord-locator data coord type ancestors) ancestors) ;Recursion
                     )
                 )
               (new-element data coord type #t #t ancestors)
               )
           )
    )
  )

;Llama al creador de funciones a partir de un elemento mutado para representar los cambios realizados, 
;calcula las nuevas funciones
(define (re-create funciones)
  (let ((funcion (mcar funciones)))
        (let ((funcion (mcar funciones)))
          (let ((data (funcion "dato"))
                (coord (funcion "coord"))
                (type (funcion "tipo")))
            (let ((new-pair (list-recursion data coord type (cdr (funcion "ancestors")))))
              (set-mcar! funciones (mcar new-pair))
              (set-mcdr! funciones (mcdr new-pair))
            )
         )
      )
    )
)

;Calcula la posición de la siguiente caja
(define (coord-locator data coord type ancestors)
  (if (mpair? data)
      (cond ((eq? type 'mcdr) (move-coord coord MARGIN 0))
            ((eq? type 'mcar)
             (if (and (mpair? (mcdr data)) (not (mlist? data)))
                 (move-coord coord 0 (+ MARGIN (* MARGIN (count-car-levels (mcdr data) ancestors))))
                 (move-coord coord 0 MARGIN)
             ))
      )
      coord ;En caso de ser un dato simple dejamos la coordenada igual que la de la caja que lo contiene
      )
)


;Busca bucles según una lista de antecesores
;En la primera parte de la pareja tenemos el elemento y en el cdr la coordenada
(define (cycle-finder pair ancestors)
  (if (null? ancestors) 
      (cons #f '())
      (if (eq? (caar ancestors) pair) 
          (cons #t (cdar ancestors))
          (cycle-finder pair (cdr ancestors))
          )
      )
  )

;Creador de funciones ya sean cajas o datos
(define (new-element pair coord tipo es-dato visible ancestors . args)
  (let ((parent-coord (if (null? (cdr ancestors)) '() (cdadr ancestors)))
        (old-data (~a ((eval tipo) pair)))
        (cycle (if (null? args) (cons #f '()) (car args)))) ;Guardamos el dato que contiene como cadena para que no nos afecten las mutaciones
    (lambda (msg)
      (cond ((string=? msg "dato") pair)
            ((string=? msg "coord") coord)
            ((string=? msg "tipo") tipo)
            ((string=? msg "visible") visible)
            ((string=? msg "es-dato") es-dato)
            ((string=? msg "parent-coord") parent-coord)
            ((string=? msg "cycle") cycle)
            ((string=? msg "ancestors") ancestors)
            ((string=? msg "cambio") (not (equal? old-data (~a ((eval tipo) pair))))) ;Para saber si ha habido una mutación comparamos con la cadena anterior
            (else (get-error "Elemento" (string-append "No existe el atributo: " msg)))
            )
      )
    )
  )

 ;;;;;; Get max and min x y coords.
(define (get-coords diagram)
  (if (mpair? diagram)
      (append (get-coords (mcar diagram)) (get-coords (mcdr diagram)))
      (list (diagram "coord"))))

(define (get-diagram-coords diagram)
  (let ((all-coords (get-coords diagram)))
    (let ((all-x-coords (map get-x all-coords))
          (all-y-coords (map get-y all-coords)))
      (cons (cons (apply min all-x-coords) (apply min all-y-coords))
            (cons (+ (apply max all-x-coords) SIZE (* 2 CYCLE-MARGIN)) (+ (apply max all-y-coords) SIZE (* 2 CYCLE-MARGIN))))
      )
    )
  )

;;;;;;;;;;;;;;;;Mover el diagrama;;;;;;;;;;;;;;;;;;;;;;;;
(define (function-coord-updater diagram difference)
  (if (mpair? diagram)
      (coord-updater diagram difference)
      (if (diagram "es-dato")
          diagram
          (let ((orig-coord (diagram "coord")))
            (begin (set-mcar! orig-coord (+ (mcar orig-coord) (car difference))) 
                   (set-mcdr! orig-coord (+ (mcdr orig-coord) (cdr difference)))
                   diagram
                   )
            )
          )
      )
  )

(define (coord-updater diagram difference)
  (if (mpair? diagram)
      (mcons (function-coord-updater (mcar diagram) difference) (function-coord-updater (mcdr diagram) difference))
      (function-coord-updater diagram difference)
      )
  )

;Mueve las posiciones del diagrama desde el punto seleccionado los pixeles indicados
(define (move-diagram diagram start-point difference)
  (let ((start-box (search-box diagram start-point)))
    (if (eq? start-box #f)
        #f
        (set! start-box (coord-updater start-box difference))
    )
  )
)

;;;;;;;;;;;;;Dada una coordenada nos da la caja sobre el que se ha hecho click junto a su par y sus hijos.
(define (search-box diagram mouse-click)
  (if (mpair? diagram)
      (if (mpair? (mcar diagram))
          (if (or (intersect? ((mcar (mcar diagram)) "coord") mouse-click)
                  (intersect? ((mcar (mcdr diagram)) "coord") mouse-click))
              diagram
              (let ((search-cdr (search-box (mcdr (mcdr diagram)) mouse-click)))
                (if (eq? search-cdr #f)
                    (search-box (mcdr (mcar diagram)) mouse-click)
                    search-cdr
                    )
                )
              )
          #f
          )
      #f
   )
)

;;;;;;;;;;;;;Dada una coordenada nos da el elemento sobre el que se ha hecho click y sus hijos.
(define (recursive-search funciones mouse-click)     
  (let ((funcion (mcar funciones)))
    (if (intersect? (funcion "coord") mouse-click)
        funciones
        (search-element (mcdr funciones) mouse-click)
        )
    )
  )

(define (search-element diagram mouse-click)
  (if (mpair? diagram)
      (let ((search-cdr (recursive-search (mcdr diagram) mouse-click)))
        (if (eq? search-cdr #f)
            (recursive-search (mcar diagram) mouse-click)
            search-cdr
            )
        )
      #f
      )
)