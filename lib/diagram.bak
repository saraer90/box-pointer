;;;;;;;;;Creates the structure and the list of functions that emulates objects of elements boxes or data of the
;;;;;;;;;box and pointer diagram
#lang racket
(#%require compatibility/mlist)
(require "coordinates.rkt")
(require "settings.rkt")

(provide new-diagram)
(provide new-element)
(provide re-create)
(provide search-element)

;Creación de la lista de funciones: visible siempre a true en el inicio
;Añade margen superior e izquierdo para poder dibujar las lineas de los ciclos en caso de que fuese necesario
;Si no es una lista mira los niveles de car para añadir margen a la izquierda y todo el gráfico sea visible.
(define (new-diagram list) (list-creator list (new-coord (+ (* MARGIN (car-levels list)) CYCLE-MARGIN) CYCLE-MARGIN) '()))

;Calcula cuantos niveles hay en cuanto a la parte car de las parejas, para calcular el margen izquierdo
(define (car-levels list)
  (count-car-levels list -1 '())
  )

;;Debe ser un pair, si es una lista dejamos de contar ya que se dibujará hacia la derecha o en vertical.
;;Además si se encuentra un ciclo también se dejara de contar.
(define (count-car-levels list levels ancestors)
  (if (and (mpair? list) (not (car (cycle-finder list ancestors))))
      (count-car-levels (mcar list) (+ levels 1) (cons (cons list ancestors) '())) ;No tenemos coordenadas, para reutilizar el metodo añadimos una pareja vacía
      levels
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
                     (new-element data coord type #t #t ancestors) ;Si es un ciclo se envía como tipo dato y pintará la linea al antecesor
                     (list-creator ((eval type) data) (coord-locator data coord type) ancestors) ;Recursion
                     )
                 )
               (new-element data coord type #t #t ancestors)
               )
           )
    )
  )


;Llama al creador de funciones para representar los cambios realizados, calcula las nuevas funciones
(define (re-create funciones)
  (let ((funcion (mcar funciones)))
        (let ((funcion (mcar funciones)))
          (let ((data (funcion "dato"))
                (coord (funcion "coord"))
                (type (funcion "tipo")))
            (let ((ancestors (cons (cons data coord) (cdr (funcion "ancestors"))))) ;Cambiamos la referencia a si mismo de la lista de antecesores
              (set-mcar! funciones (new-element data coord type #f #t ancestors))
              (set-mcdr! funciones
                         (if (mpair? ((eval type) data))
                             ;Si es una pareja miramos los bucles
                             (let ((cycle-data (cycle-finder ((eval type) data) ancestors)))
                               (if (car cycle-data)
                                   (new-element data coord type #t #t ancestors) ;Si es un ciclo se envía como tipo dato y pintará la linea al antecesor
                                   (list-creator ((eval type) data) (coord-locator data coord type) ancestors) ;Recursion
                                   )
                               )
                             (new-element data coord type #t #t ancestors)
                             )
                         )
              )
         )
      )
    )
)

;Calcula la posición de la siguiente caja
(define (coord-locator data coord type)
  (if (mpair? data)
      (cond ((eq? type 'mcar) 
             (if (mpair? (mcdr data))
                 (move-coord coord (- MARGIN) MARGIN) ;Car baja en diagonal
                 (move-coord coord (- (/ SIZE 2)) MARGIN) ;Si es un dato, bajamos en vertical
                 ))
            ((eq? type 'mcdr) ;Primero hay que mirar lo que tenemos en el car
             (if (mlist? ((eval type) data))
                 (move-coord coord MARGIN 0)
                 (if (mpair? (mcar data))
                         (if (mlist? (mcar data)) ;Si hay una lista
                             (move-coord coord (- (/ SIZE 2)) (* 2 MARGIN)) ;nos quedamos en la x pero nos movemos el doble en la y
                             (if (mlist? ((eval type) data))
                                 (move-coord coord MARGIN 0)
                                 (move-coord coord (/ MARGIN 2) MARGIN) 
                                 )
                          )
                 (move-coord coord (- (/ SIZE 2)) MARGIN)
                 )
                 )
             ) ;Si no tenemos una lista, comprobaremos que tipo de dato hay en la otra parte de la pareja
            )       ;si tambien es una pareja pondremos las flechas en diagonal, si no en vertical
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
(define (new-element pair coord tipo es-dato visible ancestors)
  (let ((parent-coord (if (null? (cdr ancestors)) '() (cdadr ancestors)))
        (old-data (~a ((eval tipo) pair)))) ;Guardamos el dato que contiene como cadena para que no nos afecten las mutaciones
    (lambda (msg)
      (cond ((string=? msg "dato") pair)
            ((string=? msg "cambio") (not (equal? old-data (~a ((eval tipo) pair))))) ;Para saber si ha habido una mutación comparamos con la cadena anterior
            ((string=? msg "coord") coord)
            ((string=? msg "parent-coord") parent-coord)
            ((string=? msg "tipo") tipo)
            ((string=? msg "visible") visible)
            ((string=? msg "es-dato") es-dato)
            ((string=? msg "ancestors") ancestors)
            )
      )
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