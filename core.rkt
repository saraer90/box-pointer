(#%require  racket/draw)
(#%require  racket/gui/base)
(#%require  racket/format)
(#%require compatibility/mlist)

(require "lib/utils.rkt")
(require "lib/settings.rkt")
(require "lib/coordinates.rkt")
(require "lib/draw.rkt")

;Creación de la lista de funciones: visible siempre a true en el inicio
;Añade margen superior e izquierdo para poder dibujar las lineas de los ciclos en caso de que fuese necesario
;Si no es una lista mira los niveles de car para añadir margen a la izquierda y todo el gráfico sea visible.
(define (draw list) (list-creator list (new-coord (+ (* MARGIN (car-levels list)) CYCLE-MARGIN) CYCLE-MARGIN) '()))

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

;;;;;;;;;;;;;;;;;Callbacks de cada modo de trabajo;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;Contract-expand
(define (contract-expand canvas diagram click . args)
  (let ((funciones (search-selected-pair diagram click)))
  (if (not (eq? funciones #f)) 
      (let ((funcion (mcar funciones)))
        (if (mpair? ((eval (funcion "tipo")) (funcion "dato"))) 
            ;Cambia el estado anterior de la visibilidad al contrario en caso de intersección
            (set-mcar! funciones (new-element (funcion "dato") (funcion "coord") (funcion "tipo") 
                                                     (funcion "es-dato") (not (funcion "visible")) (funcion "ancestors"))
                                   )
            ;Datos simples no se esconden, dejamos la lista tal cual
            )
        )
      )
    )
  )

;;;;;;;;;;;;;add-child: Añade un hijo vacío a una caja
(define (add-child canvas diagram click . args)
  (let ((funciones (search-selected-pair diagram click)))
    (if (not (eq? funciones #f))
        (create-child canvas funciones (mcons (send canvas get-car-child) (send canvas get-cdr-child)) #f)
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

;Llama al creador de funciones para representar los cambios realizados, calcula las nuevas funciones
(define (re-create funciones)
  (let ((funcion (mcar funciones)))
        (let ((funcion (mcar funciones)))
          (let ((data (funcion "dato"))
                (coord (funcion "coord"))
                (type (funcion "tipo")))
            (let ((ancestors (cons (cons data coord) (cdr (funcion "ancestors"))))) ;Cambiamos el padre de la lista de antecesores
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
    

;;;;;'start-loop : Añade un hijo siendo una referencia a otra caja
(define (start-loop canvas funciones click . args)
  (if (not (eq? funciones #f))
      (send canvas set-event 'end-loop)
  )
)

(define (end-loop canvas diagram click . args)
    (let ((origen (search-selected-pair diagram (car args)))
          (destino (search-selected-pair diagram click)))
      (if (and (not (eq? origen #f)) (not (eq? destino #f)))
          (create-child canvas origen ((mcar destino) "dato") #t)
          )
      )
  (send canvas set-event 'add-child)
)

;;;;;;;;;;;;Común: antes de realizar acciones buscamos sobre la pareja que se ha hecho click y la devolvemos
;Busca el elemento sobre el que hemos hecho click
(define (recursive-search funciones mouse-click)     
  (let ((funcion (mcar funciones)))
        (if (intersect? (funcion "coord") mouse-click)
            funciones
            (search-selected-pair (mcdr funciones) mouse-click)
       )
   )
)

(define (search-selected-pair funciones mouse-click)
  (if (mpair? funciones)
        (let ((search-cdr (recursive-search (mcdr funciones) mouse-click)))
          (if (eq? search-cdr #f)
            (recursive-search (mcar funciones) mouse-click)
            search-cdr
            )
          )
      #f
   )
)



;;;;;;;;;;;;;;;;Eventos de drag and drop;;;;;;;;;;;;;;;;;;;;;;;;
(define (function-coord-updater funciones difference)
  (if (mpair? funciones)
      (coord-updater funciones difference)
      (if (funciones "es-dato")
          funciones
          (let ((orig-coord (funciones "coord")))
            (begin (set-mcar! orig-coord (+ (mcar orig-coord) (car difference))) 
                   (set-mcdr! orig-coord (+ (mcdr orig-coord) (cdr difference)))
                   funciones
                   )
            )
          )
      )
  )

(define (coord-updater funciones difference)
  (if (mpair? funciones)
      (mcons (function-coord-updater (mcar funciones) difference) (function-coord-updater (mcdr funciones) difference))
      (function-coord-updater funciones difference)
      )
  )

;Controla el drag and drop ya que se debe ver a un nivel mas alto para poder mover las dos cajas de la pareja
(define (detector-drag funciones mouse-click difference)
  (if (mpair? funciones)
      (if (and (mpair? (mcar funciones))
               (or (intersect? ((mcar (mcar funciones)) "coord") mouse-click)
                   (intersect? ((mcar (mcdr funciones)) "coord") mouse-click)))
          (coord-updater funciones difference)
          (mcons (detector-drag (mcar funciones) mouse-click difference) 
                 (detector-drag (mcdr funciones) mouse-click difference))
          )
      funciones
      )
  )


;;;;;;;;;;;;;;;;;;Canvas;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define my-canvas%
  (class canvas%
    (super-new)
    
    (inherit get-dc)
    (inherit get-view-start)
    (inherit refresh-now)
    (inherit make-bitmap)
    
    (init-field pairs)
    (init-field label)
    (init-field text-field-car)
    (init-field text-field-cdr)
    
    (field (click-event 'contract-expand))
    (field (diagram (draw pairs)))
    (field (last-click (new-coord 0 0)))
    (field (click (new-coord 0 0)))
    
    ;;;;;; Get coords with scrollbars reference
    (define (get-coord-scrollbars coord)
      (let-values (((init-point-x init-point-y) (get-view-start)))
        (move-coord coord init-point-x init-point-y)
      )
    )

    ;;;;;;Canvas events
    (define/override (on-event e)
      (let ((my-dc (get-dc))
            (event (send e get-event-type)))
        (cond ((equal? event 'left-down)
               (begin
                 (set! last-click (copy-coord click))
                 (set! click (get-coord-scrollbars (new-coord (send e get-x) (send e get-y))))
                 )
               )
              ((equal? event 'left-up)
                  (let ((event-coord (get-coord-scrollbars (new-coord (send e get-x) (send e get-y)))))
                    (send my-dc clear) ;Limpiamos el canvas y tras realizar el evento repintamos
                    (if (equal? event-coord click)
                        ((eval click-event) this diagram click last-click)
                        (let ((x (get-x event-coord))
                              (y (get-y event-coord)))
                          (set! diagram (detector-drag diagram click 
                                                            (cons (- (if (< x 0) 0 x) (mcar click))
                                                                  (- (if (< y 0) 0 y) (mcdr click)))))
                         )
                    )
                    (refresh-now)
                  )
               )
         )
       )
    )
   
    (define/override (on-paint)
      (paint-diagram diagram (get-dc))
      (send label set-label (~a pairs))
      )
    
    (define/public (get-lista) diagram)
    
    (define/public (get-car-child) (send text-field-car get-value))
    
    (define/public (get-cdr-child) (send text-field-cdr get-value))
    
    (define/public (set-event e) (set! click-event e))
    
    (define/public (reload)
      (set! diagram (draw pairs))
      (send (get-dc) clear)
      (refresh-now)
    )
    
    (define/public (new)
      (set! pairs (mcons " " " "))
      (set! diagram (draw pairs))
      (send (get-dc) clear)
      (refresh-now)
    )
    
    (define/public (save-as-image)
      (let ((coords (get-diagram-coords diagram)))
        (let ((bitmap (make-bitmap (cadr coords) (cddr coords))))
          (let ((dc (send bitmap make-dc)))
            (paint-diagram diagram dc)
            (send bitmap save-file "canvas.png" 'png)
            )
          )
        )
    )
    )
  )


;PRUEBAS
;(define lista-parejas (mcons (mcons 'a (mcons (mlist 1 2) 'b)) (mcons 1 2)))
;(set-mcdr! (mcar lista-parejas) (mcar lista-parejas))
;(set-mcar! (mcdr lista-parejas) lista-parejas)

(define lista-parejas (mcons (mcons 1 2) (mcons 3 4)))
;(define lista-parejas (mcons 1 2))
;(set-mcdr! lista-parejas lista-parejas)

;(define lista-parejas (mcons 'a (mcons 'b (mcons 'c null))))
;(set-mcdr! (mcdr (mcdr lista-parejas)) lista-parejas)

;Listas
;(define lista-parejas (mcons (mlist (mlist 'a 'b 'b) 2 3) (mlist 3 4 5 6)))

;Todos los tipos
;(define lista-parejas (mcons (mlist 'list 'en 'el 'car) (mcons (mcons (mlist 1 2 4) 1 ) (mlist 'list (mcons 1 (mcons 3 (mcons 3 1))) 'el 'cdr))))

;Inicializa el canvas
(define frame (new frame%
                   (label "Box and Pointer")
                   (width 800)
                   (height 600)))

(define h-panel-canvas (new horizontal-panel% 
                            [alignment (list 'left 'top)] 
                            [parent frame]
                            (stretchable-width #t)
                            [min-width 700]
                            [min-height 100]))

(define h-panel-childs (new horizontal-panel% 
                          [alignment (list 'left 'top)]
                          [stretchable-height #f]
                          [parent frame]
                          [stretchable-width #t]
                          [min-width 700]
                          [min-height 25]))

(send h-panel-childs show #f)

(define h-panel-text (new horizontal-panel% 
                          [alignment (list 'left 'top)]
                          [stretchable-height #f]
                          [parent frame]
                          [stretchable-width #t]
                          [min-width 700]
                          [min-height 25]))

(define label (new message%
                   [label "hola"]
                   [parent h-panel-text]
                   [auto-resize #t]))


(define text-field-car (new text-field%
                        (label "Car: ")
                        (parent h-panel-childs)))

(define text-field-cdr (new text-field%
                        (label "Cdr: ")
                        (parent h-panel-childs)))

(new button% [label "Bucle"]	 
   	 [parent h-panel-childs]	 
            [callback 
             (lambda (boton evento)
                        (send canvas set-event 'start-loop))])

(define canvas (new my-canvas% 
                    [pairs lista-parejas] 
                    [label label]
                    [text-field-car text-field-car]
                    [text-field-cdr text-field-cdr]
                    [style (list 'vscroll 'hscroll)]
                    [parent h-panel-canvas]))

(send canvas init-auto-scrollbars 800 600 0 0)

(define menu-bar (new menu-bar%	 
                      [parent frame]	 
                      ))

(define menu-archivo (new menu%
                  [label "Archivo"]
                  [parent menu-bar]))

(define menu-modo (new menu%
                  [label "Modo"]
                  [parent menu-bar]))

(new menu-item%
     [label "&Nuevo"]
     [parent menu-archivo]
     [callback (lambda (menu evento)
                        (send canvas new))])

(new menu-item%
     [label "&Refrescar"]
     [parent menu-archivo]
     [callback (lambda (menu evento)
                        (send canvas reload))])

(new menu-item%
     [label "&Guardar imagen"]
     [parent menu-archivo]
     [callback (lambda (menu evento) (send canvas save-as-image))])

(new menu-item%
     [label "&Añadir nuevos nodos"]
     [parent menu-modo]
     [callback (lambda (menu evento)
                        (send canvas set-event 'add-child)
                        (send h-panel-childs show #t))])

(new menu-item%
     [label "&Contrae / Expande"]
     [parent menu-modo]
     [callback (lambda (menu evento)
                        (send canvas set-event 'contract-expand)
                        (send h-panel-childs show #f))])

;(new editor-canvas% [parent v-panel-buttons])

(send frame show #t)

