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

;;Debe ser un pair, si es una lista dejamos de contar ya que se dibujará hacia la derecha.
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
    (mcons (function-creator data coord type #f #t ancestors) ;Dibujamos la caja
           ;Si el hijo es una pareja movemos las coordenadas sino pintamos directamente dentro
           (if (mpair? ((eval type) data))
               ;Si es una pareja miramos los bucles
               (let ((cycle-data (cycle-finder ((eval type) data) ancestors)))
                 (if (car cycle-data)
                     (function-creator data coord type #t #t ancestors) ;Si es un ciclo se envía como tipo dato y pintará la linea al antecesor
                     (list-creator ((eval type) data) (coord-locator data coord type) ancestors) ;Recursion
                     )
                 )
               (function-creator data coord type #t #t ancestors)
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

;Creador de funciones
(define (function-creator pair coord tipo es-dato visible ancestors)
  (let ((parent-coord (if (null? (cdr ancestors)) '() (cdadr ancestors)))
        (old-data (~a ((eval tipo) pair)))) ;Guardamos el dato que contiene como cadena para que no nos afecten las mutaciones
    (lambda (dc msg)
      (cond ((string=? msg "dato") pair)
            ((string=? msg "cambio") (not (equal? old-data (~a ((eval tipo) pair))))) ;Para saber si ha habido una mutación comparamos con la cadena anterior
            ((string=? msg "coord") coord)
            ((string=? msg "parent-coord") parent-coord)
            ((string=? msg "tipo") tipo)
            ((string=? msg "visible") visible)
            ((string=? msg "es-dato") es-dato)
            ((string=? msg "ancestors") ancestors)
            ((string=? msg "dibuja")
             (let ((x (get-x coord))
                   (y (get-y coord))
                   (dato ((eval tipo) pair))
                   (cycle (cycle-finder ((eval tipo) pair) ancestors)))
               (if es-dato
                   (if (car cycle) 
                       ;Si en vez de dato tenemos un ciclo, debemos pintar la linea
                       (draw-cycle coord tipo (cdr cycle) dc)                              
                       (send dc draw-text (~a dato) (+ x PADDING) (+ y PADDING)) ;Dato simple
                       )
                   (begin 
                     (send dc draw-rectangle
                           x y       ; Top-left at (x, y), y pixels down from top-left
                           SIZE SIZE)  ; wide and high
                     (if (and (mpair? parent-coord) (eq? tipo 'mcar)) ;Pintamos las lineas hacia el padre, se encargará la parte car
                         (if (and (> x (+ (mcar parent-coord) SIZE)) (< y (+ (mcdr parent-coord) SIZE)))
                             (send dc draw-line
                                   (+ (mcar parent-coord) SIZE ) (+ (mcdr parent-coord) (/ SIZE 2))
                                   x (+ y (/ SIZE 2)))
                             (send dc draw-line
                                   (+ (mcar parent-coord) (/ SIZE 2)) (+ (mcdr parent-coord) SIZE)
                                   (+ x SIZE) y)
                             )
                         )   
                     (if (not visible)
                         ;Cuando no es visible mostramos el indicador para que se expanda
                         (send dc draw-text (~a "+") (+ x PADDING) (+ y PADDING))
                         )
                     )
                   )
               )
             )
            )
      )
    )
  )


;Llama al creador de funciones para representar los cambios realizados, calcula las nuevas funciones
(define (change-detector funcion dc)
  (list-recursion (funcion dc "dato") (funcion dc "coord") (funcion dc "tipo") (cdr (funcion dc "ancestors")))
  )

;Detecta las colisiones y mutaciones al repintar el canvas
(define (detecta-colision funciones mouse-click dc)     
  (let ((funcion (mcar funciones)))
    (if (funcion dc "cambio") ;Si hay un cambio crea las nuevas funciones y sigue detectando la colisión
        (detecta-colision (change-detector funcion dc) mouse-click dc)
        (if (intersectan? (funcion dc "coord") mouse-click) ;Si no, comprueba si hay intersección
            (if (mpair? ((eval (funcion dc "tipo")) (funcion dc "dato"))) 
                ;Cambia el estado anterior de la visibilidad al contrario en caso de intersección
                (mcons (function-creator (funcion dc "dato") (funcion dc "coord") (funcion dc "tipo") 
                                         (funcion dc "es-dato") (not (funcion dc "visible")) (funcion dc "ancestors"))
                       (mcdr funciones))
                funciones ;Datos simples no se esconden, dejamos la lista tal cual
                )
            (mcons funcion (detector-colisiones-mutaciones (mcdr funciones) mouse-click dc)) ;si no intersecta se deja tal cual y seguimos iterando
            )
        )
    )
  )

(define (detector-colisiones-mutaciones funciones mouse-click dc)
  (if (mpair? funciones)
      (mcons (detecta-colision (mcar funciones) mouse-click dc) 
             (detecta-colision (mcdr funciones) mouse-click dc))
      funciones
      )
  )


;;;;;;;;;;;;;;;;;Callbacks de cada modo de trabajo;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;Contract-expand
(define (contract-expand canvas lista-funciones click . args)
  (let ((funciones (search-selected-pair lista-funciones click (send canvas get-dc))))
  (if (not (eq? funciones #f)) 
      (let ((dc (send canvas get-dc))
            (funcion (mcar funciones)))
        (if (mpair? ((eval (funcion dc "tipo")) (funcion dc "dato"))) 
            ;Cambia el estado anterior de la visibilidad al contrario en caso de intersección
            (set-mcar! funciones (function-creator (funcion dc "dato") (funcion dc "coord") (funcion dc "tipo") 
                                                     (funcion dc "es-dato") (not (funcion dc "visible")) (funcion dc "ancestors"))
                                   )
            ;Datos simples no se esconden, dejamos la lista tal cual
            )
        )
      )
    )
  )

;;;;;;;;;;;;;add-child: Añade un hijo vacío a una caja
(define (add-child canvas lista-funciones click . args)
  (let ((funciones (search-selected-pair lista-funciones click (send canvas get-dc))))
    (if (not (eq? funciones #f))
        (create-child canvas funciones (mcons (send canvas get-car-child) (send canvas get-cdr-child)) #f)
    )
  )
)

(define (create-child canvas functions child loop)
  (let ((dc (send canvas get-dc))
        (function (mcar functions)))
    (let ((dato (function dc "dato")))
        (begin 
          ((eval (string->symbol (string-append "set-" (symbol->string (function dc "tipo")) "!"))) dato child)
          (set-mcar! functions (function-creator (function dc "dato") (function dc "coord") (function dc "tipo") 
                                               #f (function dc "visible") (function dc "ancestors")))
          (if (not loop)
              (set-mcdr! functions (list-creator child (coord-locator  child (function dc "coord") (function dc "tipo")) (function dc "ancestors")))
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

(define (end-loop canvas lista-funciones click . args)
  (let ((dc (send canvas get-dc))) 
    (let ((origen (search-selected-pair lista-funciones (car args) dc))
          (destino (search-selected-pair lista-funciones click dc)))
      (if (and (not (eq? origen #f)) (not (eq? destino #f)))
          (create-child canvas origen ((mcar destino) dc "dato") #t)
          )
      )
  )
  (send canvas set-event 'add-child)
)

;;;;;;;;;;;;Común: antes de realizar acciones buscamos sobre la pareja que se ha hecho click y la devolvemos
;Busca el elemento sobre el que hemos hecho click
(define (recursive-search funciones mouse-click dc)     
  (let ((funcion (mcar funciones)))
    (if (intersect? (funcion dc "coord") mouse-click)
        funciones
        (search-selected-pair (mcdr funciones) mouse-click dc)
        )
    )
  )

(define (search-selected-pair funciones mouse-click dc)
  (if (mpair? funciones)
      (let ((search-cdr (recursive-search (mcdr funciones) mouse-click dc)))
        (if (eq? search-cdr #f)
            (recursive-search (mcar funciones) mouse-click dc)
            search-cdr
            )
        )
      #f
      )
  )


;;;;;;;;;;;;;;;;Eventos de drag and drop;;;;;;;;;;;;;;;;;;;;;;;;
(define (function-coord-updater funciones difference dc)
  (if (mpair? funciones)
      (coord-updater funciones difference dc)
      (if (funciones dc "es-dato")
          funciones
          (let ((orig-coord (funciones dc "coord")))
            (begin (set-mcar! orig-coord (+ (mcar orig-coord) (car difference))) 
                   (set-mcdr! orig-coord (+ (mcdr orig-coord) (cdr difference)))
                   funciones
                   )
            )
          )
      )
  )

(define (coord-updater funciones difference dc)
  (if (mpair? funciones)
      (mcons (function-coord-updater (mcar funciones) difference dc) (function-coord-updater (mcdr funciones) difference dc))
      (function-coord-updater funciones difference dc)
      )
  )

;Controla el drag and drop ya que se debe ver a un nivel mas alto para poder mover las dos cajas de la pareja
(define (detector-drag funciones mouse-click difference dc)
  (if (mpair? funciones)
      (if (and (mpair? (mcar funciones))
               (or (intersect? ((mcar (mcar funciones)) dc "coord") mouse-click)
                   (intersect? ((mcar (mcdr funciones)) dc "coord") mouse-click)))
          (coord-updater funciones difference dc)
          (mcons (detector-drag (mcar funciones) mouse-click difference dc) 
                 (detector-drag (mcdr funciones) mouse-click difference dc))
          )
      funciones
      )
  )


;;;;;;;;;;;;;;;;;;Canvas;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define my-canvas%
  (class canvas%
    (super-new)
    
    (inherit get-dc)
    (inherit refresh-now)
    
    (init-field pairs)
    (init-field label)
    (init-field text-field-car)
    (init-field text-field-cdr)
    
    (field (click-event 'contract-expand))
    (field (lista-funciones (draw pairs)))
    (field (last-click (new-coord 0 0)))
    (field (click (new-coord 0 0)))
    
    ;Cuando hacemos click en una caja
    (define/override (on-event e)
      (let ((my-dc (get-dc))
            (event (send e get-event-type)))
        (cond ((equal? event 'left-down) 
               (begin
                 (set! last-click (copy-coord click))
                 (set! click (new-coord (send e get-x) (send e get-y)))
                 )
               )
              ((equal? event 'left-up)
               (begin 
                 (send my-dc clear) ;Limpiamos el canvas y tras realizar el evento repintamos
                 (if (equal? (new-coord (send e get-x) (send e get-y)) click)
                     ((eval click-event) this lista-funciones click last-click)
                     (set! lista-funciones  (detector-drag lista-funciones click 
                                                           (cons (- (send e get-x) (mcar click))
                                                                 (- (send e get-y) (mcdr click))) my-dc))
                     )
                 (refresh-now)
                 )
               )
              )
        )
      )
    
    (define/override (on-paint)
      (paint-list lista-funciones (get-dc))
      (send label set-label (~a pairs))
      )
    
    (define/public (get-lista) lista-funciones)
    
    (define/public (get-car-child) (send text-field-car get-value))
    
    (define/public (get-cdr-child) (send text-field-cdr get-value))
    
    (define/public (set-event e) (set! click-event e))
  )
  )


;PRUEBAS
(define lista-parejas (mcons (mcons 'a (mcons (mlist 1 2) 'b)) (mcons 1 2)))
(set-mcdr! (mcar lista-parejas) (mcar lista-parejas))
(set-mcar! (mcdr lista-parejas) lista-parejas)

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

(define menu-bar (new menu-bar%	 
                      [parent frame]	 
                      ))

(define menu (new menu%
                  [label "Archivo"]
                  [parent menu-bar]))

(define menu (new menu%
                  [label "Modo"]
                  [parent menu-bar]))

(new menu-item%
     [label "&Añadir nuevos nodos"]
     [parent menu]
     [callback (lambda (menu evento)
                        (send canvas set-event 'add-child)
                        (send h-panel-childs show #t))])

(new menu-item%
     [label "&Contrae / Expande"]
     [parent menu]
     [callback (lambda (menu evento)
                        (send canvas set-event 'contract-expand)
                        (send h-panel-childs show #f))])

;(new editor-canvas% [parent v-panel-buttons])

(send frame show #t)