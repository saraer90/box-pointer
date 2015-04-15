(#%require  racket/draw)
(#%require  racket/gui/base)
(#%require  racket/format)
(#%require compatibility/mlist)

(require "lib/utils.rkt")
(require "lib/settings.rkt")

;Creación de la lista de funciones: visible siempre a true en el inicio
;Añade margen superior e izquierdo para poder dibujar las lineas de los ciclos en caso de que fuese necesario
(define (draw list) (list-creator list (if (mlist? list)
                                           (mcons CYCLE-MARGIN CYCLE-MARGIN)
                                           (mcons (+ (* MARGEN (car-levels list)) CYCLE-MARGIN) CYCLE-MARGIN)) '()))

;Calcula cuantos niveles hay en cuanto a la parte car de las parejas, para calcular el margen izquierdo
(define (car-levels list)
  (count-car-levels list -1 '())
  )

(define (count-car-levels list levels ancestors)
  (if (and (mpair? list) (not (mlist? list)) (not (car (cycle-finder list ancestors))))
      (count-car-levels (mcar list) (+ levels 1) (cons (cons list ancestors) '())) ;No tenemos coordenadas, para reutilizar el metodo añadimos una pareja vacía
      levels
      )
  )

;Recorrido previo de la lista de parejas para preparar las funciones y sus coordenadas
(define (list-creator pair coord ancestors)
  (if (mpair? pair)
      (mcons (list-recursion pair coord 'mcar ancestors) 
             (list-recursion pair (mcons (+ (mcar coord) TAM) (mcdr coord)) 'mcdr ancestors) 
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
      (let ((x (mcar coord))
            (y (mcdr coord)))
        (if (mlist? data)
            (cond ((eq? type 'mcar) (mcons x (+ MARGEN y)))  ;Cuando se trate de una lista descendemos verticalmente en linea en caso de car
                  ((eq? type 'mcdr) (mcons (+ MARGEN x) y))  ;y si es cdr lo haremos horizontalmente hacia la derecha
                  )
            (cond ((eq? type 'mcar) 
                   (if (mpair? (mcdr data)) (mcons (- x MARGEN) (+ MARGEN y)) (mcons (- x (/ TAM 2)) (+ MARGEN y))))
                  ((eq? type 'mcdr)
                   (if (mpair? (mcar data)) (mcons (+ (/ MARGEN 2) x) (+ MARGEN y)) (mcons (- x (/ TAM 2)) (+ MARGEN y))))
                  ) ;Si no tenemos una lista, comprobaremos que tipo de dato hay en la otra parte de la pareja
            )       ;si tambien es una pareja pondremos las flechas en diagonal, si no en vertical
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

;Pinta linea de bucles
;Final de la linea
(define (draw-cycle-ending coord cycle dc)
  (let ((x-origin (car coord))
        (y-origin (cdr coord))
        (x-dest (mcar cycle))
        (y-dest (mcdr cycle)))
    (send dc draw-line
          x-origin y-origin
          x-origin (- y-dest CYCLE-MARGIN))
    (send dc draw-line
          x-origin (- y-dest CYCLE-MARGIN)
          x-dest (- y-dest CYCLE-MARGIN))
    (send dc draw-line
          x-dest (- y-dest CYCLE-MARGIN)
          x-dest y-dest)
    )
  )

;Inicio de la linea
(define (draw-cycle coord tipo cycle dc)
  (let ((original-pen (send dc get-pen))
        (x-origin (mcar coord))
        (y-origin (mcdr coord))
        (x-dest (mcar cycle))
        (y-dest (mcdr cycle))
        )
    (send dc set-pen (new pen% [color "red"])) ;[style 'long-dash]
    (if (eq? tipo 'mcdr)
        (begin (send dc draw-line
                     (+ x-origin TAM) (+ y-origin (/ TAM 2)) ;Start at box' right-center
                     (+ (+ x-origin TAM) CYCLE-MARGIN) (+ y-origin (/ TAM 2)))
               (draw-cycle-ending (cons (+ (+ x-origin TAM) CYCLE-MARGIN) (+ y-origin (/ TAM 2))) cycle dc)
               )
        (begin (send dc draw-line
                     x-origin (+ y-origin (/ TAM 2))  ;Start at box' left-center
                     (- x-origin CYCLE-MARGIN) (+ y-origin (/ TAM 2)))
               (draw-cycle-ending (cons (- x-origin CYCLE-MARGIN) (+ y-origin (/ TAM 2))) cycle dc)
               )
        )
    ;Common
    (send dc set-pen original-pen)
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
             (let ((x (mcar coord))
                   (y (mcdr coord))
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
                           TAM TAM)  ; wide and high
                     (if (and (mpair? parent-coord) (eq? tipo 'mcar)) ;Pintamos las lineas hacia el padre, se encargará la parte car
                         (if (and (> x (+ (mcar parent-coord) TAM)) (< y (+ (mcdr parent-coord) TAM)))
                             (send dc draw-line
                                   (+ (mcar parent-coord) TAM ) (+ (mcdr parent-coord) (/ TAM 2))
                                   x (+ y (/ TAM 2)))
                             (send dc draw-line
                                   (+ (mcar parent-coord) (/ TAM 2)) (+ (mcdr parent-coord) TAM)
                                   (+ x TAM) y)
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

;Detector de colisión
(define (intersectan? coord mouse-click) 
  (let ((x-ini (mcar coord))
        (y-ini (mcdr coord))
        (x-fin (+ (mcar coord) TAM))
        (y-fin (+ (mcdr coord) TAM))
        (x-click (car mouse-click))
        (y-click (cdr mouse-click)))
    (and (<= x-ini x-click) (<= y-ini y-click) (>= x-fin x-click) (>= y-fin y-click))
    )
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

;Eventos de drag and drop
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
               (or (intersectan? ((mcar (mcar funciones)) dc "coord") mouse-click)
                   (intersectan? ((mcar (mcdr funciones)) dc "coord") mouse-click)))
          (coord-updater funciones difference dc)
          (mcons (detector-drag (mcar funciones) mouse-click difference dc) 
                 (detector-drag (mcdr funciones) mouse-click difference dc))
          )
      funciones
      )
  )

;Pinta la lista de funciones llamando a dibuja
;Si encuentra una caja no visible deja de iterar y no pinta sus hijos
(define (pinta funciones dc)
  (if (mpair? (mcar funciones))
      (pinta-lista funciones dc)
      (if ((mcar funciones) dc "visible")
          (cons ((mcar funciones) dc "dibuja") (pinta-lista (mcdr funciones) dc))
          ((mcar funciones) dc "dibuja")
          )
      )
  )

(define (pinta-lista funciones dc)
  (if (mpair? funciones)
      (cons (pinta (mcar funciones) dc) (pinta (mcdr funciones) dc))
      (funciones dc "dibuja")
      )
  )

;Clase canvas 
(define my-canvas%
  (class canvas%
    (super-new)
    
    (inherit get-dc)
    (inherit refresh-now)
    
    (init-field pairs)
    (field (lista-funciones (draw pairs)))
    (field (click '()))
    
    ;Cuando hacemos click en una caja
    (define/override (on-event e)
      (let ((my-dc (get-dc))
            (event (send e get-event-type)))
        (cond ((equal? event 'left-down) 
               (set! click (cons (send e get-x) (send e get-y))))
              ((equal? event 'left-up)
               (begin 
                 (send my-dc clear) ;Limpiamos el canvas y tras detectar las colisiones/mutaciones repintamos
                 (set! lista-funciones 
                       (if (equal? (cons (send e get-x) (send e get-y)) click)
                           (detector-colisiones-mutaciones lista-funciones click my-dc)
                           (detector-drag lista-funciones click 
                                          (cons (- (send e get-x) (car click))
                                                (- (send e get-y) (cdr click))) my-dc)
                           ))
                 (refresh-now)
                 )
               )
              )
        )
      )
    
    (define/override (on-paint)
      (pinta-lista lista-funciones (get-dc))
      )
    
    (define/public (get-lista) lista-funciones)
    )
  )

;PRUEBAS
(define lista-parejas (mcons (mcons 'a (mcons (mlist 1 2) 'b)) (mcons 1 2)))
;(set-mcdr! (mcar lista-parejas) (mcar lista-parejas))
(set-mcar! (mcdr lista-parejas) lista-parejas)

;(define lista-parejas (mcons 1 2))
;(set-mcdr! lista-parejas lista-parejas)

;(define x (mcons 'a (mcons 'b (mcons 'c null))))
;(set-mcdr! (mcdr (mcdr x)) x)


;Inicializa el canvas
(define frame (new frame%
                   (label "Box and Pointer")
                   (width 300)
                   (height 300)))
(define canvas (new my-canvas% (pairs lista-parejas) (parent frame)))
(send frame show #t)