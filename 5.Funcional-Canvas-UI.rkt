(require racket/draw)
(require racket/gui/base)
(require racket/format)

(define TAM 30)
(define MARGEN 50)
(define PADDING 5)

;Creación de la lista de funciones
(define (crea-lista-draw pareja coord mouse-click)
  (if (pair? pareja)
      (cons (crea-funcion (car pareja) coord #t mouse-click) (crea-funcion (cdr pareja) (cons (+ (car coord) TAM) (cdr coord)) #f mouse-click))
      (funcion-dato pareja coord)
  )
)

(define (intersectan? coord mouse-click) 
  (let ((x-ini (car coord))
       (y-ini (cdr coord))
       (x-fin (+ (car coord) TAM))
       (y-fin (+ (cdr coord) TAM))
       (x-click (car mouse-click))
       (y-click (cdr mouse-click)))
  (if (and (< x-click 0) (< y-click 0))
      #f
      (and (<= x-ini x-click) (<= y-ini y-click) (>= x-fin x-click) (>= y-fin y-click))
   )
  )
)

;Creadores de funciones
(define (crea-funcion dato coord es-car mouse-click)
  (let ((x (car coord))
        (y (cdr coord)))
  (if (pair? dato)     
      (lambda (dc msg)
        (let ((coordenada coord)
             (activa (not (intersectan? coord mouse-click))))
        (cond ((string=? msg "coord") coordenada)
              ((string=? msg "activa") activa)
              ((string=? msg "dibuja")
                 (if (and es-car)
                     (list 
                       (send dc draw-rectangle
                           x y       ; Top-left at (x, y), y pixels down from top-left
                           TAM TAM)  ; wide and high
                       (send dc draw-line
                           (+ x (/ TAM 2)) (+ y TAM)
                           (+ x (/ TAM 2)) (+ MARGEN y))
                     )
                     (list
                       (send dc draw-rectangle
                            x y       ; Top-left at (x, y), y pixels down from top-left
                            TAM TAM)  ; wide and high
                       (send dc draw-line
                            (+ x TAM ) (+ y (/ TAM 2))
                            (+ x MARGEN) (+ y (/ TAM 2)))
                      )
                  )
               )
               ((string=? msg "hijo")
                (if (and es-car) 
                    (crea-lista-draw dato (cons x (+ MARGEN y)) mouse-click)
                    (crea-lista-draw dato (cons (+ MARGEN x) y) mouse-click)
               ))
         )
         )
       )
  (crea-lista-draw dato coord mouse-click)
  )
 )
)

(define (funcion-dato dato coord)
  (lambda (dc msg)
        (let ((activa #t)
              (x (car coord))
              (y (cdr coord))
              )
          (cond ((string=? msg "coord") coord)
                ((string=? msg "activa") activa)
                ((string=? msg "dibuja")
                 (list
                  (send dc draw-rectangle
                      x y    ; Top-left at (x, y), y pixels down from top-left
                      TAM TAM) ; wide and high
                  (send dc draw-text (~a dato) (+ x PADDING) (+ y PADDING))
                  )
                )
          )
        )
   )
)

(define (funcion-expande coord)
  (lambda (dc msg)
        (let ((activa #t)
              (x (car coord))
              (y (cdr coord))
              )
          (cond ((string=? msg "coord") coord)
                ((string=? msg "activa") activa)
                ((string=? msg "dibuja")
                 (list
                  (send dc draw-rectangle
                      x y    ; Top-left at (x, y), y pixels down from top-left
                      TAM TAM) ; wide and high
                  (send dc draw-text "+" (+ x PADDING) (+ y PADDING))
                  )
                )
          )
        )
   )
)

;Evaluador de la lista de funciones
(define (evaluador funcion dc)
  (if (funcion dc "activa")
      (cons (funcion dc "dibuja")
            (pinta-lista (funcion dc "hijo") dc))
      ((funcion-expande (funcion dc "coord")) dc "dibuja")
  )
)

(define (pinta-lista funciones dc)
  (if (pair? funciones)
      (cons (evaluador (car funciones) dc) (evaluador (cdr funciones) dc))
  )
)

;Detector de colisión

;Sentencias canvas 
(define frame (new frame%
                   [label "Box and Pointer"]
                   [width 300]
                   [height 300]))

(define my-canvas%
  (class canvas%
    (super-new)
    (inherit get-dc)
    
    (define/override (on-event e)
           (if (equal? (send e get-event-type) 'left-down)
               (let ([my-dc (get-dc)])
                 (send my-dc clear)
                 (pinta-lista (crea-lista-draw lista (cons 0 0) (cons (send e get-x) (send e get-y))) my-dc)
                )
             )
     )   
   )
 )

(define lista (cons (cons (cons "hola" 1) (cons #t (cons 'a 3))) (cons 2 (cons '() #\A))))
(define listaFunc (crea-lista-draw lista (cons 0 0) (cons -1 -1)))
(new my-canvas% [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (pinta-lista listaFunc dc))]
)
(send frame show #t)