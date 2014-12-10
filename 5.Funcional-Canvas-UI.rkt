(require racket/draw)
(require racket/gui/base)
(require racket/format)

(define TAM 30)
(define MARGEN 50)
(define PADDING 5)

;Creación de la lista de funciones: visible siempre a true en el inicio y genera coordenadas
(define (lista-draw pareja coord)
  (if (pair? pareja)
      (cons (llama-creador (car pareja) coord "car" #t) (llama-creador (cdr pareja) (cons (+ (car coord) TAM) (cdr coord)) "cdr" #t))
      (crea-funcion pareja coord "dato" #t)
  )
)

(define (llama-creador dato coord tipo visible)
     (cons (crea-funcion dato coord tipo visible)
           (let ((x (car coord))
                 (y (cdr coord)))
             (if (pair? dato)
                 (cond ((string=? tipo "car") (lista-draw dato (cons x (+ MARGEN y))))
                       ((string=? tipo "cdr") (lista-draw dato (cons (+ MARGEN x) y)))
                       )
                 (lista-draw dato coord)
              )
           )
      )         
  )

;Creador de funciones
(define (crea-funcion dato coord tipo visible)
  (let ((x (car coord))
        (y (cdr coord)))
     (lambda (dc msg)
       (cond ((string=? msg "dato") dato)
             ((string=? msg "coord") coord)
             ((string=? msg "tipo") tipo)
             ((string=? msg "visible") visible)
             ((string=? msg "dibuja")
              (if visible
                  (if (string=? tipo "dato")
                      (list (send dc draw-text (~a dato) (+ x PADDING) (+ y PADDING)))
                      (list 
                       (send dc draw-rectangle
                             x y       ; Top-left at (x, y), y pixels down from top-left
                             TAM TAM)  ; wide and high
                       (if (pair? dato)
                           (cond ((string=? tipo "car")
                                  (send dc draw-line
                                        (+ x (/ TAM 2)) (+ y TAM)
                                        (+ x (/ TAM 2)) (+ MARGEN y))
                                  )
                                 ((string=? tipo "cdr")
                                  (send dc draw-line
                                        (+ x TAM ) (+ y (/ TAM 2))
                                        (+ x MARGEN) (+ y (/ TAM 2)))
                                  )
                                 )
                           )
                       )
                   ) ;Cuando no es visible mostramos el indicador para que se expanda
                  (list
                   (send dc draw-rectangle
                         x y       
                         TAM TAM)  
                   (send dc draw-text (~a "+") (+ x PADDING) (+ y PADDING))
                  )
             )
           )
       )
     )
  )
)

;Detector de colisión
(define (intersectan? coord mouse-click) 
  (let ((x-ini (car coord))
       (y-ini (cdr coord))
       (x-fin (+ (car coord) TAM))
       (y-fin (+ (cdr coord) TAM))
       (x-click (car mouse-click))
       (y-click (cdr mouse-click)))
   (and (<= x-ini x-click) (<= y-ini y-click) (>= x-fin x-click) (>= y-fin y-click))
  )
)

(define (detecta-colision funciones mouse-click dc)
  (if (pair? (car funciones))
      (detector-colisiones funciones mouse-click dc)
      (let ((funcion (car funciones)))
        (if (intersectan? (funcion dc "coord") mouse-click)
            (cons (crea-funcion (funcion dc "dato") (funcion dc "coord") (funcion dc "tipo") (not (funcion dc "visible")))
                  (propaga-colision (cdr funciones) (not (funcion dc "visible")) dc)
            )
            (cons (crea-funcion (funcion dc "dato") (funcion dc "coord") (funcion dc "tipo") (funcion dc "visible"))
                  (detector-colisiones (cdr funciones) mouse-click dc)
            )
        )
      )
   )
 )
      
(define (propaga-colision funciones visible dc)
  (if (pair? funciones)
      (cons (propaga-colision (car funciones) visible dc) (propaga-colision (cdr funciones) visible dc))
      (crea-funcion (funciones dc "dato") (funciones dc "coord") (funciones dc "tipo") visible)
   )
)

(define (detector-colisiones funciones mouse-click dc)
  (if (pair? funciones)
      (cons (detecta-colision (car funciones) mouse-click dc) (detecta-colision (cdr funciones) mouse-click dc))
      funciones
   )
 )


;Pinta una lista de funciones
(define (pinta funciones dc)
  (if (pair? (car funciones))
      (pinta-lista funciones)
      (if ((car funciones) dc "visible")
          (cons ((car funciones) dc "dibuja") (pinta-lista (cdr funciones) dc))
          ((car funciones) dc "dibuja")
      )
   )
)

(define (pinta-lista funciones dc)
  (if (pair? funciones)
      (cons (pinta (car funciones) dc) (pinta (cdr funciones) dc))
      (funciones dc "dibuja")
  )
)


;Sentencias canvas 
(define my-canvas%
  (class canvas%
    (super-new)
    
    (inherit get-dc)
    (inherit refresh-now)
    
    (init-field pairs)
    (field [lista-funciones (lista-draw pairs (cons 0 0))])
    
    (define/override (on-event e)
           (if (equal? (send e get-event-type) 'left-down)
               (let ([my-dc (get-dc)])
                 (send my-dc clear)
                 (set! lista-funciones (detector-colisiones lista-funciones (cons (send e get-x) (send e get-y)) my-dc))
                 (refresh-now)
                )
             )
     )
    
    (define/override (on-paint)
        (pinta-lista lista-funciones (get-dc))
    )
   )
 )

(define lista-parejas (cons (cons "p" (cons 3 4)) (cons 1 2)))

(define frame (new frame%
                   [label "Box and Pointer"]
                   [width 300]
                   [height 300]))
(new my-canvas% [pairs lista-parejas] [parent frame])   
(send frame show #t)


;aux
(define (print-pareja pareja)
    (if (pair? pareja)
	    (begin 
		    (display "(")
            (print-dato (car pareja))
            (display " . ")
            (print-dato (cdr pareja))
            (display ")"))))

(define (print-dato dato)
    (if (pair? dato)
        (print-pareja dato)
        (display dato)))