(require racket/draw)
(require racket/gui/base)
(require racket/format)

(define TAM 30)
(define MARGEN 50)
(define PADDING 5)

;Creación de la lista de funciones: visible siempre a true en el inicio y genera coordenadas
(define (crea-lista-draw pareja coord)
  (if (pair? pareja)
      (cons (llama-creador (car pareja) coord "car" #t) (llama-creador (cdr pareja) (cons (+ (car coord) TAM) (cdr coord)) "cdr" #t))
      (llama-creador pareja coord "dato" #t)
  )
)

(define (llama-creador dato coord tipo visible)
  (if (string=? tipo "dato")
     (crea-funcion dato coord tipo visible)
     (cons (crea-funcion dato coord tipo visible)
           (let ((x (car coord))
                 (y (cdr coord)))
             (if (pair? dato)
                 (cond ((string=? tipo "car") (crea-lista-draw dato (cons x (+ MARGEN y))))
                       ((string=? tipo "cdr") (crea-lista-draw dato (cons (+ MARGEN x) y)))
                       )
                 (crea-lista-draw dato coord)
              )
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
                   )
                  (list
                   (send dc draw-rectangle
                         x y       
                         TAM TAM)  
                   (send dc draw-text (~a "exp.") (+ x PADDING) (+ y PADDING))
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
               (let ([my-dc (get-dc)]
                     (old-lista-func listaFunc))
                 (send my-dc clear)
                 (define listaFunc (detector-colisiones old-lista-func (cons (send e get-x) (send e get-y)) my-dc))
                 (pinta-lista listaFunc my-dc)
                )
             )
     )   
   )
 )

(define lista (cons (cons "p" 1) (cons 1 2)))
(define listaFunc (crea-lista-draw lista (cons 0 0)))
(new my-canvas% [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (pinta-lista listaFunc dc))]
)
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