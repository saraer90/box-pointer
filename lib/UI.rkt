#lang racket
(#%require  racket/gui/base)
(require "draw.rkt")
(require "diagram.rkt")
(require "coordinates.rkt")

(provide show-ui)
(provide menu-bar)
(provide canvas)
(provide h-panel-events)

;;Window: frame
(define frame (new frame%
                   (label "Box and Pointer")
                   (width 800)
                   (height 600)))

;;Panels
(define h-panel-canvas (new horizontal-panel% 
                            [alignment (list 'left 'top)] 
                            [parent frame]
                            (stretchable-width #t)
                            [min-width 700]
                            [min-height 100]))

(define h-panel-events (new horizontal-panel% 
                          [alignment (list 'left 'top)]
                          [stretchable-height #f]
                          [parent frame]
                          [stretchable-width #t]
                          [min-width 700]
                          [min-height 25]))

(send h-panel-events show #f)

(define h-panel-text (new horizontal-panel% 
                          [alignment (list 'left 'top)]
                          [stretchable-height #f]
                          [parent frame]
                          [stretchable-width #t]
                          [min-width 700]
                          [min-height 25]))

;;Text-fields
(define text-field-diagram (new text-field%
                                (label "")
                                (parent h-panel-text)
                                (style (list 'multiple))))

;;Menu
(define menu-bar (new menu-bar%	 
                      [parent frame]	 
                      ))

(define menu-archivo (new menu%
                  [label "&Archivo"]
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


;;;;;;;;;;;;;;;;;;Canvas;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define my-canvas%
  (class canvas%
    (super-new)
    
    (inherit get-dc)
    (inherit get-view-start)
    (inherit refresh-now)
    (inherit make-bitmap)
    
    (field (pairs (mcons " " " ")))
    (field (click-event null))
    (field (diagram (new-diagram pairs)))
    (field (last-click (new-coord 0 0)))
    (field (click (new-coord 0 0)))
    (field (editor-text-diagram (send text-field-diagram get-editor)))
    (field (my-dc (get-dc)))
    
    
    ;;;;;; Get coords with scrollbars reference
    (define (get-coord-scrollbars coord)
      (let-values (((init-point-x init-point-y) (get-view-start)))
        (move-coord coord init-point-x init-point-y)
      )
    )
    
    ;;;;;;;;;;;;;;;;;;;;;;CANVAS EVENTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/override (on-paint)
      (paint-diagram diagram (get-dc))
      (send editor-text-diagram lock #f)
      (send text-field-diagram set-value (~a pairs))
      (send editor-text-diagram lock #t)
      )

    (define/override (on-event e)
      (let ((event (send e get-event-type)))
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
                        (if (not (eq? click-event null))
                            ((eval click-event) this diagram click last-click)
                            null
                        )
                        (let ((x (get-x event-coord))
                              (y (get-y event-coord)))
                          (set! diagram (move-diagram diagram click 
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
   
    ;;;;PUBLIC METHODS    
    (define/public (get-diagram) diagram)
    
    (define/public (get-pairs) pairs)
    
    (define/public (set-pairs new-pairs)
      (set! pairs new-pairs)
      (set! diagram (new-diagram pairs))
      (send (get-dc) clear)
      (refresh-now)
    )
    
    (define/public (set-event e) (set! click-event e))
    
    (define/public (reload)
      (set! diagram (new-diagram pairs))
      (send (get-dc) clear)+
      (refresh-now)
    )
    
    (define/public (new)
      (set! pairs (mcons " " " "))
      (set! diagram (new-diagram pairs))
      (send (get-dc) clear)
      (refresh-now)
    )
    
    (define/public (save-as-image)
      (let ((coords (get-diagram-coords diagram)))
        (let ((bitmap (make-bitmap (cadr coords) (cddr coords))))
          (let ((dc (send bitmap make-dc))
                (pathfile (put-file #:message: "Guardar box&pointer" #:filters (list "*.png"))))
            (paint-diagram diagram dc)
            (send bitmap save-file pathfile 'png)
            )
          )
        )
    )
    )
  )

(define canvas (new my-canvas% 
                    [style (list 'vscroll 'hscroll)]
                    [parent h-panel-canvas]))

(send canvas init-auto-scrollbars 800 600 0 0)


(define (show-ui)
  (send frame show #t)
)