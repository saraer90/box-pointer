#lang racket
(#%require  racket/gui/base)
(require "draw.rkt")
(require "diagram.rkt")
(require "coordinates.rkt")
(require "settings.rkt")
(require "utils.rkt")

(provide show-ui)
(provide menu-bar)
(provide canvas)
(provide h-panel-events)

(define font-size (truncate (/ SIZE MAX-CONTENT)))
(define font (make-object font% font-size 'default 'normal 'normal #f 'default #t 'aligned))

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
(define feedback-text (new text-field%
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

(new menu-item%
     [label "&Guardar definición"]
     [parent menu-archivo]
     [callback (lambda (menu evento) (send canvas save-definition))])


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
    (field (click (new-coord 0 0)))
    (field (editor-text-diagram (send feedback-text get-editor)))
    (field (my-dc (get-dc)))
    
    
    ;;;;;; Get coords with scrollbars reference
    (define (get-coord-scrollbars coord)
      (let-values (((init-point-x init-point-y) (get-view-start)))
        (move-coord coord init-point-x init-point-y)
      )
    )
    
    (define (show-exception e)
     (let ((error-message (exn-message e)))
       (show-info error-message)
     )
    )
    
    ;;;;;;;;;;;;;;;;;;;;;;CANVAS EVENTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/override (on-paint)
      (send my-dc clear)
      (paint-diagram diagram (get-dc))
      (show-info (string-append (~a pairs) "\n" (get-diagram-string diagram)))
      )

    (define/override (on-event e)
      (let ((event (send e get-event-type)))
        (cond ((equal? event 'left-down)
               (begin
                 (set! click (get-coord-scrollbars (new-coord (send e get-x) (send e get-y))))
                 )
               )
              ((equal? event 'left-up)
                  (with-handlers
                      ([exn:fail:user? show-exception])
                      (let ((event-coord (get-coord-scrollbars (new-coord (send e get-x) (send e get-y)))))
                        (if (equal? event-coord click)
                            (if (not (eq? click-event null))
                                ((eval click-event) this diagram click)
                                null
                                )
                            (let ((x (get-x event-coord))
                                  (y (get-y event-coord)))
                              (move-diagram diagram click (cons (- (if (< x 0) 0 x) (mcar click))
                                                                (- (if (< y 0) 0 y) (mcdr click))))
                              (refresh-now)
                              )
                            )
                        )
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
      (refresh-now)
    )
    
    (define/public (set-event e) (set! click-event e))
    
    (define/public (reload)
      (set! diagram (new-diagram pairs))
      (refresh-now)
    )
    
    (define/public (new)
      (set! pairs (mcons " " " "))
      (set! diagram (new-diagram pairs))
      (refresh-now)
    )
    
    (define/public (show-info message)
       (send editor-text-diagram lock #f)
       (send feedback-text set-value message)
       (send editor-text-diagram lock #t)
     )
    
    (define/public (save-as-image)
      (let ((coords (get-diagram-coords diagram)))
        (let ((bitmap (make-bitmap (cadr coords) (cddr coords))))
          (let ((dc (send bitmap make-dc))
                (pathfile (put-file "Guardar box&pointer" #f ".."  "canvas" ".png" null '(("png" "*.png") ("any" "*.*")))))
            (paint-diagram diagram dc)
            (send bitmap save-file pathfile 'png)
            )
          )
        )
    )
    
    (define/public (save-definition)
      (let ((pathfile (put-file "Guardar box&pointer" #f ".."  "canvas" ".rkt" null '(("rkt" "*.rkt") ("any" "*.*")))))
        (let ((file (open-output-file pathfile #:mode 'text #:exists 'replace)))
          (display (get-diagram-definition diagram) file)
          (close-output-port file)
      )
     )
   )
    )
)

(define canvas (new my-canvas% 
                    [style (list 'vscroll 'hscroll)]
                    [parent h-panel-canvas]))

(send canvas init-auto-scrollbars 800 600 0 0)
(send (send canvas get-dc) set-font font)

(define (show-ui)
  (send frame show #t)
)