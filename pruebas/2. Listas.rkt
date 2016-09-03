;PRUEBAS
(require "../program.rkt")
(#%require compatibility/mlist)

(define pairs (mcons (mlist (mlist 'a 'b 'b) 2 3) (mlist 3 4 5 6)))

(box-pointer pairs)