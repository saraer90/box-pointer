;PRUEBAS
(require "../program.rkt")
(#%require compatibility/mlist)

(define pairs (mcons (mcons 'a (mcons (mlist 1 2) 'b)) (mcons 1 2)))
(set-mcdr! (mcar pairs) (mcar pairs))
(set-mcar! (mcdr pairs) pairs)

(box-pointer pairs)
