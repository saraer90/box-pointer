;PRUEBAS
(require "../program.rkt")
(#%require compatibility/mlist)

(define pairs (mcons 'a (mcons 'b (mcons 'c null))))
(set-mcdr! (mcdr (mcdr pairs)) pairs)

(box-pointer pairs)