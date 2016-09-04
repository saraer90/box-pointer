(require "../program.rkt")
(#%require compatibility/mlist)

(define pairs (mcons (mcons (mlist 1 2 3 4 5 6) (mcons 3 4)) (mcons (mcons 5 6) (mcons 7 8))))

(box-pointer pairs)
