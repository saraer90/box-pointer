(require "../program.rkt")

(define pairs (mcons 1 2))
(set-mcdr! pairs lista-parejas)

(box-pointer pairs)