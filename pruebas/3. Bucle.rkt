(require "../program.rkt")

(define pairs (mcons 1 2))
(set-mcdr! pairs pairs)

(box-pointer pairs)