(require "../program.rkt")

(define pairs (mcons (mcons (mcons (mcons 1 2) (mcons 3 4)) (mcons (mcons 1 2) (mcons (mcons (mcons 1 2) 3) 8))) ""))

(box-pointer pairs)
