(require "../program.rkt")

(define pairs (mcons (mcons (mcons (mcons 1 2) (mcons 3 4)) (mcons (mcons 5 6) (mcons 7 8))) ""))

(box-pointer pairs)
