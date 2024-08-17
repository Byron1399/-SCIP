#lang sicp
(define (make-rat n d)
  (if (> (* n d) 0 )  (cons (abs n) (abs d))
      (if (< n 0) (cons n d)
          (cons (- 0 n) (abs d)))))
(car(make-rat -3 -4))
