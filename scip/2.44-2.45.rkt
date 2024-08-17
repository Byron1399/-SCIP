#lang sicp
;2.44


(define (up-split painter n)
  (if(= n 0)
     painter
     (let ((smaller(up-split painter (- n 1))))
       (below painter (beside smaller smaller)))))
;2.45
(define (split n1 n2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller( (split n1 n2) painter (- n 1))))
          (n1 painter (n2 smaller smaller))))))

