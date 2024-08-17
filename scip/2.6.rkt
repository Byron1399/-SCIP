#lang sicp
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
;my:
(define (+ a b)
  (lambda (f) (lambda (x)  ((a f) ((b f) x)))))
(define one (add-1 zero))
(define two (add-1 one))
;answer:
(define +
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (lambda (x)
          ((a f) ((b f) x)))))))