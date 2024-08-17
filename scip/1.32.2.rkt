#lang sicp
(define (just a) a)
(define (inc a )(+ a 1))
(define (accumulate combiner null-value term a next b)
  (define (iter a null-value)
    (if (> a b)
        null-value
        (iter (next a) (combiner null-value (term a)))))
  (iter a null-value))
(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (pro term a next b)
  (accumulate * 1 term a next b))
