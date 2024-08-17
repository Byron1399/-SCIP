#lang sicp
(define (squre x) (* x x))
(define (just a) a)
(define (inc c) (+ 1 c))
(define (filtered-accumulate  combiner null-value term a next b judge )
  (define (iter a null-value)
    (if (> a b)
        null-value
        (if (judge (term a))
            (iter (next a) (combiner (term a) null-value))
            (iter (next a) null-value))))
  (iter a null-value))

(define (quyu base exp m)
  (cond((= exp 0) 1)
       ((even? exp) (remainder(squre(quyu base (/ exp 2) m)) m))
       (else (remainder (* (remainder base m) (quyu base (- exp 1) m)) m))))
(define (prime? n)
  (define (iter n times)
    (if(= 0 times) true
       (if (fast-test n) (iter n (- times 1))
            false)))
  (iter n 5))
(define (fast-test n)
  (if (= n 1) true
      (if (= (quyu (+ 1 (random (- n 1))) (- n 1) n) 1) true
           false)))
(define (coprime? i n)
  (and (< i n)
       (= (gcd i n) 1)))
(define(product-coprime n)
  (filtered-accumulate * 1 (lambda (x) x) 1 (lambda (x) (+ 1 x)) n (lambda (x) (coprime? x n))))

  