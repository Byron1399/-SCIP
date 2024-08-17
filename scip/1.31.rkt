#lang sicp
(define (pai1 n)
  (cond((= n 1) 2)
       ( (even? n) (+ 2 n))
       (else (+ 1 n))))
(define (pai2 n)
  (cond((= n 1 ) 3)
       ((even? n)(+ 1 n))
       (else (+ 2 n))))
(define (just a) a)
(define (inc a )(+ a 1))
(define(p1 term a b)
  (product1 term a inc b 1))
(define (product1 term a next b)
  (define (iter a result)
  (if (> a b)
       result
      (iter(next a) (* result (term a)))))
  (iter a 1))


(define(p2 term a b)
  (product2 term a inc b))
(define (product2 term a next b)
  (define (iter a)
  (if (> a b)
       1
      (* (term a) (iter (next a)))))
  (iter a))
 (define (pi n)
  (* 4 (exact->inexact(/ (product1 pai1 1 inc n) (product1 pai2 1 inc n)))))
  
