#lang sicp
(define (settime)
  (fin (runtime)))
(define (fin time)
  (- (runtime) time)) 
(define (squre x) (* x x))
(define (man base exp m)
  (cond((= exp 0)1)
       ((even? exp)(remainder (squre (man base (/ exp 2) m)) m))
       (else (remainder (* base (man base (- exp 1) m)) m))))
(define (rand n)
  (define (fenli a n b)
    (= (man a n b ) 1))
  (fenli (+ 1 (random(- n 1))) (- n 1) n))
(define (prime? n times)
  (cond((= times 0) (display "*"))
       ((rand n) (prime? n (- times 1)))
       (else 1)))
(define (start n)
  (define a (runtime))
  (display a)
  (prime? n 5)
  (newline)
  (display (runtime))
  (set! a (- (runtime) a))
  (newline)
  (display a))
  


   

