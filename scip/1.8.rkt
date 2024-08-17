#lang sicp
(define (lifang x)
  (panduan 1.0 0.0 x))
(define (panduan guess pre-guess x)
  (if(guess-enough? guess pre-guess x)
     guess
     (panduan (improve guess  x) guess x)))
(define (sqr x) (* x x))
(define (guess-enough? guess pre-guess x)
 (<(abs(- guess pre-guess))(abs(* guess 0.01))))
(define (improve guess x)
  (/(+(* 2 guess)
      (/ x (sqr guess)))
    3))
(lifang -27)

(real-time-clock)