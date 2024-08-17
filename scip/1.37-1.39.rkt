#lang sicp
(define (fixed-point f guess)
  (define count 0)
  (define (try guess)
    (let ( (next (f guess))
          )
      (if (enough?  guess next)
             next
             (try  next))))
  (try guess))
(define (ex1.36zuni)
  (fixed-point (lambda (x) (* (/ 1 2) (+ x (/ (log 1000) (log x))))) 2))
(define wucha 0.0001)
(define (enough? a b)
  (< (abs (- a b)) wucha))


(define (n1 a) 1.0)
      
(define (d3 a) (+ 1
                  (* (- a 1)
                     2)
                  ))




(define (d1 a) 1.0)
(define (d2 a)
  (cond ((= a 1) 1)
        ((= (remainder (- a 1) 3) 1) ( + (* 2
                                            (/
                                             (- a 2)
                                             3) )
                                         2))
        (else 1)))
(define (cont-frac1 n d k)
  (define (iter y result)
    (let (( o ( n y) )
          ( p ( d y)))
    (if (= y k)
        (iter (- y 1) (+ result ( / o p)))
        (if (= y 1)
            (/ o (+ p result))
            (iter (- y 1) (/ o (+ p result)))))))
    (iter k 0))



(define (cont-frac2 c d k)
  (define (iter n)
    (let (( o ( c n) )
          ( p ( d n)))
    (if (= n k) (/ o p)
        (/ o (+   p (iter (+ 1 n)))))))
  (iter 1))


(define (qiuk n)
  (if (enough? (cont-frac1 n1 d1 n) 0.6180340557275542)
      n
      (qiuk (+ n 1))))


(define (tan-cf x k)
  (define (n2 a)
    (if (= a 1) x
        (- 0 (* x x))))
  (cont-frac1 n2 d3 k))
  