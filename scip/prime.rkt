#lang sicp
(define (squre testnum) (* testnum testnum))
(define (smallest-div n)
  (define (smallest-div-iter n testnum)
  (cond((> (squre testnum) n) n)
       ((= (remainder n testnum) 0) testnum)
       (else(smallest-div-iter n (+ testnum 1)))))
  (smallest-div-iter n 2))
(define (prime? n)
  (= (smallest-div n) n))
(define (nextjishu n)
  (if (=(remainder n 2) 0) (+ n 1) (+ n 2)))

(define(search-for-primes n c)
  (define (time-prime-test x)
    (start-prime-test x (runtime)))


  (define (start-prime-test x start-time)
    (if (prime? x)
        (report-prime x (-(runtime) start-time))))



  (define (report-prime x elapsed-time)
    (set! c (- c 1))
    (newline)
    (display x)
    (display"***")
    (display elapsed-time))
  (define (search-for-primes-iter x count)
    (cond((= 0 c))
         (else (time-prime-test x)(search-for-primes-iter(+ 1 x) c))))

    (search-for-primes-iter (+ n 1) c))
