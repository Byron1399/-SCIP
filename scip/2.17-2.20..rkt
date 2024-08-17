#lang sicp
(define nu (list))
nu
;2.17
(define (last-pair item)
  (if (null? item)
      (display "empty")
      (if (null? (cdr item))
          (car item)
          (last-pair (cdr item)))))
(define a (list 1 2 3 4 5))

;2.18
(define (reverse item)
  (define b '())
  (define(reverse-iter e1 e2)
       (if (null? (cdr e1))
           (cons (car e1 ) e2)
           (reverse-iter (cdr e1) (cons (car e1) e2))))
 (if (null? item)
     (display "empty")
     (reverse-iter item b))
  )

;2.19

(define (first-deno coin)
  (car coin))
(define (ex-first-deno coin)
  (cdr coin))
(define (no-more? coin)
  (null?  coin))
(define (cc amount coin-values)
    (cond ((= amount 0)
            1)
          ((or (< amount 0) (no-more? coin-values))
            0)
          (else
            (+ (cc amount
                   (ex-first-deno coin-values))
               (cc (- amount
                      (first-deno coin-values))
                   coin-values)))))
(define us-coins(list 50 25 10 5 1))
(cc 100 us-coins)
(cc 100 (reverse us-coins))


;2.20

(define (same-parity a . b)
  (define (iter p result remain)
    (if (null? p)
        result
        (cond  ((= (remainder (car p) 2) remain) (iter (cdr p) (cons (car p) result ) remain))
               (else (iter (cdr p) result remain)))))
             
  (if(null? b)
     (display "empty")
     (reverse(iter b (list 1) (remainder a 2)))))
(same-parity 1 2 3 4 5 6 7)
