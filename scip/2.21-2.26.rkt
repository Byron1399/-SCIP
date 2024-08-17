#lang sicp
(define (square n ) (* n n))


;2.21
(define (square-list item)
  (if(null? item)
     nil
     (cons  (square(car item))
            (square-list (cdr item)))))
(define (square-list-map item)
  (map square item))
;2.23
(define (for-each-my f p)
  (cond (( null? p ) nil)
        (else ( (f (car p)) (for-each f (cdr p))))))

(define (for-each-answer1 p lst)
    (cond ((not (null? lst))
            (p (car lst))
            (for-each-answer1  p (cdr lst)))))


(define (for-each-answer2 p lst)
    (if (not (null? lst))
        (begin
            (p (car lst))
            (for-each-answer2 p (cdr lst)))))



(define x ( cons (list 1 2) (list 3 4)))
(for-each-answer1 (lambda (x) (newline) (display x)) x)
(newline)
(cons 1 (list 1 2 3))
(cons (cons 1 2) (list 1 2 3))
(cons 1 3)