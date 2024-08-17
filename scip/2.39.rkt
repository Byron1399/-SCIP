#lang sicp
(define (fold-right op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (fold-right op initial (cdr sequence)))))
(define (reverse1 sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))


(define (reverse2 sequence)
  (fold-right (lambda (x y)  (cons y (list x))) nil sequence))


(reverse2 (list (list 1 2)  3 4))
(reverse1 (list (list 1 2)  3 4))
(reverse1 (list  1 2  3 4))

;;new idea
(define (reverse-using-right sequence) 
   (fold-right 
     (lambda (x y) 
       (fold-right cons (list x) y)) 
     nil 
     sequence))



(reverse-using-right (list 1 2 3 4))
(reverse-using-right (list (list 1 2) 3 4))


