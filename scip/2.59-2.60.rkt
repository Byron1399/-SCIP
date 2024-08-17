#lang sicp
;2.59
(define (element-of-set x set)
  (cond ((null? set)  false)
        ((equal? x (car set)) true)
        (else (element-of-set x (cdr set)))))

(define (union-set m1 m2)
 (define(iter m1 m2 result)
   (if(null? m1)
      result
      (cond ((element-of-set (car m1) m2) (iter (cdr m1) m2 result))
            (else (iter (cdr m1) m2 (cons (car m1) result))))))
  (iter m1 m2 m2))


(union-set '(1 2 3 4 5) '(7 8 9 5))
(define(intersection-set set1 set2)
  
  (define (iter set1 result)
    (if(null? set1)
       result
       (if(and (element-of-set (car set1) set2)
               (not(element-of-set (car set1) result)))
          (iter (cdr set1) (cons (car set1) result))
          (iter (cdr set1) result))))
  (if(or (null? set1) (null? set2))
     nil
     (iter set1 nil)))
     



