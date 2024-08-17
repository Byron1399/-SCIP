#lang sicp
;2.65
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree m1)
  (car (iter m1 (length m1 ))))

(define (iter m1 n)
  (if(= n 0)
     (cons '() m1)
     (let( (left-num (quotient (- n 1) 2)))
       (let (( left-branch (iter m1 left-num))
             (right-num (- n (+ left-num 1))))
         (let(( left-tree (car left-branch))
              (mid-branch (cdr left-branch)))
           (let ((this-tree (car mid-branch))
                 
                 (right-branch(iter (cdr mid-branch) right-num)))
             (let((right-tree (car right-branch))
                  (remain (cdr right-branch)))
               (cons(make-tree this-tree left-tree right-tree)
                    remain))))))))


(define (tree->list m1)
  (define(iter m result)
    (if(null? m)
       result
       (iter (left-branch m) 
             (cons (entry m)
                   (iter (right-branch m) result)))))
  (iter m1 '()))
(define (union-set m1 m2)
  (define (union-set-iter m1 m2)
  (if(or(null? m1) (null? m2))
     (if (null? m1)
         m2
         m1)
     (cond((< (car m1) (car m2)) (cons (car m1) (union-set (cdr m1) m2)))
          ((= (car m1) (car m2)) (cons (car m1) (union-set (cdr m1) (cdr m2))))
          ((> (car m1) (car m2)) (cons (car m2) (union-set m1 (cdr m2)))))))
  (list->tree (union-set-iter (tree->list m1) (tree->list m2))))

(define (intersection-set m1 m2)
  (define (intersection-set-iter set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
          (cond ((= x1 x2)
                 (cons x1
                       (intersection-set (cdr set1)
                                         (cdr set2))))
                ((< x1 x2)
                 (intersection-set (cdr set1) set2))
                ((< x2 x1)
                 (intersection-set set1 (cdr set2)))))))
  (list->tree (intersection-set-iter (tree->list m1) (tree->list m2))))



;2.66
(define (lookup given-key m1)
  (cond (( null? m1) false)
        ((= given-key (key(entry m1))) (entry m1))
        ((< given-key (key(entry m1))) (lookup given-key (left-branch m1)))
        ((> given-key (key(entry m1))) (lookup given-key (right-branch m1)))))