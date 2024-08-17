#lang sicp
;2.61
(define (adjoin-set x m1)
  (if(null? m1)
     (cons x nil)
     (cond( (< x (car m1)) (cons x m1))
          ( (= x (car m1)) m1)
          ( (> x (car m1)) (cons (car m1) (adjoin-set x (cdr m1)))))))

(adjoin-set 2 '( 1 2 3))

;2.62
(define (union-set m1 m2)
  (if(or(null? m1) (null? m2))
     (if (null? m1)
         m2
         m1)
     (cond((< (car m1) (car m2)) (cons (car m1) (union-set (cdr m1) m2)))
          ((= (car m1) (car m2)) (cons (car m1) (union-set (cdr m1) (cdr m2))))
          ((> (car m1) (car m2)) (cons (car m2) (union-set m1 (cdr m2)))))))
(union-set '(1 2 3 4 5) '(4 5 6 7 8))

;2.63

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))



(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))
1
(tree->list-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define a (make-tree 7
                           (make-tree 3
                                      (make-tree 1 '() '())
                                      (make-tree 5 '() '()))
                           (make-tree 9
                                      '()
                                      (make-tree 11 '() '()))))
; 1) 同样结果
; 2) 第二个更快 第一个是 n2 第二个是n


