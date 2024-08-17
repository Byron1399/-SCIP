#lang sicp
;reverse
(define (reverse p)
  (define (reverse-iter p result)
                (if (null? (cdr p))
                    (cons (car p) result)
                    (reverse-iter  (cdr p) (cons (car p) result))))
  (if (null? p)
      '()
      (reverse-iter p '())))
  
(reverse (list 1 2 3 4))
(reverse '())
;2.27
(define (reverse-1 p)
  (define (reverse-iter p result)
                (if (null? (cdr p))
                    (if( pair? (car p))
                       (cons (reverse-1 (car p)) result)
                       (cons (car p) result))
                    (reverse-iter  (cdr p) (if( pair? (car p))
                                              (cons (reverse-1 (car p)) result)
                                              (cons (car p) result)))))
  (if (null? p)
      '()
      (reverse-iter p '())))


(reverse-1 (list (list 1 2) (list 3 4) (list 5 6)))

(reverse-1 '())
;2.28
;my
(define (fringe-my p)
  (define (fringe-mid p)
    (define (fringe-iter p result)
      (if (null? p)
          result
          (fringe-iter (cdr p) (if (pair? (car p))
                                   (append (fringe-mid (car p)) result)
                                   (append (cons (car p) '() ) result)))))
    (fringe-iter p '()))
  (reverse (fringe-mid p)))
 (fringe-my (list (list 1 2) (list 5 6) (list 3 4)))
;answer
(define (fringe tree)
    (cond ((null? tree)                         ; 空树
            '())
          ((not (pair? tree))                   ; 叶子
            (list tree))
          (else
            (append (fringe (car tree))         ; 累积左子树所有元素
                    (fringe (cdr tree))))))    ; 累积右子树所有元素
;2.29

;a
(define (make-mobile left right)
    (list left right))

(define (make-branch length structure)
    (list length structure))
(define (left-branch a)
  (car a))
(define (right-branch a)
  (cadr a))
(define (branch-length a)
  (car a))
(define (branch-structure a)
  (cadr a))
;b
(define (total-weight a)
  (define (iter a result)      
    (+ result
       (if(pair? (branch-structure (left-branch a))) 
          (total-weight (branch-structure (left-branch a)))
          (branch-structure (left-branch a)))
       (if(pair? (branch-structure (right-branch a)))
          (total-weight (branch-structure (right-branch a)))
          (branch-structure (right-branch a)))
       
       ))
    
    
  (iter a 0))
(define mobile (make-mobile (make-branch 10 25)
                                  (make-branch 5 20)))
(total-weight mobile)
;c
(define (ambulance a)
  (if( = (t(left-branch a ))  (t(right-branch a)))
     true
     false))
(define ( t a)
  (if(pair? (branch-structure a))
     (if (ambulance (branch-structure a)) 
         (* (branch-length a) (t (branch-structure a)))
         -1)
     (* (branch-length a) (branch-structure a) )))
     

(define mobile2 (list (list 1 4) (list 4 4)))
(define mobile1 (make-mobile (make-branch 3 mobile2)
                                  (make-branch 3 20)))
              (ambulance mobile1)
              

  


