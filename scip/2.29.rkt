#lang sicp
(define (make-mobile left right)
    (list left right))

(define (make-branch length structure)
    (list length structure))

(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
    (cadr mobile))

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
    (cadr branch))

(define (branch-weight branch)
  (if (pair? (branch-structure branch))
      (total-weight(branch-structure branch))
      (branch-structure branch)))


(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))


(define mobile (make-mobile (make-branch 10 20)       ; 活动体的总重量为 20 + 25 = 45
                                  (make-branch 10 25)))
(define another-mobile (make-mobile (make-branch 10 mobile)   ; 左分支吊着另一个活动体,总重为 45
                                          (make-branch 10 20)))

(total-weight another-mobile)


(define (branch-n branch)
  (* (branch-length branch) (branch-weight branch)))
(define (branch-ambulance branch)
  (if ( pair? (branch-structure branch))
      (mobile-ambulance (branch-structure branch))
      #t))
(define (mobile-ambulance mobile)
  (let (  (left (left-branch mobile))
          (right (right-branch mobile)))
    (and (= (branch-n left) (branch-n right))
         (branch-ambulance left)
         (branch-ambulance right))))
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
        
      
