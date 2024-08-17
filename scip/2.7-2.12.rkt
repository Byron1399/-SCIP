#lang sicp
;2.7
(define (make-interval a b) (cons a b))
(define (upper-bound p) (max (cdr p) (car p)))
(define (lower-bound p) (min (cdr p) (car p)))
;2.8
(define (sub-interval p1 p2)
  (make-interval  ( min (- (lower-bound p2) (upper-bound p1))
                        (- (lower-bound p1) (upper-bound p2)))
                 (- 0( min (- (lower-bound p2) (upper-bound p1))
                        (- (lower-bound p1) (upper-bound p2))))))
                  
                    
(define e1 (make-interval 1 3))
(define e2 (make-interval 0 2))
(define e3 (sub-interval e1 e2))

(define (add-interval p1 p2)
  (make-interval (+ (lower-bound p1) (lower-bound p2))
                 (+ (upper-bound p1) (upper-bound p2))))

;2.10

(define (div-interval p1 p2)
  (if (= (- (upper-bound p2) (lower-bound p2)))
      (display "error")
      (mul-interval p1
                    (make-interval ( / 1.0 (upper-bound p2))
                                   ( / 1.0 (lower-bound p2))))))
(define (mul-interval x y) 
   (let ((p1 (* (lower-bound x) (lower-bound y))) 
         (p2 (* (lower-bound x) (upper-bound y))) 
         (p3 (* (upper-bound x) (lower-bound y))) 
         (p4 (* (upper-bound x) (upper-bound y)))) 
     (make-interval (min p1 p2 p3 p4) 
                    (max p1 p2 p3 p4))))
(define e4 (make-interval 3 3))
(div-interval e1 e4)

;2.11
(define(stupid-mul p1 p2)
  (let ( ( y (upper-bound p1))
         ( x (lower-bound p1))
         ( b (upper-bound p2))
         ( a (lower-bound p2))
        )
    (cond ( (and (> x 0)  (> a 0)) (make-interval (* x a) (* y b)))
          ( (and (> x 0)  (< a 0) (< b 0)) (make-interval (* y a) (* x b)))
          ( (and (> x 0)  (< a 0) (> b 0)) (make-interval (* y a) (* y b)))
          ( (and (< x 0)  (> y 0) (> a 0)) (make-interval (* x b) (* y b )))
          ( (and (< x 0)  (> y 0) (< a 0) (< b 0)) (make-interval (* y a) (* x a)))
          ( (and (< x 0)  (> y 0) (< a 0) (> b 0)) (make-interval ( (min (* y a) (* x b)) (max (* y b) (* x a)))))
          ( (and (< x 0)  (< y 0) (> a 0) ) (make-interval (* x b) (* y a)))
          ( (and (< x 0)  (< y 0) (< a 0) (> b 0)) (make-interval (* x b) (* x a)))
          ( (and (< x 0)  (< y 0) (< a 0) (< b 0)) (make-interval (* y b) (* x a)))
          )))

;2.12
(define (make-center-percent a percent-in)
  (let ( (b (/ percent-in 100))) 
  (make-interval (- a (* a b)) (+ a ( * a b)))))


(define (percent p1)
  (let ((mid (/ (+ (upper-bound p1)
                  (lower-bound p1))
               2)))
    (/ (- (upper-bound p1) mid)
       mid)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define r1 (make-center-percent 2 5))
(define r2 (make-center-percent 3 7))
(par1 r1 r2)
(par2 r1 r2)
          
          
          
          
          
          
          
          



  
                    
                        
