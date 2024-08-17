#lang sicp

(define (make-interval a b) (cons a b))
(define (upper-bound p) (max (cdr p) (car p)))
(define (lower-bound p) (min (cdr p) (car p)))
(define (add-interval p1 p2)
  (make-interval (+ (lower-bound p1) (lower-bound p2))
                 (+ (upper-bound p1) (upper-bound p2))))

(define (sub-interval p1 p2)
  (make-interval  ( min (- (lower-bound p2) (upper-bound p1))
                        (- (lower-bound p1) (upper-bound p2)))
                 (- 0( min (- (lower-bound p2) (upper-bound p1))
                        (- (lower-bound p1) (upper-bound p2))))))
(define (mul-interval x y) 
   (let ((p1 (* (lower-bound x) (lower-bound y))) 
         (p2 (* (lower-bound x) (upper-bound y))) 
         (p3 (* (upper-bound x) (lower-bound y))) 
         (p4 (* (upper-bound x) (upper-bound y)))) 
     (make-interval (min p1 p2 p3 p4) 
                    (max p1 p2 p3 p4))))
(define (div-interval p1 p2)
  (if (= (- (upper-bound p2) (lower-bound p2)) 0)
      (display "error")
      (mul-interval p1
                    (make-interval ( / 1.0 (upper-bound p2))
                                   ( / 1.0 (lower-bound p2))))))


(define (make-center-percent a percent-in)
  (let ( (b (/ percent-in 100))) 
  (make-interval (- a (* a b)) (+ a ( * a b)))))




(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define r1 (make-center-percent 2 1))
(define r2 (make-center-percent 3 1))
(par1 r1 r2)
(par2 r1 r2)
(div-interval r1 r1)
(define r3 (make-center-percent 2 1))
(div-interval r3 r3)