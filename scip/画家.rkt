#lang sicp



;2.46
(define (make-vect x y)
  (cons x y))
(define (xcor-vect n)
  (car n))
(define (ycor-vect n)
  (cadr n))
(define (add-vect n1 n2)
  (cons (+ (xcor-vect n1) (xcor-vect n2)) (+ (ycor-vect n1) (ycor-vect n2))))
(define (sub-vect n1 n2)
  (cons (- (xcor-vect n1) (xcor-vect n2)) (- (ycor-vect n1) (ycor-vect n2))))
(define (scale-vect s n1)
  (cons (* s (xcor-vect n1)) (* s (ycor-vect n2))))

;2.47
(define (make-frame origin edg1 egde2)
  (list origin edge1 edge2))
(define (origin-fram fram)
  (car fram))
(define (edge1-fram fram)
  (cadr fram))
(define (dege2-fram fram)
  (caddr fram))


;(define (make-frame origin edge1 edge2)
 ; (cons origin (cons edge1 edge2)))
;选择函数和上面一样


;2.48
(define (make-segment start end)
  (list start end))
(define (start-segment n) 
  (car n))
(define (end-segment n)
  (cadr n))
;2.49



;a
(segmets->painter (list (make-segment (list 0 0) (list 1 0))
                        (make-segment (list 0 0) (list 0 1))
                        (make-segment (list 1 0) (list 1 1))
                        (make-segment (list 0 1) (list 1 1)))
                  (make-frame (list 1 1) (list 0 1) (list 1 0)))
;b
(segmets->painter (list (make-segment (list 1 0) (list 0 1)) 
                        (make-segment (list 0 0) (list 1 1)))
                  (make-frame (list 1 1) (list 0 1) (list 1 0)))


;2.50


(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0  0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;2.51


(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let((painter-bottom
          (transform-painter painter1
                             (make-vect 0.0 0.0)
                             (make-vect 1.0 0.0)
                             split-point))
         (painter-top 
          (transform-painter painter2
                             split-point
                             (make-vect 1.0 0.5)
                             (make-vect 0.5 1.0))))
      (lambda(frame)
        (painter-bottom frame)
        (painter-top frame)))))

(define (below painter1 painter 2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))







;2.52
;c

(define (square-limil painter n)
  (let ((quarter (conrner-split painter n)))
    ((square-of-four identity flip-horiz
                    flip-vect rotate180)
                        quarter)))
                        
