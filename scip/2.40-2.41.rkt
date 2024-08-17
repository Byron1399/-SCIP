#lang sicp
(define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence)))))

(define (enumerate-interval a b)
  (if (> a b)
      nil
      (cons a (enumerate-interval (+ a 1) b))))


(define (map op sequence)
  (accumulate (lambda ( x y) (cons (op x ) y)) nil sequence))

(define (map-1 op sequence)
  (accumulate (lambda ( x y) (append (op x ) y)) nil sequence))


(define (unique-pairs n)
  (map-1 (lambda (x)
         (map(lambda(y)
              (list y x))
              (enumerate-interval 1 (- x 1))))
       (enumerate-interval 1 n)))
(unique-pairs 5)
(unique-pairs 2)


(define (unique-pairs-3 n)
  (filter (lambda (x) (= (+ (car x) (cadr x)) (caddr x)))
          (map-1 (lambda (x)
           (map-1 (lambda(y)
              (map (lambda (z)
                      (list z y x))
                   (enumerate-interval 1 (- y 1))))
               (enumerate-interval 1 (- x 1))))
       (enumerate-interval 1 n))))




(define (filter predicate sequence)
  (if (null? sequence)
      nil
      (if (predicate (car sequence))
          (cons (car sequence) (filter predicate (cdr sequence)))
          (filter predicate (cdr sequence)))))





