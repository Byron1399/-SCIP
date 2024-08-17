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

(define (filter predicate sequence)
  (if (null? sequence)
      nil
      (if (predicate (car sequence))
          (cons (car sequence) (filter predicate (cdr sequence)))
          (filter predicate (cdr sequence)))))

(define (queens board-size)
  (define (queens-cols k)
    (if(= k 0)
       (list nil)
       (filter
        (lambda (positions) (safe? k positions))
        (flatmap
         (lambda (rest-of-queens)
           (map (lambda (new-row)
                  (adjoin-position new-row k rest-of-queens))
                (enumerate-interval 1 board-size)))
         (queens-cols (- k 1))))))
  (queens-cols board-size))


(define (flatmap pro sequence)
  (accumulate append nil (map pro sequence)))

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))


(define (safe? k positions)
  (define (iter positions)
    (item (car positions) (cdr positions) 1))
  (define (item x y i)
    (if (null? y)
        true
        (if (or (= x (car y))
                (= x (+ (car y) i))
                (= x (- (car y) i)))
            false
            (item x (cdr y) (+ i 1)))))
  (iter positions))


