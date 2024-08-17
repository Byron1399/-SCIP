#lang sicp
;2.30

(define a (cons (list 1 2) (list 3 4)))
(define (square n) (* n n))

(define (ftree tree f)
  (if (null? tree)
      nil
      (if(pair? tree )
         (cons  (ftree (car tree) f)
                (ftree (cdr tree) f))
           (f tree))))
(ftree a square)


(define (ftree-map tree f)
  (map (lambda (tree)
         (if (pair? tree)
             (ftree-map tree f)
             (f tree)))
       tree))
(ftree-map a square)


;2.31
(define (tree-map f tree)
  (ftree-map f tree))

;2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (append (list (car s)) x)) rest)))))
(subsets (list 1 2 3))

