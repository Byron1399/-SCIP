#lang sicp
;2.53
;(a b c)
;((george))
;((y1 y2))
;(y1 y2)
;false
;(red shoes)   ;flase
;(red shoes blue socks)

;2.54
(define (equal? item guess)
  (if(and (null? item)
          (null? guess))
     true
     (if (pair? item)
         (if(pair? guess)
            (if(eq? (car item) (car guess))
               (equal? (cdr item) (cdr guess))
               false)
            false)
         (eq? item guess))))

;2.56
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (exp-base x)
  (cadr x))
(define (exp-exp x)
  (caddr x))

(define(=number? x y)
  (and (number? x) (= x y )))

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((and (number? base) (number? exp)) (square-n base exp))
        (else (list '** base exp))))

(define (square-n base exp)
  (if(= exp 0)
     1
     (if(= (remainder exp 2) 1)
        (* base (square-n base (- exp 1)))
        (square-n (square-n base (/ exp 2)) 2))))




(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))



(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (if(null? (cddr s))
     0
     (cons '+ (cddr s))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (if(null? (cddr p))
      1
      (if (null? (cdddr p))
          (caddr p)
          (cons '* (cddr p)))))


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))


(define (make-product m1 m2)
  (if(pair? m2)
     (if(eq? '* (car m2))
        (make-product m1 (cdr m2))
        (list '* m1 m2))
     (cond ((or (=number? m1 0) (=number? m2 0)) 0)
           ((=number? m1 1) m2)
           ((=number? m2 1) m1)
           ((and (number? m1) (number? m2)) (* m1 m2))
           (else (list '* m1 m2)))))



(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exp-exp exp)
                        (make-exponentiation (exp-base exp) (make-sum (exp-exp exp) -1)))
          (deriv(exp-base exp) var)))
        
        (else
         (error "unknown expression type -- DERIV" exp))))
(deriv '(* x x x) 'x)
        