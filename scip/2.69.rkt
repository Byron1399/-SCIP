#lang sicp
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))



(define sample-tree
   (make-code-tree (make-leaf 'A 4)
                   (make-code-tree
                    (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))



(define (encode-symbol char tree)
  (define(in x m)
    (if (null? m)
        false
        (if (eq? x (car m ))
            true
            (in x (cdr m)))))
  (define (iter char tree result)
    (if(and (leaf? tree) (not(eq? result '())))
        result
        (if (in char (symbols tree))
            (cond ( (in char (symbols (left-branch tree))) (iter char (left-branch tree) (append result (list 0))))
                  ( (in char (symbols (right-branch tree))) (iter char (right-branch tree) (append result (list 1)))))
            (error"no"))))
                
  (iter char tree '()))

(encode '(A D A B B C A) sample-tree)


(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))
(define (successive-merge x)
  (define (iter x result)
    (if (null? x)
        result
        (iter (cdr x) (make-code-tree (car x) result))))
  (iter (cddr x) (make-code-tree (car x) (cadr x))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(decode '(0 1 1 0 0 1 0 1 0 1 1 1 0) (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))