#lang sicp
(define (lookup given-key set-of-records)
  (cond ( (null? set-of-records) false)
        ( (equal? given-key (car set-of-records)) (set-of-records))
        ( (< given-keycar set-of-records ) (lookup given-key (cadr set-of-records)))
        ( (> given-key car set-of-records) (lookup given-key (caddr set-of-records))))