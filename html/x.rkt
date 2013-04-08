#lang racket
(define-struct hash-t (vec))
(define ht (make-hash-t (make-vector 10)))
(hash-t-vec ht)
(append '(1 2) '(3 4))
(define (ap some-list more-list)
  (cond
    [(null? some-list) more-list]
    [else
     (cons (first some-list)
           (ap (rest some-list) more-list))]))
(ap '(1 2) '(3 4))