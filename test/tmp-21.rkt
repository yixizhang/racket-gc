#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one) (let* ((x0 #f) (x1 -1) (x2 #f) (x3 -1) (x4 'y)) x0))
(define (traverse-one x0) (if x0 #f #t))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
