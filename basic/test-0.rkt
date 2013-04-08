#lang plai/gc2/mutator
(allocator-setup "../generational.rkt" 400)
(define (build-one) (let* ((x0 #f) (x1 empty)) x1))
(define (traverse-one x1) (empty? x1))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 1)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 2)
