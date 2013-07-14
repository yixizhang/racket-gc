#lang plai/gc2/mutator
(allocator-setup "../collector.rkt" 1024)
(define (build-one) empty)
(define (traverse-one x1) (empty? x1))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (make-vector 20 0) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 10)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 10)
