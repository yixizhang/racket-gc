#lang plai/gc2/mutator
(allocator-setup "collector.rkt" 512)
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i) 'passed (begin (trigger-gc 200) (loop (- i 1)))))
(loop 200)
