#lang plai/gc2/mutator
(allocator-setup "../collector.rkt" 1024)
(define (build-one) empty)
(define (traverse-one x1) (empty? x1))
(define (trigger-gc n)
  (if (zero? n) 0 (begin n
                         n
                         n
                         n
                         n
                         n
                         n
                         n
                         n
                         n
                         (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 5)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 10)
