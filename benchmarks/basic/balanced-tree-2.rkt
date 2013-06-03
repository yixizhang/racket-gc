#lang plai/gc2/mutator
(allocator-setup "../collector.rkt" 512)
(define (build-one) empty)
(define (traverse-one x1) (empty? x1))
(define (build-tree n)
  (cond
    [(= n 0) (cons #f #f)]
    [else (cons (build-tree (- n 1))
                (build-tree (- n 1)))]))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (build-tree 2) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 10)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 100)
