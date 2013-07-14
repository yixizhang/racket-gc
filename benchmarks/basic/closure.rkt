#lang plai/gc2/mutator
(allocator-setup "../collector.rkt" 1536)
(define (build-one) empty)
(define (traverse-one x1) (empty? x1))
(define (build a b c)
  (+ a
     ((lambda ()
        (+ b
           ((lambda ()
              c)))))))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (build 1 2 3) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 5)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 8)
