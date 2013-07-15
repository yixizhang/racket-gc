#lang plai/gc2/mutator
(allocator-setup "../collector.rkt" 1024)
(define (build-one) empty)
(define (traverse-one x1) (empty? x1))
(define (build a b c d e f)
  (+ a
     ((lambda ()
        (+ b
           ((lambda ()
              (+ c
                 ((lambda ()
                    (+ d
                       ((lambda ()
                          (+ e
                             ((lambda ()
                                f))))))))))))))))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (build 1 2 3 4 5 6) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 10)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 10)
