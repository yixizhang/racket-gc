#lang plai/gc2/mutator
(allocator-setup "../../hybrid-bm.rkt" 400)
(define (build-one) (let* ((x0 #f) (x1 empty)) x1))
(define (traverse-one x1) (empty? x1))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 10)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 100)

#|
benchmark result:

generational collector:
peak heap size is 100%, average heap size is 49%
heap operations: peak 1529, average 333, total 36970

hybrid collector:
peak heap size is 25%, average heap size is 18%
heap operations: peak 765, average 487, total 53080
|#