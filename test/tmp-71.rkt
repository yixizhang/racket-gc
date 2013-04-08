#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 1)
         (x1 'y)
         (x2 -1)
         (x3 #t)
         (x4
          (lambda (x)
            (if (= x 0)
              x2
              (if (= x 1) x3 (if (= x 2) x0 (if (= x 3) x2 x1)))))))
    x4))
(define (traverse-one x4) (= -1 (x4 0)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
