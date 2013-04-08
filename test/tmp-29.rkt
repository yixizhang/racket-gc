#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 0)
         (x1 -1)
         (x2 (lambda (x) (if (= x 0) x0 (if (= x 1) x0 x0))))
         (x3
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1)
                x1
                (if (= x 2) x0 (if (= x 3) x2 (if (= x 4) x1 x0)))))))
         (x4 (cons #f x0)))
    (set-first! x4 x4)
    x3))
(define (traverse-one x3) (= -1 (x3 0)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
