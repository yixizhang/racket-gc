#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 0)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3) x0 (if (= x 4) x0 (if (= x 5) x0 x0))))))))
         (x2 'x)
         (x3 0)
         (x4 -1)
         (x5
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1)
                x4
                (if (= x 2) x2 (if (= x 3) x1 (if (= x 4) x1 x2))))))))
    x5))
(define (traverse-one x5) (= 0 ((x5 0) 2)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
