#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 -1)
         (x1 (cons #f #f))
         (x2
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1) x1 (if (= x 2) x1 (if (= x 3) x1 x0))))))
         (x3
          (lambda (x)
            (if (= x 0)
              x2
              (if (= x 1)
                x1
                (if (= x 2) x2 (if (= x 3) x2 (if (= x 4) x0 x0)))))))
         (x4 (lambda (x) (if (= x 0) x0 (if (= x 1) x3 (if (= x 2) x0 x3)))))
         (x5
          (lambda (x)
            (if (= x 0)
              x4
              (if (= x 1) x0 (if (= x 2) x0 (if (= x 3) x1 x4))))))
         (x6 (cons #f x0))
         (x7 (cons #f #f))
         (x8 (cons x4 x1))
         (x9 (cons #f x0)))
    (set-first! x1 x8)
    (set-rest! x1 x2)
    (set-first! x6 x6)
    (set-first! x7 x8)
    (set-rest! x7 x8)
    (set-first! x9 x9)
    x7))
(define (traverse-one x7) (= -1 (((first (first x7)) 3) 5)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
