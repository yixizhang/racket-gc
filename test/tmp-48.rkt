#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 'y)
         (x1 (cons #f #f))
         (x2 (lambda (x) (if (= x 0) x0 (if (= x 1) x0 x0))))
         (x3 1)
         (x4 (cons x0 #f))
         (x5
          (lambda (x)
            (if (= x 0)
              x3
              (if (= x 1)
                x3
                (if (= x 2)
                  x3
                  (if (= x 3) x2 (if (= x 4) x0 (if (= x 5) x3 x4))))))))
         (x6 -1)
         (x7 'x)
         (x8
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1) x3 (if (= x 2) x6 (if (= x 3) x6 x7))))))
         (x9 (cons x2 x5)))
    (set-first! x1 x8)
    (set-rest! x1 x9)
    (set-rest! x4 x4)
    x8))
(define (traverse-one x8) (= 1 ((first ((first ((first (x8 0)) 0)) 0)) 1)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
