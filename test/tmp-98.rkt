#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 -1)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2) x0 (if (= x 3) x0 (if (= x 4) x0 x0)))))))
         (x2 #f)
         (x3 0)
         (x4 -1)
         (x5 (cons #f x0))
         (x6 (cons x1 x1)))
    (set-first! x5 x6)
    x2))
(define (traverse-one x2) (if x2 #f #t))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
