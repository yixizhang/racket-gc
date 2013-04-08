#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 'x)
         (x1 (cons x0 #f))
         (x2 (cons x0 #f))
         (x3
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1) x2 (if (= x 2) x1 (if (= x 3) x2 x0)))))))
    (set-rest! x1 x1)
    (set-rest! x2 x2)
    x3))
(define (traverse-one x3) (symbol=? 'x (first (x3 0))))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
