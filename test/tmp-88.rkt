#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 -1)
         (x1 (cons #f x0))
         (x2
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x1
                    (if (= x 4) x0 (if (= x 5) x1 (if (= x 6) x1 x1)))))))))
         (x3 (cons #f #f))
         (x4 (cons #f x2))
         (x5 'y)
         (x6 (cons x3 x3)))
    (set-first! x1 x4)
    (set-first! x3 x3)
    (set-rest! x3 x4)
    (set-first! x4 x5)
    x6))
(define (traverse-one x6) (symbol=? 'y (first (rest (rest x6)))))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
