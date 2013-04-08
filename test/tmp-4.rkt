#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 0)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1) x0 (if (= x 2) x0 (if (= x 3) x0 x0))))))
         (x2 (lambda (x) x0))
         (x3 'y)
         (x4 'x)
         (x5 (cons #f #f))
         (x6 (cons #f x1))
         (x7 empty)
         (x8
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x5
                (if (= x 2)
                  x1
                  (if (= x 3)
                    x1
                    (if (= x 4)
                      x2
                      (if (= x 5) x7 (if (= x 6) x3 (if (= x 7) x0 x4))))))))))
         (x9 #f))
    (set-first! x5 x5)
    (set-rest! x5 x5)
    (set-first! x6 x7)
    x8))
(define (traverse-one x8) (symbol=? 'y (x8 6)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
