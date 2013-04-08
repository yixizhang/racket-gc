#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 1)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2) x0 (if (= x 3) x0 (if (= x 4) x0 x0)))))))
         (x2 (cons #f #f))
         (x3 #t)
         (x4 (cons x0 #f))
         (x5
          (lambda (x)
            (if (= x 0)
              x4
              (if (= x 1)
                x2
                (if (= x 2)
                  x4
                  (if (= x 3)
                    x4
                    (if (= x 4) x1 (if (= x 5) x1 (if (= x 6) x1 x2)))))))))
         (x6 (cons x3 #f))
         (x7 'y)
         (x8 (lambda (x) (if (= x 0) x3 x1)))
         (x9
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1) x7 (if (= x 2) x5 (if (= x 3) x5 x5)))))))
    (set-first! x2 x7)
    (set-rest! x2 x5)
    (set-rest! x4 x6)
    (set-rest! x6 x6)
    x9))
(define (traverse-one x9) (= 1 (first ((x9 2) 3))))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
