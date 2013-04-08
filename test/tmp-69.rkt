#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 #t)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x0
                    (if (= x 4) x0 (if (= x 5) x0 (if (= x 6) x0 x0)))))))))
         (x2 1)
         (x3 (cons #f #f))
         (x4 (lambda (x) (if (= x 0) x2 x1)))
         (x5
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1) x0 (if (= x 2) x4 (if (= x 3) x3 x2))))))
         (x6
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1)
                x4
                (if (= x 2)
                  x4
                  (if (= x 3)
                    x2
                    (if (= x 4)
                      x1
                      (if (= x 5) x1 (if (= x 6) x1 (if (= x 7) x5 x1))))))))))
         (x7 (cons x4 #f)))
    (set-first! x3 x4)
    (set-rest! x3 x5)
    (set-rest! x7 x7)
    x6))
(define (traverse-one x6) (= 1 ((x6 1) 0)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
