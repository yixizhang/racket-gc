#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 'y)
         (x1 (cons #f x0))
         (x2
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1) x1 (if (= x 2) x0 (if (= x 3) x1 x0))))))
         (x3
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1) x1 (if (= x 2) x1 (if (= x 3) x1 x1))))))
         (x4 (lambda (x) (if (= x 0) x2 (if (= x 1) x3 x3))))
         (x5 (cons x3 #f))
         (x6
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1)
                x3
                (if (= x 2)
                  x3
                  (if (= x 3)
                    x1
                    (if (= x 4)
                      x2
                      (if (= x 5) x5 (if (= x 6) x4 (if (= x 7) x5 x1))))))))))
         (x7 (cons x4 x3))
         (x8 #t)
         (x9 'x))
    (set-first! x1 x7)
    (set-rest! x5 x9)
    x8))
(define (traverse-one x8) (let ((res x8)) (if (boolean? res) res #f)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
