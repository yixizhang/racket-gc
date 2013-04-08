#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 empty)
         (x1 (lambda (x) x0))
         (x2 (lambda (x) x0))
         (x3
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1)
                x0
                (if (= x 2)
                  x2
                  (if (= x 3)
                    x1
                    (if (= x 4)
                      x2
                      (if (= x 5) x0 (if (= x 6) x2 (if (= x 7) x0 x1))))))))))
         (x4 (cons x1 #f))
         (x5 -1)
         (x6 (lambda (x) (if (= x 0) x2 x1)))
         (x7 (lambda (x) (if (= x 0) x3 (if (= x 1) x1 (if (= x 2) x1 x5))))))
    (set-rest! x4 x5)
    x7))
(define (traverse-one x7) (empty? ((x7 0) 7)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
