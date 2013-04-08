#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 'y)
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
         (x2 #f)
         (x3 (lambda (x) (if (= x 0) x0 (if (= x 1) x0 x2))))
         (x4 (lambda (x) (if (= x 0) x2 x1))))
    x4))
(define (traverse-one x4) (if (x4 0) #f #t))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
