#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 #f)
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
         (x2 (cons #f #f))
         (x3 #f)
         (x4
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3) x3 (if (= x 4) x2 (if (= x 5) x2 x3))))))))
         (x5 (cons x2 #f)))
    (set-first! x2 x3)
    (set-rest! x2 x2)
    (set-rest! x5 x5)
    x3))
(define (traverse-one x3) (if x3 #f #t))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
