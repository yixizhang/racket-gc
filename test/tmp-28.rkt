#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 'x)
         (x1 (cons x0 #f))
         (x2 (cons #f #f))
         (x3 0)
         (x4 (lambda (x) (if (= x 0) x0 x3)))
         (x5 (lambda (x) (if (= x 0) x2 x4)))
         (x6
          (lambda (x)
            (if (= x 0)
              x3
              (if (= x 1)
                x5
                (if (= x 2)
                  x5
                  (if (= x 3) x1 (if (= x 4) x3 (if (= x 5) x1 x0))))))))
         (x7
          (lambda (x)
            (if (= x 0)
              x2
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x2
                    (if (= x 4)
                      x4
                      (if (= x 5)
                        x0
                        (if (= x 6)
                          x0
                          (if (= x 7) x5 (if (= x 8) x3 x6)))))))))))
         (x8 1))
    (set-rest! x1 x1)
    (set-first! x2 x3)
    (set-rest! x2 x2)
    x8))
(define (traverse-one x8) (= 1 x8))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
