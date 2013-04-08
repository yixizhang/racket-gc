#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 #t)
         (x1 -1)
         (x2
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1)
                x0
                (if (= x 2)
                  x1
                  (if (= x 3)
                    x0
                    (if (= x 4)
                      x1
                      (if (= x 5)
                        x1
                        (if (= x 6)
                          x1
                          (if (= x 7) x1 (if (= x 8) x0 x0)))))))))))
         (x3 (cons #f x0))
         (x4 1)
         (x5 0)
         (x6 #f)
         (x7
          (lambda (x)
            (if (= x 0)
              x2
              (if (= x 1)
                x0
                (if (= x 2) x1 (if (= x 3) x1 (if (= x 4) x5 x0))))))))
    (set-first! x3 x5)
    x7))
(define (traverse-one x7) (= -1 ((x7 0) 5)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
