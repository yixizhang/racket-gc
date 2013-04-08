#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 0)
         (x1 0)
         (x2 (cons #f x0))
         (x3 1)
         (x4 -1)
         (x5
          (lambda (x)
            (if (= x 0)
              x2
              (if (= x 1)
                x3
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x2
                    (if (= x 4)
                      x4
                      (if (= x 5)
                        x4
                        (if (= x 6)
                          x0
                          (if (= x 7) x0 (if (= x 8) x3 x3)))))))))))
         (x6
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x4
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x1
                    (if (= x 4) x0 (if (= x 5) x1 (if (= x 6) x4 x3))))))))))
    (set-first! x2 x5)
    x2))
(define (traverse-one x2) (= -1 ((first ((first ((first x2) 3)) 0)) 5)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
