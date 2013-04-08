#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 'x)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2) x0 (if (= x 3) x0 (if (= x 4) x0 x0)))))))
         (x2 'y)
         (x3 (cons x2 #f))
         (x4
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1)
                x2
                (if (= x 2)
                  x1
                  (if (= x 3)
                    x0
                    (if (= x 4)
                      x0
                      (if (= x 5)
                        x2
                        (if (= x 6) x2 (if (= x 7) x0 x1)))))))))))
    (set-rest! x3 x3)
    x4))
(define (traverse-one x4) (symbol=? 'x ((x4 2) 2)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
