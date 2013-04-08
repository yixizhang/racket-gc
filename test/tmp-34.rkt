#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 'x)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1) x0 (if (= x 2) x0 (if (= x 3) x0 x0))))))
         (x2 'y)
         (x3
          (lambda (x)
            (if (= x 0)
              x2
              (if (= x 1) x0 (if (= x 2) x1 (if (= x 3) x1 x2))))))
         (x4 (cons x2 x3))
         (x5
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x4
                (if (= x 2)
                  x3
                  (if (= x 3)
                    x0
                    (if (= x 4)
                      x0
                      (if (= x 5) x1 (if (= x 6) x4 (if (= x 7) x1 x4))))))))))
         (x6 (cons x3 #f))
         (x7
          (lambda (x)
            (if (= x 0)
              x6
              (if (= x 1)
                x2
                (if (= x 2)
                  x3
                  (if (= x 3)
                    x6
                    (if (= x 4)
                      x5
                      (if (= x 5) x4 (if (= x 6) x6 (if (= x 7) x0 x0))))))))))
         (x8 (cons x7 #f))
         (x9 (cons x3 x2)))
    (set-rest! x6 x8)
    (set-rest! x8 x8)
    x8))
(define (traverse-one x8)
  (symbol=? 'y ((((first (rest ((first (rest x8)) 6))) 4) 2) 0)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
