#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 'y)
         (x1 (cons #f #f))
         (x2 -1)
         (x3 empty)
         (x4 (lambda (x) (if (= x 0) x2 (if (= x 1) x0 x0))))
         (x5 (cons #f x4))
         (x6 'x)
         (x7
          (lambda (x)
            (if (= x 0)
              x5
              (if (= x 1)
                x0
                (if (= x 2)
                  x4
                  (if (= x 3)
                    x6
                    (if (= x 4)
                      x2
                      (if (= x 5) x2 (if (= x 6) x4 (if (= x 7) x2 x6))))))))))
         (x8
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x6
                (if (= x 2)
                  x1
                  (if (= x 3)
                    x2
                    (if (= x 4)
                      x5
                      (if (= x 5) x0 (if (= x 6) x6 (if (= x 7) x4 x4))))))))))
         (x9 (cons x1 x0)))
    (set-first! x1 x6)
    (set-rest! x1 x5)
    (set-first! x5 x8)
    x1))
(define (traverse-one x1)
  (symbol=? 'x ((first (rest ((first (rest x1)) 2))) 1)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
