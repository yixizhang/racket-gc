#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 #f)
         (x1 (cons #f #f))
         (x2
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x1
                    (if (= x 4)
                      x1
                      (if (= x 5) x1 (if (= x 6) x1 (if (= x 7) x1 x0))))))))))
         (x3 'x)
         (x4 empty)
         (x5 (cons #f #f))
         (x6 'x)
         (x7
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x5
                (if (= x 2)
                  x6
                  (if (= x 3)
                    x1
                    (if (= x 4)
                      x5
                      (if (= x 5)
                        x4
                        (if (= x 6)
                          x4
                          (if (= x 7) x5 (if (= x 8) x3 x2)))))))))))
         (x8 (cons x2 x0))
         (x9 (cons x0 x5)))
    (set-first! x1 x3)
    (set-rest! x1 x1)
    (set-first! x5 x7)
    (set-rest! x5 x6)
    x5))
(define (traverse-one x5) (symbol=? 'x ((first ((first x5) 4)) 8)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
