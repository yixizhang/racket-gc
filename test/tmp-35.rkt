#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 empty)
         (x1 0)
         (x2
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x1
                  (if (= x 3)
                    x1
                    (if (= x 4)
                      x1
                      (if (= x 5)
                        x0
                        (if (= x 6)
                          x1
                          (if (= x 7) x0 (if (= x 8) x0 x1)))))))))))
         (x3 (cons x2 #f))
         (x4 (cons #f x1))
         (x5 (cons #f x0))
         (x6
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1)
                x2
                (if (= x 2)
                  x2
                  (if (= x 3)
                    x1
                    (if (= x 4)
                      x2
                      (if (= x 5) x4 (if (= x 6) x0 (if (= x 7) x3 x4))))))))))
         (x7 (lambda (x) x5))
         (x8 (lambda (x) x5)))
    (set-rest! x3 x3)
    (set-first! x4 x5)
    (set-first! x5 x5)
    x5))
(define (traverse-one x5) (empty? (rest x5)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
