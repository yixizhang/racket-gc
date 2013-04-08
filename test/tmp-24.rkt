#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 'x)
         (x1 #f)
         (x2 #t)
         (x3 (cons #f x1))
         (x4
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1)
                x1
                (if (= x 2)
                  x3
                  (if (= x 3)
                    x2
                    (if (= x 4)
                      x2
                      (if (= x 5) x0 (if (= x 6) x0 (if (= x 7) x2 x0))))))))))
         (x5 (cons x1 #f))
         (x6 (cons #f x3))
         (x7 (lambda (x) x0)))
    (set-first! x3 x6)
    (set-rest! x5 x7)
    (set-first! x6 x6)
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
