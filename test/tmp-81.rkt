#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 #t)
         (x1 (lambda (x) (if (= x 0) x0 (if (= x 1) x0 (if (= x 2) x0 x0)))))
         (x2 (cons #f #f))
         (x3 (cons #f #f))
         (x4
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1)
                x2
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x1
                    (if (= x 4)
                      x0
                      (if (= x 5)
                        x2
                        (if (= x 6)
                          x3
                          (if (= x 7) x0 (if (= x 8) x1 x2)))))))))))
         (x5 (lambda (x) x1))
         (x6 'y)
         (x7 (cons #f x0))
         (x8 (lambda (x) (if (= x 0) x0 (if (= x 1) x4 (if (= x 2) x6 x2))))))
    (set-first! x2 x2)
    (set-rest! x2 x6)
    (set-first! x3 x3)
    (set-rest! x3 x3)
    (set-first! x7 x7)
    x8))
(define (traverse-one x8) (let ((res ((x8 1) 4))) (if (boolean? res) res #f)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
