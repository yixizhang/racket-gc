#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 1)
         (x1 0)
         (x2 'y)
         (x3 (lambda (x) (if (= x 0) x0 (if (= x 1) x2 (if (= x 2) x0 x2)))))
         (x4
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x3
                  (if (= x 3)
                    x1
                    (if (= x 4)
                      x0
                      (if (= x 5) x0 (if (= x 6) x2 (if (= x 7) x3 x0))))))))))
         (x5 -1)
         (x6 (cons #f x4))
         (x7 (lambda (x) (if (= x 0) x0 (if (= x 1) x2 x5))))
         (x8 (lambda (x) x5))
         (x9 #t))
    (set-first! x6 x7)
    x9))
(define (traverse-one x9) (let ((res x9)) (if (boolean? res) res #f)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
