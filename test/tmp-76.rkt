#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 #t)
         (x1 (cons #f #f))
         (x2 (cons x1 x1))
         (x3
          (lambda (x)
            (if (= x 0)
              x2
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x1
                    (if (= x 4)
                      x2
                      (if (= x 5) x0 (if (= x 6) x1 (if (= x 7) x0 x1))))))))))
         (x4 (cons x0 x2)))
    (set-first! x1 x1)
    (set-rest! x1 x4)
    x3))
(define (traverse-one x3)
  (let ((res (first (rest (x3 8))))) (if (boolean? res) res #f)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
