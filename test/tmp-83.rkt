#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 -1)
         (x1 (cons #f #f))
         (x2
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x1
                (if (= x 2)
                  x1
                  (if (= x 3)
                    x0
                    (if (= x 4)
                      x1
                      (if (= x 5) x0 (if (= x 6) x1 (if (= x 7) x0 x1))))))))))
         (x3 (cons #f #f))
         (x4 #t)
         (x5 (cons #f x4))
         (x6 empty)
         (x7 (cons x0 x1)))
    (set-first! x1 x6)
    (set-rest! x1 x3)
    (set-first! x3 x5)
    (set-rest! x3 x7)
    (set-first! x5 x6)
    x5))
(define (traverse-one x5) (let ((res (rest x5))) (if (boolean? res) res #f)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
