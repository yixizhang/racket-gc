#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 60)
(define (build-one)
  (let* ((x0 'y)
         (x1 'x)
         (x2 #t)
         (x3 #t)
         (x4
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1)
                x1
                (if (= x 2)
                  x3
                  (if (= x 3)
                    x0
                    (if (= x 4)
                      x3
                      (if (= x 5) x0 (if (= x 6) x2 (if (= x 7) x3 x0))))))))))
         (x5
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x1
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x0
                    (if (= x 4) x3 (if (= x 5) x4 (if (= x 6) x1 x3)))))))))
         (x6
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1)
                x5
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x2
                    (if (= x 4)
                      x0
                      (if (= x 5)
                        x1
                        (if (= x 6)
                          x5
                          (if (= x 7) x0 (if (= x 8) x1 x5))))))))))))
    x2))
(define (traverse-one x2) (let ((res x2)) (if (boolean? res) res #f)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
