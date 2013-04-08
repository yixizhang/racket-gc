#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 #t)
         (x1
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x0
                (if (= x 2)
                  x0
                  (if (= x 3)
                    x0
                    (if (= x 4)
                      x0
                      (if (= x 5)
                        x0
                        (if (= x 6)
                          x0
                          (if (= x 7) x0 (if (= x 8) x0 x0)))))))))))
         (x2 -1)
         (x3
          (lambda (x)
            (if (= x 0)
              x1
              (if (= x 1)
                x1
                (if (= x 2)
                  x1
                  (if (= x 3)
                    x2
                    (if (= x 4)
                      x0
                      (if (= x 5)
                        x0
                        (if (= x 6)
                          x2
                          (if (= x 7) x2 (if (= x 8) x0 x0)))))))))))
         (x4 empty)
         (x5
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1)
                x1
                (if (= x 2)
                  x2
                  (if (= x 3) x1 (if (= x 4) x1 (if (= x 5) x1 x3))))))))
         (x6 #t))
    x5))
(define (traverse-one x5) (let ((res ((x5 3) 8))) (if (boolean? res) res #f)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
