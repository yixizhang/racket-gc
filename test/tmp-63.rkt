#lang plai/gc2/mutator
(allocator-setup "coll.rkt" 200)
(define (build-one)
  (let* ((x0 #f)
         (x1 'y)
         (x2 #f)
         (x3 #t)
         (x4 (cons #f #f))
         (x5 (lambda (x) (if (= x 0) x4 (if (= x 1) x1 (if (= x 2) x3 x3)))))
         (x6
          (lambda (x)
            (if (= x 0)
              x4
              (if (= x 1)
                x1
                (if (= x 2)
                  x2
                  (if (= x 3)
                    x2
                    (if (= x 4)
                      x0
                      (if (= x 5)
                        x0
                        (if (= x 6)
                          x5
                          (if (= x 7) x5 (if (= x 8) x3 x4)))))))))))
         (x7 (lambda (x) (if (= x 0) x3 x1)))
         (x8 (cons x2 x7)))
    (set-first! x4 x5)
    (set-rest! x4 x6)
    x4))
(define (traverse-one x4) (symbol=? 'y ((first ((rest ((rest x4) 9)) 9)) 1)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
