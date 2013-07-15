#lang plai/gc2/mutator
(allocator-setup "../collector.rkt" 1024)

(define-struct s (x))
(define (f x)
  (cond
    [(= x 0) empty]
    [else (let ([ss (make-s x)])
            (cond
              [(s? ss)
               (begin
                 (set-s-x! ss 0)
                 (cons (s-x ss)
                       (f (- x 1))))]
              [else
               (printf "ss: ~s is not a s" ss)]))]))
(define (build-one) empty)
(define (traverse-one x1) (empty? x1))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (f 10) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 5)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 5)
