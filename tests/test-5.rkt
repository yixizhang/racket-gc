#lang plai/gc2/mutator
(allocator-setup "collector.rkt" 512)

(define-struct s (x))
(define (f x)
  (cond
    [(= x 0) empty]
    [else (cons (s-x (make-s x))
                (f (- x 1)))]))

(f 3)
