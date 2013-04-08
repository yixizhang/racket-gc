#lang plai/gc2/mutator
(allocator-setup "../incr-struct.rkt" 100)

(define-struct s (x y))
(define-struct (t s) (w z))
(define-struct (u t) (a b c))
(define s-i (make-s 1 2))