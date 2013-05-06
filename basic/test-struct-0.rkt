#lang plai/gc2/mutator
(allocator-setup "../generational.rkt" 200)

(provide (all-defined-out))

(define-struct s (x))
(define-struct (t s) (w z))
(define-struct (u t) (a b c))
(define s-i (make-s 1))
(define u-i (make-u 1 1 1 1 1 1))
(s-x u-i)
(t? u-i)