#lang plai/gc2/mutator
(allocator-setup "../incr-struct.rkt" 100)

(provide (all-defined-out))

(define-struct s (x))
(define-struct (t s) (w z))
(define-struct (u t) (a b c))
(define s-i (make-s 1))
(define t-i (make-t 1 1 1))