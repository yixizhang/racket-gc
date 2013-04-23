#lang plai/gc2/mutator

(allocator-setup "../incr-struct.rkt" 200)

(provide (all-defined-out))

(define a 1)
17