#lang plai/gc2/mutator
(allocator-setup "incr.rkt" 200)

(define p (current-input-port))
p
(read-char p)