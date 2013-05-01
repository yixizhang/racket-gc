#lang plai/gc2/mutator

(allocator-setup "../incr-struct.rkt" 200)

(define (f c)
  (cond
    [c (begin 1 2)]
    [else (begin 3 4)]))

(f 1)

(format "~a" 1)

(procedure? (lambda (x) x))

'(1 2 3)