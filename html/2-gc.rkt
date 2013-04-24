#lang plai/gc2/mutator

(allocator-setup "../incr-struct.rkt" 200)

(require "1-gc.rkt")

(define b 2)
b

a
(define-struct (s t) (text))
make-t