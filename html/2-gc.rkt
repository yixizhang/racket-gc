#lang plai/gc2/mutator

(allocator-setup "../incr-struct.rkt" 200)

(require "1-gc.rkt")

(define b 2)
b
(define c 3)
c

a