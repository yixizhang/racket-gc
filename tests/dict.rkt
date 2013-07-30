#lang plai/gc2/mutator
(allocator-setup "collector.rkt" 512)
(require "hash.rkt")

(define hash (make-hash))
(hash-set! hash 0 0)
(hash-ref hash 0 (lambda () #f))
(hash-set! hash 1 1)
(hash-ref hash 1 (lambda () #f))
(hash-set! hash 1 10)
(hash-ref hash 1 (lambda () #f))
