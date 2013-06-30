#lang plai/gc2/mutator

(allocator-setup "collector.rkt" 512)

(require "test-struct-0.rkt")

(define-struct (p t) (text))
(define p-i (make-p 0 0 0 0))
(s? p-i)
(s-x p-i)
