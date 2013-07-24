#lang plai/gc2/mutator
(allocator-setup "collector.rkt" 512)
(import-primitives not)
(require "dep1.rkt")
(require "dep0.rkt")

(define-struct (w p) (g))
(define wi (make-w 0 0 0 0 0))
(w-g wi)
(s? wi)
(make-vector 2 (make-vector 2 0))
(not 1)
