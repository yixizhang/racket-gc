#lang plai/gc2/mutator
(allocator-setup "collector.rkt" 512)
(require "dep0.rkt")

(make-vector 2 (make-vector 2 0))
