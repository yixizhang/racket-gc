#lang plai/gc2/mutator
(allocator-setup "../generational.rkt" 200)
(require "test-struct-0.rkt")

(make-vector 2 (make-vector 2 0))