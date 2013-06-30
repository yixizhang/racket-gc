#lang plai/gc2/mutator
(allocator-setup "collector.rkt" 512)

(+ (cons 1 2) 3)