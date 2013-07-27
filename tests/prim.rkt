#lang plai/gc2/mutator
(allocator-setup "collector.rkt" 20)

(if #t 0 1)
(if '(0) 0 1)
(and)
(and 1)
(and #t)
(and '(0 1))
(and 1 2)
(and #t 2)
(and #t '(0))
(and #t '(0 1))
(and '(0 1) #t)
