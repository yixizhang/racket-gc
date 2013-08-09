#lang plai/gc2/mutator
(allocator-setup "collector.rkt" 512)

(define (f b) 17)
(define (g l f)
  ((lambda (x) (f (first l)))
   1))
(g '(3) f)
