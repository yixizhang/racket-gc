#lang plai/gc2/mutator
(allocator-setup "collector.rkt" 512)
(require "list.rkt")
(import-primitives
 <)

(sort '(3 1 4 2) (lambda (a b) (< a b)))
(sort '((1 . 2) (2 . 1))
      (lambda (a b)
        (< (cdr a) (cdr b))))
