#lang plai/gc2/mutator
(allocator-setup "collector.rkt" 512)
(import-primitives
 current-command-line-arguments)

(define args (current-command-line-arguments))
(if (> (vector-length args) 0)
    (vector-ref args 0)
    (printf "no args\n"))