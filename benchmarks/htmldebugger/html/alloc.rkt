#lang plai/gc2/mutator
(allocator-setup "../../collector.rkt" 30)
(import-primitives
 identity)

'(1 2)
(identity '(1 2))
'(1 2)