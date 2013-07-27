#lang plai/gc2/mutator
(allocator-setup "../../collector.rkt" 2048)
(require "html.rkt")
(import-primitives
 open-input-file
 current-command-line-arguments)

(read-html-as-xml (open-input-file
                    (vector-ref (current-command-line-arguments) 0)))
