#lang plai/gc2/mutator
(allocator-setup "collector.rkt" 5120)
(require "libs/html/html.rkt")

(call-with-input-file (vector-ref (current-command-line-arguments) 0)
  (lambda (port)
    (read-html-as-xml port)))
