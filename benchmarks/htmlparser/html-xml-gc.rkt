#lang plai/gc2/mutator
(allocator-setup "../collector.rkt" 10240)
(require "html/html.rkt")

(call-with-input-file (vector-ref (current-command-line-arguments) 0)
  (lambda (port)
    (read-html-as-xml port)))
