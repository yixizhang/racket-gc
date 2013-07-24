#lang plai/gc2/mutator
(allocator-setup "../collector.rkt" 2048)
(require "html/html.rkt")
(import-primitives
 open-input-file
 current-command-line-arguments)
;(vector-ref (current-command-line-arguments) 0)
(read-html-as-xml 
  (open-input-file
    "0.html"))