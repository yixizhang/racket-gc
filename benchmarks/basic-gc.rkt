#lang plai/gc2/mutator
(allocator-setup "../hybrid.rkt" 400)
(require "libs/html/html.rkt")

(define in (open-input-file "basic.html"))
(read-html-as-xml in)