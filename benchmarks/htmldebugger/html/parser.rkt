#lang plai/gc2/mutator
(allocator-setup "../../collector.rkt" 10240)
(require "html.rkt")
(import-primitives
 open-input-file)

(define (loop i)
  (if (zero? i)
    'passed
    (begin
      (read-html-as-xml (open-input-file "1.html"))
      (loop (- i 1)))))
(loop 80)