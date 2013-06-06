#lang plai/gc2/mutator
(allocator-setup "../generational-bm.rkt" 5096)
(require racket/pretty)
(require "libs/html/html.rkt")

(with-handlers ([exn:fail?
                  (lambda (exn)
                    (pretty-print exn))])
               (call-with-input-file (vector-ref (current-command-line-arguments) 0)
                                     (lambda (port)
                                       (read-html-as-xml port))))
