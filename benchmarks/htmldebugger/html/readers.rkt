#lang plai/gc2/mutator
(allocator-setup "../../collector.rkt" 10240)
(require "sgml-reader.rkt")
(require "html-spec.rkt")
(import-primitives
 current-command-line-arguments
 length
 open-input-file)

(define (implicit-starts parent child)
  (or (and (eq? child 'tr) (eq? parent 'table) 'tbody)
      (and (eq? child 'td) (memq parent '(table tbody tfoot thead)) 'tr)))

(length
  (read-from-port (gen-may-contain html-spec)
                  implicit-starts
                  (open-input-file
                    (vector-ref (current-command-line-arguments) 0))))
