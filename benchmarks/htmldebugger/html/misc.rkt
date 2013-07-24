#lang plai/gc2/mutator
(allocator-setup "../../collector.rkt" 2048)
(require "html-spec.rkt")
(require "hash.rkt")
(require "list.rkt")
(import-primitives
 read-char
 open-input-file open-input-string)

;; gen-may-contain : Spec -> Kid-lister
(define (gen-may-contain spec)
  (let ([table (make-hash)]) ;; use assoc-table (listof pairs)
    (for-each (lambda (def)
                (let ([rhs (rest def)])
                  (for-each (lambda (name) (hash-set! table name rhs))
                            (first def))))
              spec)
    (lambda (name)
      (hash-ref table name (lambda () #f)))))
(gen-may-contain html-spec)
(read-char (open-input-file "0.html"))
(read-char (open-input-string ""))