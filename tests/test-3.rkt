#lang plai/gc2/mutator
(allocator-setup "collector.rkt" 512)
(require "util.rkt")

(for-each (lambda (x)
            (printf "~s\n" (+ x 1)))
          '(0 1 2 3))

(define (gen-may-contain spec)
  (let ([table (make-hash)])
    (for-each (lambda (def)
                (let ([rhs (rest def)])
                  (for-each (lambda (name) (hash-set! table name rhs))
                            (first def))))
              spec)
    (lambda (name)
      (hash-ref table name (lambda () #f)))))
(define may-contain-anything
  (gen-may-contain empty))
(may-contain-anything '(1))