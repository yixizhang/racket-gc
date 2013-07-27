#lang plai/gc2/mutator
(allocator-setup "../../collector.rkt" 512)
(import-primitives
 eof-object?)

(define-struct s (x))
(eof-object? (make-s 0))
(define (char-whitespace? c)
  (equal? c #\ ))
(char-whitespace? #\ )
(eq? #\c #\c)
