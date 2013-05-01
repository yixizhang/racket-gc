#lang racket
(require "1.rkt")
a
(define (f c)
  (cond
    [c 1 2]
    [else 3 4]))

(define-syntax (mutator-format stx)
  (syntax-case stx ()
    [(_ form v ...)
     #`(format form
               #,@(for/list ([vs (in-list (syntax->list #`(v ...)))])
                    #`(+ #,vs 1)))]))

(mutator-format "~a" 1)
(char-whitespace? #\ )
(format "~s" 100)
(byte-regexp? #rx#"^[^&<]*")