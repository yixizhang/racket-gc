#lang scheme
(require (for-syntax racket/syntax))
(provide (all-defined-out))

(define-syntax (define-collector-export stx)
  (syntax-case stx ()
    [(_ i)
     (with-syntax 
      ([collector:i (format-id #'i "collector:~a" #'i)]
       [set-collector:i! (format-id #'i "set-collector:~a!" #'i)])
      #'(begin (define collector:i false)
               (define (set-collector:i! mod-name proc)
                 (check-unset-or-same 'set-collector:i mod-name collector:i proc)
                 (set! collector:i proc))))]))

(define (check-unset-or-same who mod-name old new)
  (when old
    (unless (equal? old new)
      (error who "tried to set to ~s, but was already set to a different function: ~s, new one from ~s"
             new old mod-name))))

(define-syntax-rule (define-collector-exports i ...)
  (begin (define-collector-export i)
         ...))

(define-collector-exports
  deref
  alloc-flat
  cons
  first
  rest
  flat?
  cons?
  set-first!
  set-rest!
  closure
  closure?
  closure-code-ptr
  closure-env-ref
  vector
  vector?
  vector-length
  vector-ref
  vector-set!
  alloc-struct
  alloc-struct-instance
  struct-pred
  struct-select)
