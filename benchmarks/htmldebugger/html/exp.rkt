#lang racket

;; local struct
(define sc (make-hash))
(define-syntax (mk-s stx)
  (syntax-case stx ()
    [(_ s (x ...) sc)
     #`(begin
         (struct s (x ...))
         (hash-set! sc
                    (format "~a" s)
                    s))]))
(mk-s s (w) sc)
sc
(struct local:t (x))
(local:t 0)
(define-syntax (make-local-struct stx)
  (syntax-case stx ()
    [(_ s x)
     (with-syntax ([local-s (datum->syntax #'s
                                           (string->symbol
                                            (format "local:~a" 
                                                    (syntax->datum #'s))))])
       #`(apply local-s x))]))
(make-local-struct t '(0))

;; apply implementation
(define (f . args)
  (for ([i (in-list args)])
    (printf "~s\n" i)))
(f 0)
(define-syntax (app stx)
  (syntax-case stx ()
    [(_ f args)
     (with-syntax ([(arg ...) (for/list ([i (in-naturals)] [v (in-list (syntax->datum #'args))])
                                (datum->syntax #'s v))])
       #`(f arg ...))]))
(app + (1 2 3))
(define-syntax (app1 stx)
  (syntax-case stx ()
    [(_ f args)
     #`(f . args)]))
(app1 + (1 2 3))