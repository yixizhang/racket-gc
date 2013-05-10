#lang racket

(define (collector:alloc-struct s parent num) (void))
(define (collector:alloc-struct-instance struct field-vals) (void))
(define (collector:struct-pred struct x) #f)
(define (collector:struct-select struct struct-instance f) (void))

(define-syntax (define-struct/offset stx)
  (syntax-case stx ()
    [(_ base s (x ...))
     (let* ([base-num (syntax-e #'base)]
            [fields (syntax->list #'(x ...))]
            [fields-num (length fields)]
            [index-list (build-list fields-num (λ (x) (+ x base-num)))])
       (with-syntax ([struct:s (datum->syntax #'s (string->symbol
                                                   (format "struct:~a" (syntax-e #'s))))]
                     [make-s (datum->syntax #'s (string->symbol (format "make-~a" (syntax-e #'s))))]
                     [s? (datum->syntax #'s (string->symbol (format "~a?" (syntax-e #'s))))]
                     [(s-x ...) (map (lambda (x)
                                       (datum->syntax #'s
                                                      (string->symbol (format "~a-~a"
                                                                              (syntax-e #'s)
                                                                              (syntax-e x)))))
                                     fields)])
         #`(begin
             (define struct:s 
               (collector:alloc-struct 's #f #,(+ base-num fields-num)))
             (define (make-s x ...)
               (collector:alloc-struct-instance struct:s (vector x ...)))
             (define (s? a)
               (collector:struct-pred struct:s a))
             #,@(for/list ([i index-list]
                           [f (in-list (syntax->list #'(s-x ...)))])
                  #`(define (#,f a)
                      (collector:struct-select struct:s a #,i))))))]))

(begin-for-syntax
  (define-struct define-struct-info (num)))

(define-syntax (mutator-define-struct stx)
  (syntax-case stx ()
    [(_ s (f ...))
     (identifier? #'s)
     (begin
       (for ([x (in-list (syntax->list #'(f ...)))])
         (unless (identifier? x)
           (raise-syntax-error 'define-struct "expected an identifier" stx x)))
       #`(begin
           (define-syntax s #,(make-define-struct-info (length (syntax->list #'(f ...)))))
           (define-struct/offset 0 s (f ...))))]
    [(_ (s t) (f ...))
     (begin
       (unless (identifier? #'s)
         (raise-syntax-error 'define-struct "expected an identifier" stx #'s))
       (unless (and (identifier? #'t)
                    (define-struct-info? (syntax-local-value #'t (λ () #f))))
         (raise-syntax-error 'define-struct "expected an identifier from an earlier define-struct" stx #'t))
       (for ([x (in-list (syntax->list #'(f ...)))])
         (unless (identifier? x)
           (raise-syntax-error 'define-struct "expected an identifier" stx x)))
       (let ([base (define-struct-info-num (syntax-local-value #'t))])
         #`(begin
             (define-syntax s #,(make-define-struct-info (+ base (length (syntax->list #'(f ...))))))
             (define-struct/offset #,base s (f ...)))))]))

(mutator-define-struct s (x y))
(mutator-define-struct (t s) (w z))
(mutator-define-struct (u t) (a b c))
