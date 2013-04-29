#lang racket
(require "1.rkt")
a
(define (f c)
  (cond
    [c
     1
     2]
    [else
     3
     4]))

(define-syntax (mutator-format stx)
  (syntax-case stx ()
    [(_ form v ...)
     #`(f (format form
                  #,@(for/list ([vs (in-list (syntax->list #`(v ...)))])
                       #`(+ #,vs 1))))]))

(mutator-format "~a" 1)
(char-whitespace? #\ )
(format "~s" 100)
(byte-regexp? #rx#"^[^&<]*")

;; call/cc
(define (search wanted? lst)
  (call/cc 
   ;; call/ec : proc -> any
   ;; like call/cc, but proc isn't called in the tail position,
   ;; and the continuation procedure supplied to proc can only be
   ;; called during the dynamic extend of the call/ec call.
   (lambda (return)
     (for-each (lambda (x)
                 (when (wanted? x)
                   (return x)))
               lst)
     #f)))
(search char? '(1 #\a 2))
(search char? '(1 2 3))

(define return #f)
(+ 1 (let/cc cont
       (set! return cont)
       1))
(return 22)

(let/ec out
  (let loop ([matched 0] [out out] [in '(1 #\a #\b 2)])
    (let* ([matched (add1 matched)])
      (cond
        [(null? in) (out null)]
        [else 
         (let ([c (car in)])
           (printf "~s " matched)
           (printf "~s\n" c)
           (cons c (loop matched out (cdr in))))]))))

(let loop ([matched 0] [in '(1 #\a #\b 2)])
  (let* ([matched (add1 matched)])
    (cond
      [(null? in) null]
      [else 
       (let ([c (car in)])
         (printf "~s " matched)
         (printf "~s\n" c)
         (cons c (loop matched (cdr in))))])))