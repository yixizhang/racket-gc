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
   (lambda (cc)
     (for-each (lambda (x)
                 (when (wanted? x)
                   (cc x)))
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
  (let loop ([matched 0] [out out] [in '(#\  #\a #\b)])
    (cond
      [(null? in) (out null)]
      [(zero? matched) (cons (car in) (let/ec out (loop matched out (cdr in))))]
      [else 
       (let ([c (car in)])
         (printf "~s " matched)
         (printf "~s\n" c)
         (cons c (loop (add1 matched) out (cdr in))))])))

;; onlisp about continuation
(define frozen 'FRONZEN)
(append '(the call/cc returned)
        (list (call/cc
               (lambda (cc)
                 (set! frozen cc)
                 'a))))
(+ 1 (frozen 'again))
;; another one
(define froz1 1)
(define froz2 2)
(let ((x 0))
  (call/cc
   (λ (cc)
     (set! froz1 cc)
     (set! froz2 cc)))
  (set! x (add1 x))
  x)
(froz1 '())
(froz2)
;; dft
(define t1 '(a (b (d h)) (c e (f i) g)))
(define t2 '(1 (2 (3 6 7) 4 5)))
(define (dft tree)
  (cond
    [(null? tree) '()]
    [(not (pair? tree)) (print tree)]
    [else (dft (car tree))
          (dft (cdr tree))]))
(define *saved* '())
(define (dft-node tree)
  (cond
    [(null? tree) (restart)]
    [(not (pair? tree)) tree]
    [else (call/cc
           (λ (cc)
             (set! *saved*
                   (cons (λ ()
                           (cc (dft-node (cdr tree))))
                         *saved*))
             (dft-node (car tree))))]))
(define (restart)
  (if (null? *saved*)
      'done
      (let ([cont (car *saved*)])
        (set! *saved* (cdr *saved*))
        (cont))))
(define (dft2 tree)
  (set! *saved* '())
  (let ([node (dft-node tree)])
    (cond
      [(eq? node 'done) '()]
      [else (print node)
            (restart)])))