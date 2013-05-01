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
(+ 1 (froz2))

;; experiments
(define froz3 10)
(define froz4 11)
(let ((x 0))
  (call/cc
   (λ (cc)
     (set! froz3 (λ ()
                   (cc 20)))))
  (set! x (add1 x))
  x)
(let ((x 0))
  (call/cc
   (λ (cc)
     (set! froz4 (λ ()
                   (cc (+ 20 1))))
     (set! x (add1 x))
     x)))
(froz3)
(froz3)
(froz4)

;; some more
(define froz5 '())
(define froz6 '())
(define (ff lst)
  (cond
    [(null? lst) 'done]
    [else (call/cc
           (λ (cc)
             (set! froz5 (λ () 
                           (cc (ff (cdr lst)))))
             (car lst)))])) ;; set! froz5 to (λ () (ff (cdr lst)))
(define (ff/f lst)
  (let ((x (ff lst)))
    (cond ((eq? x 'done) '())
          (else (print x)
                (froz5)))))
(define (ff1 lst)
  (cond
    [(null? lst) 'done]
    [else (call/cc
           (λ (cc)
             (set! froz6 (λ ()
                           (cc (ff1 (cdr lst)))))))
          (car lst)])) ;; set! froz6 to (λ () (car lst))
(ff/f '(1 2 3))
(ff1 '(1 2 3))
(froz6)

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
;; explanation:
;; the continuation *saved* stores is like
;; (let ([node (cc)])
;;    (cond
;;      [(eq? node 'done) '()]
;;      [else (print node) (restart)]))
;; and inside cc it's like
;; (cond
;;   [(null? tree) (restart)]
;;   [(not (pair? tree)) tree]
;;   [else ...])
;; so the call of continuation causes recursion

;; continuation-passing macros
(define *cont* identity)

(define-syntax-rule (=lambda (parm ...) body ...)
  (lambda (*cont* parm ...) body ...))

(define-syntax (=define stx)
  (syntax-case stx ()
    [(_ (name parm ...) body ...)
     (with-syntax ([f (datum->syntax #'name 
                                     (string->symbol (format "=~a" (syntax-e #'name))))])
       #`(begin
           (define-syntax-rule (name parm ...)
             (f *cont* parm ...))
           (define (f *cont* parm ...) body ...)))]))

(define-syntax-rule (=bind ([(parm ...) expr]) body ...)
  (let ([*cont* (lambda (parm ...) body ...)]) expr))

(define (funcall fn . args)
  (apply fn args))

(define-syntax-rule (=values val ...)
  (funcall *cont* val ...))

(define-syntax-rule (=funcall fn val ...)
  (funcall fn *cont* val ...))

(define-syntax-rule (=apply fn arg ...)
  (apply fn *cont* arg ...))

;; macro test

;; =values
(=values (add1 1))
;; =bind
(=define (add/one n) (=values (add1 n)))
(let ([fn (=lambda (n) (add/one n))])
  (=bind ([(y) (=funcall fn 9)])
         (format "9+1=~a" y))) ;; > "9+1=10"
;; =define & =values
#|
(=define (bar x)
         (=values (list 'a (add1 x))))
(bar 5)
(=define (message)
         (=values 'hello 'there))
(=define (baz)
         (=bind ([(m n) (message)])
                (=values (list m n))))
(baz) ;; > '(hello there)
|#

;; replace call/cc with cps
(set! *saved* null)
(=define (dft-node1 tree)
         (cond
           [(null? tree) (restart1)]
           [(not (pair? tree)) (=values tree)]
           [else (set! *saved*
                       (cons (lambda () (dft-node1 (cdr tree)))
                             *saved*))
                 (dft-node1 (car tree))]))
(=define (restart1)
         (cond
           [(null? *saved*) (=values 'done)]
           [else (let ([cont (car *saved*)])
                   (set! *saved* (cdr *saved*))
                   (cont))]))
(=define (dft3 tree)
         (set! *saved* null)
         (=bind ([(node) (dft-node1 tree)])
                (cond
                  [(eq? node 'done) (=values null)]
                  [else (print node)
                        (restart1)])))