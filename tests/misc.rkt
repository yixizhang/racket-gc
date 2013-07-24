#lang plai/gc2/mutator
(allocator-setup "collector.rkt" 512)
(require "hash.rkt")
(import-primitives
  read-char peek-char exact-nonnegative-integer? read-string bytes->string/utf-8
  string->number string-append format string=? string<? string>?
  symbol->string list->string string-downcase string-length string-ref
  regexp-match regexp-replace* regexp-match-positions)

(define hash (make-hash))
(hash-set! hash 0 0)
(hash-ref hash 0 (lambda () (void)))
(hash-set! hash 1 1)
(hash-ref hash 1 (lambda () (void)))
(hash-set! hash 1 10)
(hash-ref hash 1 (lambda () (void)))

(not 100000)
(not '(1 2))
(define f0 (lambda () 0))
(not f0)
(not (make-vector 2 0))
(procedure? (lambda (x) (printf "~a\n" x)))
(procedure-arity-includes? (lambda () 0) 0)
(eq? (cons 0 0) (cons 0 0))
(eq? (make-vector 1 0) (make-vector 1 0))
(eq? 0 0)
(equal? (make-vector 1 0) (make-vector 1 0))
(equal? 1 "1")
(string->number "111")
(string->number "abc")
(string-append "1" "2")
(format "hello ~a" "world")
(string=? "a" "a")
(string<? "a" "b")
(symbol->string 'a)
(list->string '(#\a #\b))
(string-downcase "ABC")
(string-length "123")
(string-ref "123" 0)
(regexp-match #rx"x." "12x4x6")
(regexp-replace* "([Mm])i ([a-zA-Z]*)" "mi cerveza Mi Mi Mi" "\\1y \\2")
(regexp-match-positions #rx"x." "12x4x6")

(define local-even? 'undefined)
(define local-odd? 'undefined)
(define is-odd?
  (begin
    (set! local-even? (λ (n)
                        (or (= n 0)
                            (local-odd? (sub1 n)))))
    (set! local-odd? (λ (n)
                       (and (not (= n 0))
                            (local-even? (sub1 n)))))
    local-odd?))
(is-odd? 11)
(define a 1)
(define b 3)
(define (f x y)
  (when (< x y)
    (begin
      (printf "yes\n")
      (f (+ x 1) y))))
(f a b)
(define (f1 c)
  (cond
    [c (begin 1 2)]
    [else (begin 3 4)]))
(f1 1)
(define (loop x)
  (cond
    [(= x 0) 'pass]
    [else (loop (sub1 x))]))
(define (ff number)
  (loop number))
(ff 10)

(define (compose f g)
  (lambda (x)
    (f (g x))))
((compose (lambda (x) (* x 10))
          (lambda (x) (+ x 1)))
 0)
