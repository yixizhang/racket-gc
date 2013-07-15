#lang plai/gc2/mutator
(allocator-setup "collector.rkt" 512)
(require "util.rkt")

(define hash (make-hash))
(hash-set! hash 0 0)
(hash-ref hash 0 (lambda () (void)))
(hash-set! hash 1 1)
(hash-ref hash 1 (lambda () (void)))
(hash-set! hash 1 10)
(hash-ref hash 1 (lambda () (void)))

(foldr cons '() '(1 2 3 4))
(map (lambda (x) (+ 1 x)) '(1 2 3 4))
(memq 2 '(1 2 3 4))
(memf (λ (x) (> x 9)) '(7 8 9 10 11))
(filter (λ (x) (> x 0)) '(1 -2 3 4 -5))
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
(regexp-replace* "([Mm])i ([a-zA-Z]*)" "mi cerveza Mi Mi Mi"
                 "\\1y \\2")
(regexp-match-positions #rx"x." "12x4x6")

(sort '(1 3 4 2) (lambda (a b) (< a b)))
(sort '("3" "2") (lambda (a b) (string<? a b)))

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
