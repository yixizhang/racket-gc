#lang plai/gc2/mutator
(allocator-setup "../../collector.rkt" 40)

;; debug (let ([id expr] ...) body)

(define f
  (lambda (x) (cons x empty)))
(define g
  (lambda (x y) (cons x y)))
(define (ff x w z)
  (cons (first (f x)) 
        (cons (first (g w z)) 
              (cons (rest (g w z)) empty))))
(let ([x 1] [w 0] [z 2])
  (ff x w z))