#lang plai/gc2/mutator
(allocator-setup "../collector.rkt" 1024)

(define (fib n)
  (cond
    [(or (= n 0)
         (= n 1))
     1]
    [else 
     (+ (fib (- n 1))
        (fib (- n 2)))]))

(fib 15)
