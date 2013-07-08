#lang plai/gc2/mutator

(allocator-setup "collector.rkt" 512)

(define (f c)
  (cond
    [c (begin 1 2)]
    [else (begin 3 4)]))

(f 1)

(format "~a" 1)

(procedure? (lambda (x) x))
(procedure-arity-includes? (lambda () 1) 0)

(call-with-input-file "test-1.rkt"
  (lambda (port) 'hello))

'(1 2 3)

(define (loop x)
  (cond
    [(= x 0) 'pass]
    [else (loop (sub1 x))]))
(define (ff number)
  (loop number))
(ff 10)
