#lang plai/gc2/mutator

(allocator-setup "../../hybrid.rkt" 200)

(define (f c)
  (cond
    [c (begin 1 2)]
    [else (begin 3 4)]))

(f 1)

(format "~a" 1)

(procedure? (lambda (x) x))

'(1 2 3)

#|
(define (ff number)
  (let ([loop 47])
    (begin
      (set! loop (lambda (x)
                   (cond
                     [(= x 0) 'pass]
                     [else (loop (sub1 x))])))
      (loop number))))
(ff 10)
|#
(define (loop x)
  (cond
    [(= x 0) 'pass]
    [else (loop (sub1 x))]))
(define (ff number)
  (loop number))
(ff 10)