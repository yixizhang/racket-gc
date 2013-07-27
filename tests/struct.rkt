#lang plai/gc2/mutator
(allocator-setup "collector.rkt" 512)

(define-struct s (x))
(define-struct (t s) (w z))
(define si (make-s 'x))
(define ti (make-t 's 'w 'z))
(s-x si)
(t-w ti)

(define (f x)
  (cond
    [(= x 0) empty]
    [else (let ([ss (make-s x)])
            (cond
              [(s? ss)
               (begin
                 (set-s-x! ss 0)
                 (cons (s-x ss)
                       (f (- x 1))))]
              [else
               (printf "ss: ~s is not a s" ss)]))]))
(f 20)
