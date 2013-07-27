#lang plai/gc2/mutator
(allocator-setup "../../collector.rkt" 30)
(import-primitives
 identity
 regexp-match
 read-char open-input-string)

'(1 2)
(identity '(1 2))
'(1 2)

(regexp-match "x" "x1x4")

(define-struct s (x y))
(make-s 1 2)
(define (append a b)
  (cond [(empty? a) b]
        [else (cons (car a) (append (cdr a) b))]))
(append '(0)
        (cons (make-s 0 0) empty))

(let ([c (read-char 
          (open-input-string "c"))])
  (eq? c #\-))

(let-values ([(x y) (values 1 2)])
  (cons x y))
