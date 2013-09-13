#lang plai/gc2/mutator
(allocator-setup "collector.rkt" 512)

(define null? empty?)
(define not
  (lambda (x) (if x #f #t)))
(define (append x y)
  (if (null? x)
      y
      (cons (first x) (append (rest x) y))))
(define (reverse l) (reverse1 empty l))
(define (reverse1 a l)
  (if (null? l)
      a
      (reverse1 (cons (first l) a) (rest l))))
(define (filter f l) (filter1 f l empty))
(define (filter1 f l result)
  (if (empty? l)
      (reverse result)
      (filter1 f (rest l) (if (f (first l)) (cons (first l) result) result))))
(define (sort l less?)
  (if (null? l)
      l
      (append (sort (filter (lambda (x) (less? x (first l))) (rest l)) less?)
              (append (cons (first l) empty)
                      (sort (filter (lambda (x) (not (less? x (first l)))) (rest l)) less?)))))
(sort '(3 1 4 2) (lambda (a b) (< a b)))
