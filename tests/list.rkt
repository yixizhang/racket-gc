#lang plai/gc2/mutator
(allocator-setup "collector.rkt" 512)

(provide (all-defined-out))
;; reverse : list -> list
(define (reverse l) (reverse1 empty l))
(define (reverse1 a l)
  (if (null? l)
      a
      (reverse1 (cons (first l) a) (rest l))))
;; append : list list -> list
(define (append x y)
  (if (null? x)
      y
      (cons (first x) (append (rest x) y))))
;; map : proc list -> list
(define (map f l)
  (if (null? l)
      empty
      (cons (f (first l)) (map f (rest l)))))
;; filter : proc list -> list
(define (filter f l) (filter1 f l empty))
(define (filter1 f l result)
  (if (empty? l)
      (reverse result)
      (filter1 f (rest l) (if (f (first l)) (cons (first l) result) result))))
;; foldr : proc any/c list -> any/c
(define (foldr f init l)
  (if (empty? l)
      init
      (f (first l) (foldr f init (rest l)))))
;; memq : any/c list -> list/#f
(define (memq x l)
  (if (null? l)
      #f
      (if (eq? x (first l)) l (memq x (rest l)))))
;; memf : proc list -> list/#f
(define (memf f l)
  (if (null? l)
      #f
      (if (f (first l)) l (memf f (rest l)))))
;; for-each : proc list -> void
(define (for-each f l)
  (if (null? l)
      (void)
      (begin (f (first l))
             (for-each f (rest l)))))
;;; sort : list proc -> list
;(define (sort l less?)
;  (let ([f (lambda (x) (less? x (car l)))])
;    (if (null? l)
;        l
;        (append (sort (filter f (cdr l)) less?)
;                (append (cons (car l) empty)
;                        (sort (filter (lambda (x) (if (f x) #f #t)) (cdr l)) less?))))))
