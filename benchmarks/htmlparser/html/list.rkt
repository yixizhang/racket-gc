#lang plai/gc2/mutator
(allocator-setup "../../collector.rkt" 10240)
(import-primitives
 reverse append)

(provide (all-defined-out))
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
