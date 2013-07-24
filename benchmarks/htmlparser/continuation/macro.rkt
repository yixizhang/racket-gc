#lang racket

(define *cont* (lambda (x) (printf "value is: ~a\n" x)))

(define-syntax-rule (=bind ([(id ...) expr]) body ...)
  (let ([*cont* (lambda (id ...) body ...)]) expr))

(define-syntax-rule (=values id ...)
  (*cont* id ...))

;; expected: "9 + 1 = 10", but get value is: 1
(=bind ([(y) (*cont* 1)]) (format "9 + 1 = ~a" y))

(let ([*cont* (lambda (y) (format "9 + 1 = ~a" y))])
  (*cont* 1))