#lang racket
(provide path-name
         path-ext
         path-with-ext)

(define (path-name path)
  (first (string-split path ".")))
(define (path-ext path)
  (second (string-split path ".")))
(define (path-with-ext name ext)
  (string->path (string-append name "." ext)))