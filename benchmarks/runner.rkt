#lang racket
(require plot)
(require racket/runtime-path)
(require "collector.rkt")

(define (path-name path)
  (first (string-split path ".")))

(define (path-with-ext name ext)
  (string->path (string-append name ext)))

(define (write-to-path value path)
  (display value
           (open-output-file path #:exists 'append)))

(define coll-name (vector-ref (current-command-line-arguments) 0))
(define test (vector-ref (current-command-line-arguments) 1))
(define out-path (path-with-ext (path-name test) ".txt"))
(dynamic-require test #f)
(let-values ([(out all-heap-size) (print-metrics)])
  (write-to-path coll-name out-path)
  (write-to-path (get-output-string out) out-path)
  (write-to-path "\n" out-path))
