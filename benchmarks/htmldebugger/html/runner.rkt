#lang racket
(require plot)
(require "../../collector.rkt")

(let* ([coll-name (vector-ref (current-command-line-arguments) 0)]
       [data-out-path (format "~a.rktd" coll-name)])
  (dynamic-require "parser.rkt" #f)
  (call-with-output-file data-out-path
    (λ (port)
      (fprintf port ";; ~s\n" coll-name)
      (fprintf port "~a\n" (get-output-string (print-metrics))))
    #:mode 'text
    #:exists 'replace))
