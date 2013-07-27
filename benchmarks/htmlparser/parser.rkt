#lang racket
(require html)

(pretty-display
  (call-with-input-file (vector-ref (current-command-line-arguments) 0)
    (lambda (port)
      (read-html-as-xml port))))
