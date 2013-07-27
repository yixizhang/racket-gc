#lang racket
(require html)

(define (implicit-starts parent child)
  (or (and (eq? child 'tr) (eq? parent 'table) 'tbody)
      (and (eq? child 'td) (memq parent '(table tbody tfoot thead)) 'tr)))

(length
  (call-with-input-file (vector-ref (current-command-line-arguments) 0)
    (lambda (port)
      (read-from-port (gen-may-contain html-spec)
                      implicit-starts
                      port))))
