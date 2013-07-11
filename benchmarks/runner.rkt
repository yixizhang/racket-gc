#lang racket
(require plot)
(require racket/runtime-path)
(require "collector.rkt")
(require "utils.rkt")

(define coll-name (vector-ref (current-command-line-arguments) 0))
(define test (vector-ref (current-command-line-arguments) 1))
(define ratio (vector-ref (current-command-line-arguments) 2))
(define data-out-path (path-with-ext (string-append (path-name test)
                                                    "-"
                                                    ratio)
                                     "rktd"))

(with-handlers ([exn:fail?
                 (lambda (exn)
                   (call-with-output-file data-out-path
                     (λ (port)
                       (parameterize ((current-error-port port))
                         ((error-display-handler) (exn-message exn) exn)))
                     #:mode 'text
                     #:exists 'append))])
  (dynamic-require test #f)
  (call-with-output-file data-out-path
    (λ (port)
      (fprintf port ";; ~s\n" coll-name)
      (fprintf port "~a\n" (get-output-string (print-metrics))))
    #:mode 'text
    #:exists 'append))
