#lang racket
(require plot)
(require racket/runtime-path)
(require "../collector.rkt")

(let* ([coll-name (vector-ref (current-command-line-arguments) 0)]
       [bench-name (vector-ref (current-command-line-arguments) 1)]
       [bench (format "~a.rkt" bench-name)]
       [ratio (vector-ref (current-command-line-arguments) 2)]
       [data-out-path (format "~a/~a-~a.rktd" 
                              bench-name
                              coll-name
                              ratio)])
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (call-with-output-file data-out-path
                       (λ (port)
                         (parameterize ((current-error-port port))
                           ((error-display-handler) (exn-message exn) exn)))
                       #:mode 'text
                       #:exists 'replace))])
    (dynamic-require bench #f)
    (call-with-output-file data-out-path
      (λ (port)
        (fprintf port ";; ~s\n" coll-name)
        (fprintf port "~a\n" (get-output-string (print-metrics))))
      #:mode 'text
      #:exists 'replace)))
