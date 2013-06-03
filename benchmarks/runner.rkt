#lang racket
(require plot)
(require racket/runtime-path)
(require "collector.rkt")

(define (path-name path)
  (first (string-split path ".")))

(define (path-with-ext name ext)
  (string->path (string-append name ext)))

(define (plot-data-out-to-file data file)
  (plot
   (points
    (for/list ([d (in-list data)]
               [x (in-naturals)])
      (vector x d)))))

(define coll-name (vector-ref (current-command-line-arguments) 0))
(define test (vector-ref (current-command-line-arguments) 1))
(define data-out-path (path-with-ext (path-name test) ".txt"))
(define plot-out-path (path-with-ext (path-name test) ".pdf"))
(with-handlers ([exn:fail?
                 (lambda (exn)
                   (call-with-output-file data-out-path
                     (λ (port)
                       (parameterize ((current-error-port port))
                         ((error-display-handler) (exn-message exn) exn)))))])
  (dynamic-require test #f)
  (let-values ([(out all-heap-size) (print-metrics)])
    (plot-data-out-to-file all-heap-size plot-out-path)
    (call-with-output-file data-out-path
      (λ (port)
        (fprintf port "~s\n" coll-name)
        (fprintf port "~a\n" (get-output-string out))))))
