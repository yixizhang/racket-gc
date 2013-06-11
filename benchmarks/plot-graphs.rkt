#lang racket
(require plot)
(require racket/pretty)
(require "utils.rkt")

;; all .txt files under /basic/ directory
(define all-results
  (for/list ([p (directory-list "basic" #:build? #t)]
             #:when (equal? "rktd" (path-ext (path->string p))))
    (path->string p)))

;; read allocated-spaces records and plot out to .pdf files
;; plot data for two collectors on the same graph
(for ([p (in-list all-results)])
  (let ([plot-path (path-with-ext (path-name p) "pdf")])
    (printf "plotting data of ~s\n" p)
    (call-with-input-file p
      (λ (port)
        (read port)
        (define g (read port))
        (read port)
        (read port)
        (read port)
        (read port)
        (define h (read port))
        (with-handlers ([exn:fail?
                         (λ (exn)
                           (pretty-print exn))])
          (plot-file
           (list
            (points
             (for/list ([d (in-list g)]
                        [x (in-naturals)])
               (vector x d))
             #:label "mark-sweep for big generation"
             #:color "red"
             #:size 2
             #:sym 'triangle)
            (points
             (for/list ([d (in-list h)]
                        [x (in-naturals)])
               (vector x d))
             #:label "incremental mark-sweep for big generation"
             #:color "blue"
             #:size 2
             #:sym 'square))
           #:x-label "object allocation"
           #:y-label "heap size"
           plot-path))))))
