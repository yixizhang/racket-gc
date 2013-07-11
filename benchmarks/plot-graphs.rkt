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
  (let ([heap-size-graph (path-with-ext (string-append (path-name p)
                                                       "-"
                                                       "mem")
                                        "pdf")]
        [heap-operations-graph (path-with-ext (string-append (path-name p)
                                                             "-"
                                                             "ops")
                                              "pdf")])
    (printf "plotting data of ~s\n" p)
    (call-with-input-file p
      (λ (port)
        (read port)
        (define g (read port))
        (read port)
        (read port)
        (read port)
        (define g-o (read port))
        (read port)
        (define h-in (read port))
        (define h-out (read port))
        (read port)
        (read port)
        (read port)
        (define h-o (read port))
        
        (with-handlers ([exn:fail?
                         (λ (exn)
                           (pretty-print exn))])
          (plot-file
           (list
            (points
             (for/list ([d (in-list g)]
                        [x (in-naturals)])
               (vector x d))
             #:label "Batch GC"
             #:color "red"
             #:size 2
             #:sym 'triangle)
            (points
             (for/list ([d (in-list h-in)]
                        [x (in-naturals)])
               (vector x d))
             #:label "Incremental GC inside marking"
             #:color "blue"
             #:size 2
             #:sym 'square)
            (points
             (for/list ([d (in-list h-out)]
                        [x (in-naturals)])
               (vector x d))
             #:label "Incremental GC outside marking"
             #:color "yellow"
             #:size 2
             #:sym 'square))
           #:x-label "Object allocations"
           #:y-label "Memory usage"
           heap-size-graph)
          (plot-file
           (list
            (points
             (for/list ([d (in-list g-o)]
                        [x (in-naturals)])
               (vector x d))
             #:label "Batch GC"
             #:color "red"
             #:size 2
             #:sym 'triangle)
            (points
             (for/list ([d (in-list h-o)]
                        [x (in-naturals)])
               (vector x d))
             #:label "Incremental GC"
             #:color "blue"
             #:size 2
             #:sym 'square))
           #:x-label "Heap operations"
           #:y-label "# of running cycles"
           heap-operations-graph))))))