#lang racket
(require plot)
(require "utils.rkt")

;; all .txt files under /basic/ directory
(define all-results
  (for/list ([p (directory-list "basic" #:build? #t)]
             #:when (equal? "txt" (path-ext (path->string p))))
    p))

;; read allocated-spaces records and plot out to .pdf files
;; plot data for two collectors on the same graph
(for ([p (in-list all-results)])
  (let ([plot-path (path-with-ext (path-name p) "pdf")])
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
                          (void)])
                       (plot-file
                         (list
                           (points
                             (for/list ([d (in-list g)]
                                        [x (in-naturals)])
                                       (vector x d))
                             #:size 2)
                           (points
                             (for/list ([d (in-list h)]
                                        [x (in-naturals)])
                                       (vector x h))
                             #:size 2))
                         plot-file))))))
