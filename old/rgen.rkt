#lang racket
(require plai/random-mutator)
(for ([i (in-range 1 100)])
  (let ([out (string-append "tmp-" (number->string i) ".rkt")])
    (save-random-mutator out "coll.rkt" #:gc2? #t)))