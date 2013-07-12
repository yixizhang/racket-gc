#lang racket
(require plot)

;; read-data : string? -> (valuesof (or/c number? (listof number?)))
(define (read-data file)
  (unless (string? file)
    (error 'read-data "expected file name in string fmt"))
  (cond
    [(regexp-match "incremental" file)
     (call-with-input-file file
       (lambda (port)
         (define largest-mem (read port))
         (define mem-in (read port))
         (define mem-out (read port))
         (define largest-cycles (read port))
         (define average-cycles (read port))
         (define total-cycles (read port))
         (define cycle-list (read port))
         (values mem-in mem-out cycle-list)))]
    [else
     (call-with-input-file file
       (lambda (port)
         (define largest-mem (read port))
         (define mem-list (read port))
         (define largest-cycles (read port))
         (define average-cycles (read port))
         (define total-cycles (read port))
         (define cycle-list (read port))
         (values mem-list cycle-list)))]))

;; produce/points : string? -> (valuesof (listof points) points)
;; produce points for a single file
(define (produce/points file)
  (unless (string? file)
    (error 'read-data "expected file name in string fmt"))
  (local [(define-syntax-rule (gen-points lst label color)
            (points (for/list ([d (in-list lst)] 
                               [x (in-naturals)])
                      (vector x d))
                    #:label label
                    #:color color
                    #:size 2))]
    (cond
      [(regexp-match "incremental" file)
       (let-values ([(mem-in mem-out cycle-list) (read-data file)])
         (list (list (gen-points mem-in "Incremental GC inside marking" "blue")
                     (gen-points mem-out "Incremental GC outside marking" "yellow"))
               (list (gen-points cycle-list "Incremental GC" "blue"))))]
      [else
       (let-values ([(mem-list cycle-list) (read-data file)])
         (list (list (gen-points mem-list "Batch GC" "red"))
               (list (gen-points cycle-list "Batch GC" "red"))))])))

;; produce/graphs : (listof string?) string? -> void?
;; output graph to corresponding file
(define (produce/graphs fs name)
  (for ([f (in-list fs)])
    (unless (string? f)
      (error 'read-data "expected file name in string fmt")))
  (define lps (map produce/points fs))
  (define memps (apply append (map first lps)))
  (define opsps (apply append (map second lps)))
  
  (plot-file memps
             (format "~a-mem.pdf" name)
             #:x-label "allocations" #:y-label "mem")
  (plot-file opsps
             (format "~a-ops.pdf" name)
             #:x-label "heap ops" #:y-label "cycles"))

;; main : current-command-line-arguments -> void
;; produce graph for single or multiple data files
(with-handlers ([exn:fail?
                 (lambda (e)
                   (print e))])
  (define argv (vector->list (current-command-line-arguments)))
  (define argc (length argv))
  (unless (not (= 0 argc))
    (error 'main "expected files(s) to plot"))
  
  (cond
    [(= argc 1) (let ([name (first (string-split (first argv) "."))])
                  (produce/graphs argv name))]
    [(= argc 2) (let* ([bases (map (lambda (x)
                                     (first (string-split x ".")))
                                   argv)]
                       [ns (map (lambda (x)
                                  (first (string-split x "-")))
                                bases)]
                       [ratio (second (string-split (first bases) "-"))])
                  (produce/graphs argv (format "~a-~a-~a"
                                               (first ns)
                                               (last (string-split (second ns) "/"))
                                               ratio)))]
    [else (error 'main "expected one or two file(s)")]))