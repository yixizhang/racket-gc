#lang racket
(require plot)

;; color set
(define colors '("blue" "red" "yellow" "green"))
(define-syntax-rule (choose/color index)
  (let ([c (list-ref colors index)])
    (set! index (add1 index))
    c))                         

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
(define (ppoints index1 index2)
  (lambda (file)
    (unless (string? file)
      (error 'read-data "expected file name in string fmt"))
    (local [(define (choose/color1) (choose/color index1))
            (define (choose/color2) (choose/color index2))
            (define (gen-points lst label color)
              (points (for/list ([d (in-list lst)] 
                                 [x (in-naturals)])
                        (vector x d))
                      #:label label
                      #:color color
                      #:size 2))]
      (define name (string-titlecase
                    (last 
                     (string-split 
                      (first (string-split file "-"))
                      "/"))))
      (cond
        [(regexp-match "incremental" file)
         (let-values ([(mem-in mem-out cycle-list) (read-data file)])
           (list (map (lambda (lst side)
                        (gen-points lst
                                    (format "~a GC ~a marking" name side)
                                    (choose/color1)))
                      (list mem-in mem-out) '("inside" "outside"))
                 (list (gen-points cycle-list
                                   (format "~a GC" name)
                                   (choose/color2)))))]
        [else
         (let-values ([(mem-list cycle-list) (read-data file)])
           (list (list (gen-points mem-list "Batch GC" (choose/color1)))
                 (list (gen-points cycle-list "Batch GC" (choose/color2)))))]))))
(define produce/points (ppoints 0 0))

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