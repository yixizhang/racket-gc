#lang plai/gc2/collector

;; configuration

(define step-length 10) ;; step-length :: argument for how much work every gc step does
(define heap-threshold "heap-threshold not initialized") ;; heap-threshold :: the portion of heap to detect if gc phase should be triggered

;; main functions

(define (init-allocator)
  (set! heap-threshold (round (* (heap-size) 1/2)))
  (heap-set! 0 'not-in-gc) ;; gc state :: not-in-gc, mark-white!, mark-black!, free-white!
  (heap-set! 1 0) ;; bit for non-free-space
  (heap-set! 2 0) ;; bit for step within gc round
  (heap-set! 3 0) ;; bit for tree traversal continuation locating
  (for ([i (in-range 4 (heap-size))])
    (heap-set! i 'free)))

(define (gc:flat? loc)
  (or (equal? (heap-ref loc) 'flat)
      (equal? (heap-ref loc) 'grey-flat)
      (equal? (heap-ref loc) 'white-flat)))

(define (gc:deref loc)
  (cond
    [(gc:flat? loc)
     (heap-ref (+ loc 1))]
    [else
     (error 'gc:deref
            "non-flat @ ~s"
            loc)]))

(define (gc:cons? loc)
  (or (equal? (heap-ref loc) 'pair)
      (equal? (heap-ref loc) 'white-pair)
      (equal? (heap-ref loc) 'grey-pair)))

(define (gc:first pr-ptr)
  (if (gc:cons? pr-ptr)
      (heap-ref (+ pr-ptr 1))
      (error 'first "non pair @ ~s" pr-ptr)))

(define (gc:rest pr-ptr)
  (if (gc:cons? pr-ptr)
      (heap-ref (+ pr-ptr 2))
      (error 'rest "non pair @ ~s" pr-ptr)))

(define (gc:set-first! pr-ptr new)
  (cond
    [(gc:cons? pr-ptr)
     (heap-set/check! (+ pr-ptr 1) new)
     (write-barrier pr-ptr new)]
    [else (error 'set-first! "non pair @ ~s" pr-ptr)]))

(define (gc:set-rest! pr-ptr new)
  (cond 
    [(gc:cons? pr-ptr)
     (heap-set/check! (+ pr-ptr 2) new)
     (write-barrier pr-ptr new)]
    [else (error 'set-rest! "non pair @ ~s" pr-ptr)]))

(define (write-barrier pr-ptr new) 
  (when (and (equal? (heap-ref pr-ptr) 'pair)
             (or (equal? (heap-ref new) 'white-pair)
                 (equal? (heap-ref new) 'white-flat)
                 (equal? (heap-ref new) 'white-proc)
                 (equal? (heap-ref new) 'white-struct)
                 (equal? (heap-ref new) 'white-struct-instance)))
    (case (heap-ref new)
      [(white-pair) (heap-set/check! new 'grey-pair)
                    (push/cont new)]
      [(white-flat) (heap-set/check! new 'grey-flat)
                    (push/cont new)]
      [(white-proc) (heap-set/check! new 'grey-proc)
                    (push/cont new)]
      [(white-struct) (heap-set/check! new 'grey-struct)
                    (push/cont new)]
      [(white-struct-instance) (heap-set/check! new 'grey-struct-instance)
                    (push/cont new)])))

(define (gc:closure-code-ptr loc)
  (if (gc:closure? loc)
      (heap-ref (+ loc 1))
      (error 'gc:closure-code "non closure")))

(define (gc:closure-env-ref loc i)
  (if (gc:closure? loc)
      (heap-ref (+ loc 3 i))
      (error 'closure-env-ref "non closure")))

(define (gc:closure? loc)
  (or (equal? (heap-ref loc) 'proc)
      (equal? (heap-ref loc) 'white-proc)
      (equal? (heap-ref loc) 'grey-proc)))

(define (gc:alloc-flat fv)
  (define ptr (alloc 2 #f #f))
  (heap-set/check! ptr 'flat)
  (heap-set/check! (+ ptr 1) fv)
  ptr)

(define (gc:cons hd tl)
  (define ptr (alloc 3 hd tl))
  (heap-set/check! ptr 'pair)
  (heap-set/check! (+ ptr 1) hd)
  (heap-set/check! (+ ptr 2) tl)
  ptr)

(define (gc:closure code-ptr free-vars)
  (define fv-count (vector-length free-vars))
  (define next (alloc (+ fv-count 3)
                      (vector->roots free-vars)
                      '()))
  (heap-set/check! next 'proc)
  (heap-set/check! (+ next 1) code-ptr)
  (heap-set/check! (+ next 2) fv-count)
  (for ([x (in-range 0 fv-count)])
    (heap-set/check! (+ next 3 x)
               (vector-ref free-vars x)))
  next)

;; define-struct related
;; gc:alloc-struct : symbol loc number -> loc
;; struct . name . parent/#f . number of fields
(define (gc:alloc-struct name parent fields-count)
  (define next (alloc 4 parent #f))
  (heap-set! next 'struct)
  (heap-set! (+ next 1) name)
  (heap-set! (+ next 2) parent)
  (printf "~s\n" fields-count)
  (heap-set/check! (+ next 3) fields-count)
  next)

;; gc:alloc-struct-instance : loc (vectorof loc) -> loc
;; struct-instance . struct . fields values
(define (gc:alloc-struct-instance s fields-value)
  (define fv-count (vector-length fields-value))
  (define next (alloc (+ fv-count 2)
                      (vector->roots fields-value)
                      '()))
  (heap-set! (+ next 1) s)
  (for ([x (in-range 0 fv-count)])
       (heap-set! (+ next 2 x)
                        (vector-ref fields-value x)))
  (heap-set/check! next 'struct-instance)
  next)

;; gc:struct-pred : loc loc -> true/false
;; suport parents/inheritance
(define (gc:struct-pred s instance)
  (and (equal? (heap-ref s) 'struct)
       (gc:struct-pred-helper s (heap-ref (+ instance 1)))))

;; gc:struct-pred-helper : loc loc/#f -> true/false
(define (gc:struct-pred-helper target s)
  (and s
       (or (= target s)
           (gc:struct-pred-helper target (heap-ref (+ s 2))))))

;; gc:struct-select : loc loc number -> loc
(define (gc:struct-select s instance index)
  (unless (gc:struct-pred s instance)
    (error 'gc:struct-select "value at ~a is not an instance of ~a" 
           instance
           (heap-ref (+ 1 s))))
  (heap-ref (+ instance 2 index)))

(define (alloc n some-roots more-roots)
  (define next (find-free-space 4 n))
  (cond
    [next 
      (heap-set/check! 1 (+ n (heap-ref 1)))
      (unless (<= (heap-ref 1) (heap-size)) (error 'alloc "> heap-size"))
      (case (heap-ref 0)
        [(not-in-gc) (when (>= (heap-ref 1) heap-threshold)
                       (heap-set/check! 0 'mark-white!)
                       (heap-set/check! 2 (+ n (heap-ref 2))) ;; start step count
                       (mark-white! 4)
                       (traverse/roots (get-root-set))
                       (traverse/roots some-roots)
                       (traverse/roots more-roots)
                       (heap-set/check! 0 'mark-black!))]
        [(mark-black!) (heap-set/check! 2 (+ n (heap-ref 2))) ;; step count
                       (traverse/roots-white some-roots)
                       (traverse/roots-white more-roots)
                       (cond 
                         [(>= (heap-ref 2) step-length)
                          ;;(traverse/roots (get-root-set))
                          (traverse/incre-mark (next/cont))]
                         [else (void)])]
        [else (error 'alloc "wrong gc state ~s" (heap-ref 0))])
      (define loc (find-free-space next n))
      (if loc loc (error 'alloc "incremental gc failed"))]
    [else (error 'alloc "incremental gc failed")]))

(define (find-free-space start size)
  (cond
    [(= start (heap-size)) #f]
    [else
     (case (heap-ref start)
       [(free) (if (n-free-blocks? start size)
                   start
                   (find-free-space (+ start 1) size))]
       [(flat grey-flat white-flat) (find-free-space (+ start 2) size)]
       [(pair grey-pair white-pair cont) (find-free-space (+ start 3) size)]
       [(proc grey-proc white-proc)
        (find-free-space
         (+ start 3 (heap-ref (+ start 2)))
         size)]
       [(struct grey-struct white-struct) (find-free-space (+ start 4) size)]
       [(struct-instance grey-struct-instance white-struct-instance)
        (find-free-space
          (+ start 2 (heap-ref (+ 3 (heap-ref (+ start 1)))))
          size)]
       [else
        (error 'find-free-space "wrong tag at ~a" start)])]))

(define (n-free-blocks? start size)
  (cond
    [(= size 0) #t]
    [(= start (heap-size)) #f]
    [else 
     (and (eq? 'free (heap-ref start))
          (n-free-blocks? (+ start 1)
                          (- size 1)))]))

(define (mark-white! i)
  (when (< i (heap-size))
    (case (heap-ref i)
      [(pair) (heap-set! i 'white-pair)
              (mark-white! (+ i 3))]
      [(flat) (heap-set! i 'white-flat)
              (mark-white! (+ i 2))]
      [(proc) (heap-set! i 'white-proc)
              (mark-white!
               (+ i 3 (heap-ref (+ i 2))))]
      [(struct) (heap-set! i 'white-struct)
                (mark-white! (+ i 4))]
      [(struct-instance) (heap-set! i 'white-struct-instance)
                         (mark-white!
                           (+ i 2 (heap-ref (+ 3 (heap-ref (+ i 1))))))]
      [(free) (mark-white! (+ i 1))]
      [(white-pair white-flat white-proc) (void)]
      [else
       (error 'mark-white! "unknown tag @ ~a" i)])))

(define (free-white! i)
  (when (< i (heap-size))
    (case (heap-ref i)
      [(pair) (free-white! (+ i 3))]
      [(flat) (free-white! (+ i 2))]
      [(proc) (free-white! (+ i 3 (heap-ref (+ i 2))))]
      [(white-pair cont) (heap-set! i 'free)
                         (heap-set! (+ i 1) 'free)
                         (heap-set! (+ i 2) 'free)
                         (heap-set! 1 (- (heap-ref 1) 3))
                         (unless (>= (heap-ref 1) 0) (error 'free-white! "< 0"))
                         (free-white! (+ i 3))]
      [(white-flat) (heap-set! i 'free)
                    (heap-set! (+ i 1) 'free)
                    (heap-set! 1 (- (heap-ref 1) 2))
                    (unless (>= (heap-ref 1) 0) (error 'free-white! "< 0"))
                    (free-white! (+ i 2))]
      [(white-proc) (define closure-size (heap-ref (+ i 2)))
                    (for ([dx (in-range 0 (+ closure-size 3))])
                      (heap-set! (+ i dx) 'free))
                    (heap-set! 1 (- (heap-ref 1) (+ 3 closure-size)))
                    (unless (>= (heap-ref 1) 0) (error 'free-white! "< 0"))
                    (free-white! (+ i 3 closure-size))]
      [(white-struct) (heap-set! i 'free)
                      (heap-set! (+ i 1) 'free)
                      (heap-set! (+ i 2) 'free)
                      (heap-set! (+ i 3) 'free)
                      (heap-set! 1 (- (heap-ref 1) 4))
                      (unless (>= (heap-ref 1) 0) (error 'free-white! "< 0"))
                      (free-white! (+ i 4))]
      [(white-struct-instance) (heap-set! i 'free)
                               (heap-set! (+ i 1) 'free)
                               (define fields-size (heap-ref (+ 3 (heap-ref (+ i 1)))))
                               (for ([dx (in-range 0 fields-size)])
                                    (heap-set! (+ i dx) 'free))
                               (heap-set! 1 (- (heap-ref 1) (+ 2 fields-size)))
                               (unless (>= (heap-ref 1) 0) (error 'free-white! "< 0"))
                               (free-white! (+ i 2 fields-size))]
      [(free) (free-white! (+ i 1))]
      [else (error 'free-white! "unknown tag ~s" (heap-ref i))])))

(define (traverse/roots thing)
  (cond
    [(list? thing)
     (for-each traverse/roots thing)]
    [(root? thing)
     (define loc (read-root thing))
     (push/cont loc)]
    [(number? thing)
     (push/cont thing)]))

;; only mark-grey if it's white
(define (traverse/roots-white thing)
  (cond
    [(list? thing)
     (for-each traverse/roots-white thing)]
    [(root? thing)
     (define loc (read-root thing))
     (when (white? loc)
       (push/cont loc))]
    [(number? thing)
     (when (white? thing)
       (push/cont thing))]))

;; incremental traverse/loc
;; no scan, only tree traversal
(define (traverse/incre-mark loc) ;; loc of cont
  (unless (or (equal? 'cont (heap-ref loc))
              (= loc 0))
    (error 'traverse/incre-mark "not cont at ~a" loc))
  (cond
    [(= loc 0) (heap-set/check! 2 0)
               (heap-set/check! 3 0)
               (heap-set/check! 0 'free-white!)
               (free-white! 4)
               (check/tag 4)
               (heap-set/check! 0 'not-in-gc)]
    [else (define ptr (heap-ref (+ loc 1)))
          (case (heap-ref ptr)
            [(flat grey-flat) (mark-black ptr)
                              (step/count 2)
                              (clean/cont loc)
                              (continue/incre-mark)]
            [(pair grey-pair) (mark-black ptr)
                              (step/count 3)
                              (push/cont (heap-ref (+ ptr 2)))
                              (push/cont (heap-ref (+ ptr 1)))
                              (clean/cont loc)
                              (continue/incre-mark)]
            [(proc grey-proc) (mark-black ptr)
                              (define closure-size (heap-ref (+ ptr 2)))
                              (step/count (+ 3 closure-size))
                              (for ([i (in-range closure-size)])
                                   (push/cont (heap-ref (+ ptr 3 i))))
                              (clean/cont loc)
                              (continue/incre-mark)]
            [(struct grey-struct) (mark-black ptr)
                                  (step/count 4)
                                  (define parent (heap-ref (+ ptr 2)))
                                  (when parent (push/cont parent))
                                  (clean/cont loc)
                                  (continue/incre-mark)]
            [(struct-instance grey-struct-instance) (mark-black ptr)
                                                    (define fv-count (heap-ref (+ 3 (heap-ref (+ ptr 1)))))
                                                    (step/count (+ 2 fv-count))
                                                    (push/cont (heap-ref (+ ptr 1)))
                                                    (for ([i (in-range fv-count)])
                                                         (push/cont (heap-ref (+ ptr 2 i))))
                                                    (clean/cont loc)
                                                    (continue/incre-mark)]
            [else (error 'traverse/incre-mark "cont at ~a leads to a wrong tag at ~a" loc ptr)])]))

(define (mark-black loc)
  (case (heap-ref loc)
    [(grey-flat)
     (heap-set/check! loc 'flat)]
    [(grey-pair)
     (mark-grey (heap-ref (+ loc 1)))
     (mark-grey (heap-ref (+ loc 2)))
     (heap-set/check! loc 'pair)]
    [(grey-proc)
     (for ([i (in-range (heap-ref (+ loc 2)))])
          (mark-grey (heap-ref (+ loc 3 i))))
     (heap-set/check! loc 'proc)]
    [(grey-struct)
     (define parent (heap-ref (+ loc 2)))
     (when parent (mark-grey parent))
     (heap-set/check! loc 'struct)]
    [(grey-struct-instance)
     (define fv-count (heap-ref (+ 3 (heap-ref (+ loc 1)))))
     (mark-grey (heap-ref (+ loc 1)))
     (for ([i (in-range fv-count)])
          (mark-grey (heap-ref (+ loc 2 i))))
     (heap-set/check! loc 'struct-instance)]
    [(pair flat proc struct struct-instance white-pair white-flat white-proc white-struct white-struct-instance cont) (void)]))

(define (mark-grey loc)
  (case (heap-ref loc)
    [(white-pair) (heap-set/check! loc 'grey-pair)]
    [(white-flat) (heap-set/check! loc 'grey-flat)]
    [(white-proc) (heap-set/check! loc 'grey-proc)]
    [(white-struct) (heap-set/check! loc 'grey-struct)]
    [(white-struct-instance) (heap-set/check! loc 'grey-struct-instance)]
    [(pair flat proc struct struct-instance grey-pair grey-flat grey-proc grey-struct grey-struct-instance cont) (void)]))

;; helper functions

;; check if any black object points to white objects
(define (heap-set/check! loc thing)
  (heap-set! loc thing)
  (when (= loc 3)
    (unless (or (equal? (heap-ref thing) 'cont)
                (= thing 0))
      (error 'heap-set/check! "should be cont at ~a" thing)))
  (check/cont 4)
  (heap-check 4))

(define (heap-check loc)
  (when (< loc (heap-size))
    (case (heap-ref loc)
      [(pair) 
       (if (or (white? (heap-ref (+ loc 1)))
               (white? (heap-ref (+ loc 2))))
         (error 'heap-set/check! "black object points to white object at ~a" loc)
         (heap-check (+ loc 3)))]
      [(flat) 
       (heap-check (+ loc 2))]
      [(proc)
       (define closure-size (heap-ref (+ loc 2)))
       (case closure-size
         [(free)
          (heap-check (+ loc 3))]
         [else
           (for ([i (in-range closure-size)])
                (when (white? (heap-ref (+ loc 3 i)))
                  (error 'heap-set/check! "black object points to white object at ~a" loc)))
           (heap-check (+ loc 3 closure-size))])]
      [(struct)
       (define parent (heap-ref (+ loc 2)))
       (when (and parent 
                  (white? parent))
         (error 'heap-set/check! "black object points to white object at ~a" loc))
       (heap-check (+ loc 4))]
      [(struct-instance)
       (when (white? (heap-ref (+ loc 1)))
         (error 'heap-set/check! "black object points to white object at ~a" loc))
       (define field-count (heap-ref (+ 3 (heap-ref (+ loc 1)))))
       (for ([i (in-range field-count)])
         (when (white? (heap-ref (+ loc 2 i)))
           (error 'heap-set/check! "black object points to white object at ~a" loc)))
       (heap-check (+ loc 2 field-count))]
      [(white-pair grey-pair)
       (heap-check (+ loc 3))]
      [(white-flat grey-flat)
       (heap-check (+ loc 2))]
      [(white-proc grey-proc)
       (heap-check (+ loc 3 (heap-ref (+ loc 2))))]
      [(white-struct grey-struct)
       (heap-check (+ loc 4))]
      [(white-struct-instance grey-struct-instance)
       (heap-check (+ loc 2 (heap-ref (+ 3 (heap-ref (+ 1 loc))))))]
      [(free) (heap-check (+ loc 1))]
      [(cont) (heap-check (+ loc 3))]
      [else (error 'check "unknown tag @ ~a" loc)])))

(define (check/tag loc)
  (when (< loc (heap-size))
    (case (heap-ref loc)
      [(pair) (check/tag (+ loc 3))]
      [(flat) (check/tag (+ loc 2))]
      [(proc) (check/tag (+ loc 3 (heap-ref (+ loc 2))))]
      [(struct) (check/tag (+ loc 4))]
      [(struct-instance) (check/tag (+ loc 2 (heap-ref (+ 3 (heap-ref (+ loc 1))))))]
      [(free) (check/tag (+ loc 1))]
      [else (error 'check/tag "wrong tag at ~a" loc)])))

(define (check/cont loc)
  (when (< loc (heap-size))
    (case (heap-ref loc)
      [(pair white-pair grey-pair) (check/cont (+ loc 3))]
      [(flat white-flat grey-flat) (check/cont (+ loc 2))]
      [(proc white-proc grey-proc) 
       (define closure-size (heap-ref (+ loc 2)))
       (case closure-size
         [(free) (check/cont (+ loc 3))]
         [else (check/cont (+ loc 3 closure-size))])]
      [(struct white-struct grey-struct) (check/cont (+ loc 4))]
      [(struct-instance white-struct-instance grey-struct-instance)
       (define fv-count (heap-ref (+ 3 (heap-ref (+ loc 1)))))
       (case fv-count
         [(free) (check/cont (+ loc 2))]
         [else (check/cont (+ loc fv-count))])]
      [(free) (check/cont (+ loc 1))]
      [(cont) (check-helper/cont (heap-ref (+ loc 2)))]
      [else (error 'check/cont "wrong tag at ~a" loc)])))

(define (check-helper/cont loc)
  (cond
    [(equal? 'cont (heap-ref loc))
     (check-helper/cont (heap-ref (+ loc 2)))]
    [(= loc 0) (void)]
    [else
      (error 'check/cont "cont links to wrong tag at ~a" loc)]))

;; white? : location? -> boolean?
(define (white? loc)
  (case (heap-ref loc)
    [(white-pair white-proc white-flat white-struct white-struct-instance)
     true]
    [else false]))

(define (next/cont)
  (define loc (heap-ref 3)) 
  (cond
    [(= loc 0) 0]
    [else
      (heap-set! 3 (heap-ref (+ loc 2)))
      ;;(heap-set! loc 'free)
      ;;(heap-set! (+ loc 1) 'free)
      ;;(heap-set! (+ loc 2) 'free)
      loc]))

(define (push/cont ptr)
  (mark-grey ptr)
  (case (heap-ref ptr)
    [(pair flat proc) (void)]
    [else
      (define loc (find-free-space 4 3))
      (define head (heap-ref 3))
      (heap-set! loc 'cont)
      (heap-set! (+ loc 1) ptr)
      (heap-set! (+ loc 2) head)
      (heap-set! 3 loc)]))

(define (continue/incre-mark)
  (if (step/finished?)
    (heap-set/check! 2 0)
    (traverse/incre-mark (next/cont))))

(define (clean/cont loc)
  (heap-set! loc 'free)
  (heap-set! (+ loc 1) 'free)
  (heap-set! (+ loc 2) 'free))

(define (step/count n)
  (heap-set/check! 2 (- (heap-ref 2) n)))

(define (step/finished?)
  (<= (heap-ref 2) 0))

;; test for functions
(print-only-errors #t)
(define v1 (make-vector 24 'f))

(test (with-heap v1
                 (init-allocator)
                 v1)
      (vector 'not-in-gc 0 0 0
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free))

(test (with-heap v1
                 (init-allocator)
                 (gc:alloc-flat 1)
                 (mark-white! 4)
                 (push/cont 4)
                 v1)
      (vector 'not-in-gc 2 0 6
              'grey-flat 1 'cont 4
              0 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free))

(test (with-heap v1
                 (init-allocator)
                 (gc:alloc-flat 1)
                 (mark-white! 4)
                 (push/cont 4)
                 (gc:cons 4 4)
                 v1)
      (vector 'not-in-gc 5 0 6
              'grey-flat 1 'cont 4
              0 'pair 4 4
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free))

(test (with-heap v1
                 (init-allocator)
                 (gc:alloc-flat 1)
                 (mark-white! 4)
                 (push/cont 4)
                 (find-free-space 4 3))
      9)
