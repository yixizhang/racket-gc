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
     (heap-set! (+ pr-ptr 1) new)
     (write-barrier pr-ptr new)
     (heap-cont/check)]
    [else (error 'set-first! "non pair @ ~s" pr-ptr)]))

(define (gc:set-rest! pr-ptr new)
  (cond 
    [(gc:cons? pr-ptr)
     (heap-set! (+ pr-ptr 2) new)
     (write-barrier pr-ptr new)
     (heap-cont/check)]
    [else (error 'set-rest! "non pair @ ~s" pr-ptr)]))

(define (write-barrier ptr new) 
  (when (and (or (equal? (heap-ref ptr) 'pair)
                 (equal? (heap-ref ptr) 'vector))
             (or (equal? (heap-ref new) 'white-pair)
                 (equal? (heap-ref new) 'white-flat)
                 (equal? (heap-ref new) 'white-proc)
                 (equal? (heap-ref new) 'white-vector)
                 (equal? (heap-ref new) 'white-struct)
                 (equal? (heap-ref new) 'white-struct-instance)))
    (case (heap-ref new)
      [(white-pair) (heap-set! new 'grey-pair)
                    (push/cont new)]
      [(white-flat) (heap-set! new 'grey-flat)
                    (push/cont new)]
      [(white-proc) (heap-set! new 'grey-proc)
                    (push/cont new)]
      [(white-vector) (heap-set! new 'grey-vector)
                      (push/cont new)]
      [(white-struct) (heap-set! new 'grey-struct)
                    (push/cont new)]
      [(white-struct-instance) (heap-set! new 'grey-struct-instance)
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
  (heap-set! ptr 'white-flat)
  (heap-set! (+ ptr 1) fv)
  (heap-cont/check)
  ptr)

(define (gc:cons hd tl)
  (define ptr (alloc 3 hd tl))
  (heap-set! ptr 'white-pair)
  (heap-set! (+ ptr 1) hd)
  (heap-set! (+ ptr 2) tl)
  (heap-cont/check)
  ptr)

(define (gc:closure code-ptr free-vars)
  (define fv-count (vector-length free-vars))
  (define next (alloc (+ fv-count 3)
                      (vector->roots free-vars)
                      '()))
  (heap-set! next 'white-proc)
  (heap-set! (+ next 1) code-ptr)
  (heap-set! (+ next 2) fv-count)
  (for ([x (in-range 0 fv-count)])
    (heap-set! (+ next 3 x)
               (vector-ref free-vars x)))
  (heap-cont/check)
  next)

;; vector related
;; gc:vector : number loc -> loc
(define (gc:vector length loc)
  (define next (alloc (+ length 2) #f #f))
  (heap-set! next 'white-vector)
  (heap-set! (+ next 1) length)
  (for ([i (in-range length)])
       (heap-set! (+ next 2 i) loc))
  (heap-cont/check)
  next)

(define (gc:vector? loc)
  (or (equal? (heap-ref loc) 'vector)
      (equal? (heap-ref loc) 'white-vector)
      (equal? (heap-ref loc) 'grey-vector)))

(define (gc:vector-length loc)
  (if (gc:vector? loc)
    (heap-ref (+ loc 1))
    (error 'gc:vector-length "non vector @ ~s" loc)))

;; gc:vector-ref loc number -> loc
(define (gc:vector-ref loc number)
  (unless (gc:vector? loc)
    (error 'gc:vector-ref "non vector @ ~s" loc))
  (cond
    [(< number (gc:vector-length loc)) (heap-ref (+ loc 2 number))]
    [else (error 'gc:vector-ref 
                 "vector @ ~s index ~s out of range"
                 loc number)]))

;; gc:vector-set! : loc number loc -> void
(define (gc:vector-set! loc number thing)
  (unless (gc:vector? loc)
    (error 'gc:vector-set! "non vector @ ~s" loc))
  (cond 
    [(< number (gc:vector-length loc))
     (heap-set! (+ loc 2 number) thing)
     (write-barrier loc thing)
     (heap-cont/check)]
    [else
      (error 'gc:vector-set! 
             "vector @ ~s index ~s out of range"
             loc number)]))

;; define-struct related
;; gc:alloc-struct : symbol loc number -> loc
;; struct . name . parent/#f . number of fields
(define (gc:alloc-struct name parent fields-count)
  (define next (alloc 4 parent #f))
  (heap-set! next 'white-struct)
  (heap-set! (+ next 1) name)
  (heap-set! (+ next 2) parent)
  (heap-set! (+ next 3) fields-count)
  (heap-cont/check)
  next)

(define (struct? thing)
  (or (equal? (heap-ref thing) 'struct)
      (equal? (heap-ref thing) 'white-struct)
      (equal? (heap-ref thing) 'grey-struct)))

;; gc:alloc-struct-instance : loc (vectorof loc) -> loc
;; struct-instance . struct . fields values
(define (gc:alloc-struct-instance s fields-value)
  (define fv-count (vector-length fields-value))
  (define next (alloc (+ fv-count 2)
                      (vector->roots fields-value)
                      '()))
  (heap-set! next 'white-struct-instance)
  (heap-set! (+ next 1) s)
  (for ([x (in-range 0 fv-count)])
       (heap-set! (+ next 2 x)
                        (vector-ref fields-value x)))
  (heap-cont/check)
  next)

;; gc:struct-pred : loc loc -> true/false
(define (gc:struct-pred s instance)
  (and (struct? s)
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
      (heap-set! 1 (+ n (heap-ref 1)))
      (unless (<= (heap-ref 1) (heap-size)) (error 'alloc "> heap-size"))
      (case (heap-ref 0)
        [(not-in-gc) (when (>= (heap-ref 1) heap-threshold)
                       (heap-set! 2 (+ n (heap-ref 2))) ;; start step count
                       (traverse/roots (get-root-set))
                       (traverse/roots some-roots)
                       (traverse/roots more-roots)
                       (heap-set! 0 'mark-black!))]
        [(mark-black!) (heap-set! 2 (+ n (heap-ref 2))) ;; step count
                       (traverse/roots-white (get-root-set))
                       (traverse/roots-white some-roots)
                       (traverse/roots-white more-roots)
                       (cond 
                         [(>= (heap-ref 2) step-length)
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
       [(vector grey-vector white-vector)
        (find-free-space
          (+ start 2 (heap-ref (+ start 2)))
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
      [(vector) (heap-set! i 'white-vector)
                (mark-white!
                  (+ i 2 (heap-ref (+ i 2))))]
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
                         (free-white! (+ i 3))]
      [(white-flat) (heap-set! i 'free)
                    (heap-set! (+ i 1) 'free)
                    (heap-set! 1 (- (heap-ref 1) 2))
                    (free-white! (+ i 2))]
      [(white-proc) (define closure-size (heap-ref (+ i 2)))
                    (for ([dx (in-range 0 (+ closure-size 3))])
                      (heap-set! (+ i dx) 'free))
                    (heap-set! 1 (- (heap-ref 1) (+ 3 closure-size)))
                    (free-white! (+ i 3 closure-size))]
      [(white-vector) (define size (heap-ref (+ i 1)))
                      (for ([dx (in-range (+ size 2))])
                           (heap-set! (+ i dx) 'free))
                      (heap-set! 1 (- (heap-ref 1) (+ 2 size)))
                      (free-white! (+ i 2 size))]
      [(white-struct) (heap-set! i 'free)
                      (heap-set! (+ i 1) 'free)
                      (heap-set! (+ i 2) 'free)
                      (heap-set! (+ i 3) 'free)
                      (heap-set! 1 (- (heap-ref 1) 4))
                      (free-white! (+ i 4))]
      [(white-struct-instance) (heap-set! i 'free)
                               (heap-set! (+ i 1) 'free)
                               (define fields-size (heap-ref (+ 3 (heap-ref (+ i 1)))))
                               (for ([dx (in-range 0 fields-size)])
                                    (heap-set! (+ i dx) 'free))
                               (heap-set! 1 (- (heap-ref 1) (+ 2 fields-size)))
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
    [(= loc 0) (heap-set! 2 0)
               (heap-set! 3 0) ;; no need for check/cont-alloc
               (heap-set! 0 'free-white!)
               (free-white! 4)
               (check/tag 4)
               (heap-set! 0 'not-in-gc)
               (heap-cont/check)]
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
            [(vector grey-vector) (mark-black ptr)
                                  (define size (heap-ref (+ ptr 1)))
                                  (step/count (+ 2 size))
                                  (for ([i (in-range size)])
                                       (push/cont (heap-ref (+ ptr 2 i))))
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
     (heap-set! loc 'flat)
     (heap-cont/check)]
    [(grey-pair)
     (mark-grey (heap-ref (+ loc 1)))
     (mark-grey (heap-ref (+ loc 2)))
     (heap-set! loc 'pair)
     (heap-cont/check)]
    [(grey-proc)
     (for ([i (in-range (heap-ref (+ loc 2)))])
          (mark-grey (heap-ref (+ loc 3 i))))
     (heap-set! loc 'proc)
     (heap-cont/check)]
    [(grey-vector)
     (for ([i (in-range (heap-ref (+ loc 1)))])
          (mark-grey (heap-ref (+ loc 2 i))))
     (heap-set! loc 'vector)
     (heap-cont/check)]
    [(grey-struct)
     (define parent (heap-ref (+ loc 2)))
     (when parent (mark-grey parent))
     (heap-set! loc 'struct)
     (heap-cont/check)]
    [(grey-struct-instance)
     (define fv-count (heap-ref (+ 3 (heap-ref (+ loc 1)))))
     (mark-grey (heap-ref (+ loc 1)))
     (for ([i (in-range fv-count)])
          (mark-grey (heap-ref (+ loc 2 i))))
     (heap-set! loc 'struct-instance)
     (heap-cont/check)]
    [(pair flat proc struct struct-instance white-pair white-flat white-proc white-struct white-struct-instance cont) (void)]))

(define (mark-grey loc)
  (case (heap-ref loc)
    [(white-pair) (heap-set! loc 'grey-pair)
                  (heap-cont/check)]
    [(white-flat) (heap-set! loc 'grey-flat)
                  (heap-cont/check)]
    [(white-proc) (heap-set! loc 'grey-proc)
                  (heap-cont/check)]
    [(white-vector) (heap-set! loc 'grey-vector)
                    (heap-cont/check)]
    [(white-struct) (heap-set! loc 'grey-struct)
                    (heap-cont/check)]
    [(white-struct-instance) (heap-set! loc 'grey-struct-instance)
                             (heap-cont/check)]
    [(pair flat proc struct struct-instance grey-pair grey-flat grey-proc grey-struct grey-struct-instance cont) (void)]))

;; helper functions

;; heap-check & check/cont
;; ToDo: why scan heap twice?
(define (heap-cont/check)
  (check/cont 4)
  (heap-check 4))

;; check what allocated to cont
(define (check/cont-alloc loc thing)
  (cond
    [(= loc 3)
     (unless (or (equal? (heap-ref thing) 'cont)
                 (= thing 0))
       (error 'check/cont-alloc "not a cont at location ~a" thing))]
    [else (error 'check/cont-alloc "meant to check location 3")]))

(define (heap-check loc)
  (when (< loc (heap-size))
    (case (heap-ref loc)
      [(pair) 
       (if (or (white? (heap-ref (+ loc 1)))
               (white? (heap-ref (+ loc 2))))
         (error 'heap-check "black object points to white object at ~a" loc)
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
                  (error 'heap-check "black object points to white object at ~a" loc)))
           (heap-check (+ loc 3 closure-size))])]
      [(vector)
       (define size (heap-ref (+ loc 1)))
       (case size
         [(free)
          (heap-check (+ loc 2))]
         [else
           (for ([i (in-range size)])
                (when (white? (heap-ref (+ loc 2 i)))
                  (error 'heap-check "black object points to white object at ~a" loc)))
           (heap-check (+ loc 2 size))])]
      [(struct)
       (define parent (heap-ref (+ loc 2)))
       (when (and parent 
                  (white? parent))
         (error 'heap-check "black object points to white object at ~a" loc))
       (heap-check (+ loc 4))]
      [(struct-instance)
       (when (white? (heap-ref (+ loc 1)))
         (error 'heap-check "black object points to white object at ~a" loc))
       (define field-count (heap-ref (+ 3 (heap-ref (+ loc 1)))))
       (for ([i (in-range field-count)])
         (when (white? (heap-ref (+ loc 2 i)))
           (error 'heap-check "black object points to white object at ~a" loc)))
       (heap-check (+ loc 2 field-count))]
      [(white-pair grey-pair)
       (heap-check (+ loc 3))]
      [(white-flat grey-flat)
       (heap-check (+ loc 2))]
      [(white-proc grey-proc)
       (heap-check (+ loc 3 (heap-ref (+ loc 2))))]
      [(white-vector grey-vector)
       (heap-check (+ loc 2 (heap-ref (+ loc 1))))]
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
      [(vector) (check/tag (+ loc 2 (heap-ref (+ loc 1))))]
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
       (check/cont (+ loc 3 closure-size))]
      [(vector white-vector grey-vector)
       (define size (heap-ref (+ loc 1)))
       (check/cont (+ loc 2 size))]
      [(struct white-struct grey-struct) (check/cont (+ loc 4))]
      [(struct-instance white-struct-instance grey-struct-instance)
       (define fields-num (heap-ref (+ 3 (heap-ref (+ loc 1)))))
       (check/cont (+ loc 2 fields-num))]
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
    [(white-pair white-proc white-flat white-vector white-struct white-struct-instance)
     true]
    [else false]))

(define (next/cont)
  (define loc (heap-ref 3)) 
  (cond
    [(= loc 0) 0]
    [else
      (check/cont-alloc 3 (heap-ref (+ loc 2)))
      (heap-set! 3 (heap-ref (+ loc 2)))
      (heap-cont/check)
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
      (check/cont-alloc 3 loc)
      (heap-set! 3 loc)
      (heap-cont/check)]))

(define (continue/incre-mark)
  (if (step/finished?)
    (heap-set! 2 0)
    (traverse/incre-mark (next/cont))))

(define (clean/cont loc)
  (heap-set! loc 'free)
  (heap-set! (+ loc 1) 'free)
  (heap-set! (+ loc 2) 'free)
  (heap-cont/check))

(define (step/count n)
  (heap-set! 2 (- (heap-ref 2) n))
  (heap-cont/check))

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
                 (push/cont 4)
                 (gc:cons 4 4)
                 v1)
      (vector 'not-in-gc 5 0 6
              'grey-flat 1 'cont 4
              0 'white-pair 4 4
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free))

(test (with-heap v1
                 (init-allocator)
                 (gc:alloc-flat 1)
                 (push/cont 4)
                 (find-free-space 4 3))
      9)