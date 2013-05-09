#lang plai/gc2/collector

(define step-length 10)
(define 1st-gen-size "size of young generation")
(define 2nd-gen-size "size of old generation")
(define 2nd-gen-start "start position to allocate objects in old generation")
(define status-word "current phase of old generation incremental gc")
(define volume-word "current total size of objects in old generation")
(define step-count-word "work count for each tracing round")
(define tracing-head-word "head of tracing tree")
(define table-start-word "start position of table for cross-generation pointers")

(define (init-allocator)
  (heap-set! 0 2) ;; allocation pointer
  (heap-set! 1 'left) ;; active-semi-space : left -> left side, right -> right side
  (for ([i (in-range 2 (heap-size))])
    (heap-set! i 'free))
  (set! 1st-gen-size (round (* (heap-size) 1/4)))
  (set! 2nd-gen-size (round (* (heap-size) 7/8)))
  (set! 2nd-gen-start (+ 4 1st-gen-size))
  (set! status-word 1st-gen-size)
  (set! volume-word (+ 1 1st-gen-size))
  (set! step-count-word (+ 2 1st-gen-size))
  (set! tracing-head-word (+ 3 1st-gen-size))
  (set! table-start-word 2nd-gen-size)
  (heap-set! status-word 'not-in-gc)
  (heap-set! volume-word 0)
  (heap-set! step-count-word 0)
  (heap-set! tracing-head-word 0)
  (heap-set! table-start-word (+ table-start-word 1)))

;; gc:deref : loc -> heap-value
;; must signal an error if fl-loc doesn't point to a flat value
(define (gc:deref fl-loc)
  (cond
    [(gc:flat? fl-loc) (heap-ref/check! (+ fl-loc 1))]
    [else (error 'gc:deref "non-flat @ ~s" fl-loc)]))

;; track/loc : loc -> loc
;; if loc points to a flat or pair or proc, then return loc
;; else if loc points to a frwd, return the frwd address
(define (track/loc loc)
  (case (heap-ref/check! loc)
    [(flat grey-flat white-flat pair grey-pair white-pair proc grey-proc white-proc vector grey-vector white-vector struct grey-struct white-struct struct-instance grey-struct-instance white-struct-instance) loc]
    [(frwd) (heap-ref/check! (+ loc 1))]
    [else (error 'track/loc "wrong tag ~s at ~a" (heap-ref/check! loc) loc)]))

;; gc:alloc-flat : heap-value -> loc
(define (gc:alloc-flat fv)
  (define ptr (alloc 2 #f #f))
  (heap-set! ptr 'flat)
  (heap-set! (+ ptr 1) fv)
  ptr)

;; gc:cons : loc loc -> loc
;; hd and tl are guaranteed to have been earlier
;; results from either gc:alloc-flat or gc:cons
(define (gc:cons hd tl)
  (define ptr (alloc 3 hd tl))
  (define head (track/loc hd))
  (define tail (track/loc tl))
  (when (and (= ptr 2)
             (or (need-forwarding-pointers? hd)
                 (need-forwarding-pointers? tl)))
    (free-1st-gen))
  (heap-set! ptr 'pair)
  (heap-set! (+ ptr 1) head)
  (heap-set! (+ ptr 2) tail)
  ptr)

;; gc:first : loc -> loc
;; must signal an error of pr-loc does not point to a pair
(define (gc:first pr-loc)
  (if (equal? (heap-ref/check! pr-loc) 'pair)
      (heap-ref/check! (+ pr-loc 1))
      (error 'first "non pair @ ~s" pr-loc)))

;; gc:rest : loc -> loc
;; must signal an error of pr-loc does not point to a pair
(define (gc:rest pr-loc)
  (if (equal? (heap-ref/check! pr-loc) 'pair)
      (heap-ref/check! (+ pr-loc 2))
      (error 'first "non pair @ ~s" pr-loc)))

;; gc:flat? : loc -> boolean
;; loc is guaranteed to have been an earlier
;; result from either gc:alloc-flat or gc:cons
(define (gc:flat? loc)
  (case (heap-ref/check! loc)
    [(flat grey-flat white-flat) #t]
    [(frwd) (gc:flat? (heap-ref/check! (+ loc 1)))]
    [else #f]))

;; gc:cons? : loc -> boolean
;; loc is guaranteed to have been an earlier
;; result from either gc:alloc-flat or gc:cons
(define (gc:cons? loc)
  (case (heap-ref/check! loc)
    [(pair grey-pair white-pair) #t]
    [(frwd) (gc:cons? (heap-ref/check! (+ loc 1)))]
    [else #f]))

;; gc:set-first! : loc loc -> void
;; must signal an error of pr-loc does not point to a pair
(define (gc:set-first! pr-loc new)
  (if (equal? (heap-ref/check! pr-loc) 'pair)
      (begin
        (heap-set! (+ pr-loc 1) new)
        (when (and (2nd-gen? pr-loc)
                   (1st-gen? new))
          (table/alloc (+ pr-loc 1) new)))
      (error 'set-first! "non pair")))

;; gc:set-rest! : loc loc -> void
;; must signal an error of pr-loc does not point to a pair
(define (gc:set-rest! pr-loc new) 
  (if (equal? (heap-ref/check! pr-loc) 'pair)
      (begin
        (heap-set! (+ pr-loc 2) new)
        (when (and (2nd-gen? pr-loc)
                   (1st-gen? new))
          (table/alloc (+ pr-loc 2) new)))
      (error 'set-rest! "non pair")))

;; gc:closure : heap-value (vectorof loc) -> loc
;; allocates a closure with 'code-ptr' and the free variables
;; in the vector 'free-vars'.
(define (gc:closure code-ptr free-vars)
  (define fv-count (vector-length free-vars))
  (define fv-vars (vector->roots free-vars))
  (define next (alloc (+ fv-count 3)
                      fv-vars
                      '()))
  (define updated-free-vars (for/vector ([v (in-vector free-vars)])
                      (track/loc v)))
  (when (and (= next 2)
             (need-forwarding-pointers? fv-vars))
    (free-1st-gen))
  (heap-set! next 'proc)
  (heap-set! (+ next 1) code-ptr)
  (heap-set! (+ next 2) fv-count)
  (for ([x (in-range 0 fv-count)])
    (heap-set! (+ next 3 x)
               (vector-ref updated-free-vars x)))
  next)

;; gc:closure-code-ptr : loc -> heap-value
;; given a location returned from an earlier allocation
;; check to see if it is a closure; if not signal an
;; error. if so, return the code-ptr
(define (gc:closure-code-ptr loc)
  (if (gc:closure? loc)
      (heap-ref/check! (+ (track/loc loc) 1))
      (error 'gc:closure-code-ptr "non closure at ~a" loc)))

;; gc:closure-env-ref : loc number -> loc
;; given a location returned from an earlier allocation
;; check to see if it is a closure; if not signal an
;; error. if so, return the 'i'th variable in the closure
(define (gc:closure-env-ref loc i)
  (if (gc:closure? loc)
      (heap-ref/check! (+ (track/loc loc) 3 i))
      (error 'gc:closure-env-ref "non closure at ~a" loc)))

;; gc:closure? : loc -> boolean
;; determine if a previously allocated location 
;; holds a closure
(define (gc:closure? loc)
  (case (heap-ref/check! loc)
    [(proc grey-proc white-proc) #t]
    [(frwd) (gc:closure? (heap-ref/check! (+ loc 1)))]
    [else #f]))

;; vector related
(define (gc:vector length loc)
  (define next (alloc (+ length 2) loc #f))
  (define l (track/loc loc))
  (when (and (= next 2)
             (need-forwarding-pointers? loc))
    (free-1st-gen))
  (heap-set! next 'vector)
  (heap-set! (+ next 1) length)
  (for ([i (in-range length)])
    (heap-set! (+ next 2 i) l))
  next)

(define (gc:vector? loc)
  (case (heap-ref loc)
    [(vector grey-vector white-vector) #t]
    [(frwd) (gc:vector? (heap-ref/check! (+ loc 1)))]
    [else #f]))

(define (gc:vector-length loc)
  (if (gc:vector? loc)
      (heap-ref (+ loc 1))
      (error 'gc:vector-length "non vector @ ~s" loc)))

(define (gc:vector-ref loc number)
  (unless (gc:vector? loc)
    (error 'gc:vector-ref "non vector @ ~s" loc))
  (cond
    [(< number (gc:vector-length loc)) (heap-ref (+ loc 2 number))]
    [else (error 'gc:vector-ref 
                 "vector @ ~s index ~s  out of range"
                 loc number)]))

(define (gc:vector-set! loc number thing)
  (unless (gc:vector? loc)
    (error 'gc:vector-set! "non vector @ ~s" loc))
  (cond 
    [(< number (gc:vector-length loc)) (heap-set! (+ loc 2 number) thing)]
    [else (error 'gc:vector-set! 
                 "vector @ ~s index ~s out of range"
                 loc number)]))

;; struct related
(define (gc:alloc-struct name parent fields-count)
  (define next (alloc 4 parent #f))
  (define p (and parent (track/loc parent)))
  (when (and (= next 2)
             (need-forwarding-pointers? parent))
    (free-1st-gen))
  (heap-set! next 'struct)
  (heap-set! (+ next 1) name)
  (heap-set! (+ next 2) p)
  (heap-set! (+ next 3) fields-count)
  next)

(define (gc:alloc-struct-instance s fields-value)
  (define fv-count (vector-length fields-value))
  (define fv-roots (vector->roots fields-value))
  (define next (alloc (+ fv-count 2)
                      s
                      fv-roots))
  (define ss (track/loc s))
  (define fields (for/vector ([v (in-vector fields-value)])
                   (track/loc v)))
  (when (and (= next 2)
             (or (need-forwarding-pointers? s)
                 (need-forwarding-pointers? fv-roots)))
    (free-1st-gen))
  (heap-set! (+ next 1) ss)
  (for ([x (in-range 0 fv-count)])
    (heap-set! (+ next 2 x)
               (vector-ref fields-value x)))
  (heap-set! next 'struct-instance)
  next)

(define (struct? loc)
  (case (heap-ref loc)
    [(struct grey-struct white-struct) #t]
    [(frwd) (struct? (heap-ref/check! (+ loc 1)))]
    [else #f]))

(define (gc:struct-pred s instance)
  (and (struct? s)
       (gc:struct-pred-helper s (heap-ref (+ instance 1)))))

(define (gc:struct-pred-helper target s)
  (and s
       (or (= target s)
           (gc:struct-pred-helper target (heap-ref (+ s 2))))))

(define (gc:struct-select s instance index)
  (unless (gc:struct-pred s instance)
    (error 'gc:struct-select "value at ~a is not an instance of ~a" 
           instance
           (heap-ref (+ 1 s))))
  (heap-ref (+ instance 2 index)))

(define (table/alloc pointer target)
  (define next (heap-ref/check! table-start-word))
  (cond
    [(>= (+ next 2) (heap-size))
     (error 'table/alloc "no space for indirection table")]
    [else
     (heap-set! next pointer)
     (heap-set! (+ next 1) target)
     (heap-set! table-start-word (+ next 2))]))

;; alloc : number[size] roots roots -> loc
(define (alloc n some-roots more-roots)
  (define addr (heap-ref/check! 0))
  (cond 
    [(<= (+ addr n) 1st-gen-size)
     (heap-set! 0 (+ addr n))
     addr]
    [else
     (collect-garbage some-roots more-roots)
     (unless (or (need-forwarding-pointers? some-roots)
                 (need-forwarding-pointers? more-roots))
       (free-1st-gen))
     (heap-set! 0 2)
     (unless (<= (+ 2 n) 1st-gen-size)
       (error 'alloc "no space"))
     (heap-set! 0 (+ 2 n))
     2]))

(define (need-forwarding-pointers? thing)
  (cond
    [(list? thing) (ormap 1st-gen? thing)]
    [(root? thing) (1st-gen? (read-root thing))]
    [(number? thing) (1st-gen? thing)]
    [else thing]))

;; collect-garbage : roots roots -> void
(define (collect-garbage some-roots more-roots)
  (forward/roots (get-root-set))
  (forward/roots some-roots)
  (forward/roots more-roots)
  (forward/pointers (+ 1 table-start-word)))

;; forward/roots : loc/(listof loc) -> loc
;; move every thing reachable from 'roots'
;; to the to space
(define (forward/roots thing)
  (cond
    [(list? thing)
     (for-each forward/roots thing)]
    [(root? thing)
     (define new-addr (forward/loc (read-root thing)))
     (set-root! thing new-addr)
     (forward/ref new-addr)]
    [(number? thing)
     (forward/ref (forward/loc thing))]))

;; forward/loc : loc -> loc
;; move object to the other semi-space
;; and return the new addr of moved object
(define (forward/loc loc)
  (cond
    [(1st-gen? loc)
     (case (heap-ref/check! loc)
       [(flat) (define new-addr (copy/alloc 2 #f #f))
               (heap-set! new-addr 'flat)
               (heap-set! (+ new-addr 1) (heap-ref/check! (+ loc 1))) 
               (heap-set! loc 'frwd)
               (heap-set! (+ loc 1) new-addr)
               new-addr]
       [(pair) (define new-addr (copy/alloc 3
                                            (heap-ref/check! (+ loc 1))
                                            (heap-ref/check! (+ loc 2))))
               (heap-set! new-addr 'pair)
               (heap-set! (+ new-addr 1) (track/loc (heap-ref/check! (+ loc 1))))
               (heap-set! (+ new-addr 2) (track/loc (heap-ref/check! (+ loc 2))))
               (heap-set! loc 'frwd)
               (heap-set! (+ loc 1) new-addr)
               new-addr]
       [(proc) (define length (+ 3 (heap-ref/check! (+ loc 2))))
               (define free-vars (build-vector (- length 3)
                                               (lambda (i)
                                                 (heap-ref/check! (+ loc 3 i)))))
               (define new-addr (copy/alloc length free-vars '()))
               (for ([x (in-range 0 3)])
                 (heap-set! (+ new-addr x) (heap-ref/check! (+ loc x))))
               (for ([x (in-range 3 length)])
                 (heap-set! (+ new-addr x) (track/loc (heap-ref/check! (+ loc x)))))
               (heap-set! loc 'frwd)
               (heap-set! (+ loc 1) new-addr)
               new-addr]
       [(vector) (define var-count (heap-ref/check! (+ loc 1)))
                 (define vars (build-vector var-count
                                            (lambda (i)
                                              (heap-ref/check! (+ loc 2 i)))))
                 (define new-addr (copy/alloc (+ 2 var-count) vars '()))
                 (for ([x (in-range 0 2)])
                   (heap-set! (+ new-addr x) (heap-ref/check! (+ loc x))))
                 (for ([x (in-range var-count)])
                   (heap-set! (+ new-addr 2 x) (track/loc (heap-ref/check! (+ loc 2 x)))))
                 (heap-set! loc 'frwd)
                 (heap-set! (+ loc 1) new-addr)
                 new-addr]
       [(struct) (define new-addr (copy/alloc 4 (heap-ref/check! (+ loc 2)) #f))
                 (heap-set! new-addr 'struct)
                 (heap-set! (+ new-addr 1) (heap-ref/check! (+ loc 1)))
                 (define parent (heap-ref/check! (+ loc 2)))
                 (heap-set! (+ new-addr 2) (and parent (track/loc parent)))
                 (heap-set! (+ new-addr 3) (heap-ref/check! (+ loc 3)))
                 (heap-set! loc 'frwd)
                 (heap-set! (+ loc 1) new-addr)
                 new-addr]
       [(struct-instance) (define var-count (heap-ref/check! (+ 3 (heap-ref/check! (+ loc 1)))))
                          (define vars (build-vector var-count
                                                     (lambda (i)
                                                       (heap-ref/check! (+ loc 2 i)))))
                          (define new-addr (copy/alloc (+ 2 var-count) vars '()))
                          (heap-set! new-addr 'struct-instance)
                          (heap-set! (+ new-addr 1) (track/loc (heap-ref/check! (+ loc 1))))
                          (for ([x (in-range var-count)])
                            (heap-set! (+ new-addr 2 x) (track/loc (heap-ref/check! (+ loc 2 x)))))
                          (heap-set! loc 'frwd)
                          (heap-set! (+ loc 1) new-addr)
                          new-addr]
       [(frwd) (heap-ref/check! (+ loc 1))]
       [else (error 'forward/loc "wrong tag ~s at ~a" (heap-ref/check! loc) loc)])]
    [else loc]))

;; forward/ref : loc -> loc
;; move the referenced object to the other semi-space
;; and return the new addr of moved object
(define (forward/ref loc)
  (case (heap-ref/check! loc)
    [(flat grey-flat white-flat) (void)]
    [(pair grey-pair white-pair) (gc:set-first! loc (forward/loc (heap-ref/check! (+ loc 1))))
            (gc:set-rest! loc (forward/loc (heap-ref/check! (+ loc 2))))
            (forward/ref (heap-ref (+ loc 1)))
            (forward/ref (heap-ref (+ loc 2)))]
    [(proc grey-proc white-proc) (define fv-count (heap-ref/check! (+ loc 2)))
            (for ([x (in-range 0 fv-count)])
              (define l (+ loc 3 x))
              (heap-set! l (forward/loc (heap-ref/check! l)))
              (forward/ref (heap-ref/check! l)))]
    [(vector grey-vector white-vector) (define var-count (heap-ref/check! (+ loc 1)))
              (for ([x (in-range var-count)])
                (define l (+ loc 2 x))
                (heap-set! l (forward/loc (heap-ref/check! l)))
                (forward/ref (heap-ref/check! l)))]
    [(struct grey-struct white-struct) (define parent (heap-ref/check! (+ loc 2)))
              (when parent
                (heap-set! (+ loc 2) (forward/loc parent))
                (forward/ref (heap-ref/check! (+ loc 2))))]
    [(struct-instance grey-struct-instance white-struct-instance) (define fields-count (heap-ref/check! (+ 3 (heap-ref/check! (+ loc 1)))))
                       (heap-set! (+ loc 1) (forward/loc (heap-ref/check! (+ loc 1))))
                       (forward/ref (heap-ref/check! (+ loc 1)))
                       (for ([x (in-range fields-count)])
                         (define l (+ loc 2 x))
                         (heap-set! l (forward/loc (heap-ref/check! l)))
                         (forward/ref (heap-ref/check! l)))]
    [else (error 'forward/ref "wrong tag at ~a" loc)]))

(define (forward/pointers loc)
  (cond
    [(= loc (heap-size)) (void)]
    [(equal? 'free (heap-ref/check! loc)) (void)]
    [else
     (define new-addr (forward/loc (heap-ref/check! (+ loc 1))))
     (heap-set! (+ loc 1) new-addr)
     (forward/ref new-addr)
     (forward/pointers (+ loc 2))]))

(define (free-1st-gen)
  (for ([i (in-range 2 1st-gen-size)])
    (heap-set! i 'free)))

(define (1st-gen? loc)
  (and (>= loc 2)
       (< loc 1st-gen-size)))

(define (2nd-gen? loc)
  (and (>= loc 1st-gen-size)
       (< loc 2nd-gen-size)))

(define (heap-ref/check! loc)
  (unless (number? loc)
    (error 'heap-ref/check! "should be a location? at ~a" loc))
  (heap-ref loc))

(define (find-free-space start size)
  (cond
    [(= start 2nd-gen-size) #f]
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
    [(= start 2nd-gen-size) #f]
    [else 
     (and (eq? 'free (heap-ref/check! start))
          (n-free-blocks? (+ start 1)
                          (- size 1)))]))

(define (make-pointers-to-2nd-gen-roots start)
  (cond
    [(= start 1st-gen-size)
     (void)]
    [else
     (case (heap-ref/check! start)
       [(flat) (make-pointers-to-2nd-gen-roots (+ start 2))]
       [(pair) (define one-loc (heap-ref/check! (+ start 1)))
               (when (2nd-gen? one-loc) (traverse/roots-white one-loc))
               (define another-loc (heap-ref/check! (+ start 2)))
               (when (2nd-gen? another-loc) (traverse/roots-white another-loc))
               (make-pointers-to-2nd-gen-roots (+ start 3))]
       [(proc) (define fv-counts (heap-ref/check! (+ start 2)))
               (for ([i (in-range fv-counts)])
                 (define loc (heap-ref/check! (+ start 3 i)))
                 (when (2nd-gen? loc) (traverse/roots-white loc)))
               (make-pointers-to-2nd-gen-roots (+ start 3 fv-counts))]
       [(vector) (define element-count (heap-ref/check! (+ start 1)))
                 (for ([i (in-range element-count)])
                   (define loc (heap-ref/check! (+ start 2 i)))
                   (when (2nd-gen? loc) (traverse/roots-white loc)))
                 (make-pointers-to-2nd-gen-roots (+ start 2 element-count))]
       [(struct) (define parent (heap-ref/check! (+ start 2)))
                 (when (and parent (2nd-gen? parent))
                   (traverse/roots-white parent))
                 (make-pointers-to-2nd-gen-roots (+ start 4))]
       [(struct-instance) (define fields-count (heap-ref/check! (+ 3 (heap-ref/check! (+ start 1)))))
                          (define struct-loc (heap-ref/check! (+ start 1)))
                          (when (2nd-gen? struct-loc)
                            (traverse/roots-white struct-loc))
                          (for ([i (in-range fields-count)])
                            (define loc (heap-ref/check! (+ start 2 i)))
                            (when (2nd-gen? loc) (traverse/roots-white loc)))
                          (make-pointers-to-2nd-gen-roots (+ start 2 fields-count))]
       [(frwd) (define loc (heap-ref/check! (+ start 1)))
               (case (heap-ref/check! loc)
                 [(flat) (make-pointers-to-2nd-gen-roots (+ start 2))]
                 [(pair) (make-pointers-to-2nd-gen-roots (+ start 3))]
                 [(proc) (make-pointers-to-2nd-gen-roots (+ start 3 (heap-ref/check! (+ loc 2))))]
                 [(vector) (make-pointers-to-2nd-gen-roots (+ start 2 (heap-ref/check! (+ loc 1))))]
                 [(struct) (make-pointers-to-2nd-gen-roots (+ start 4))]
                 [(struct-instance) (make-pointers-to-2nd-gen-roots (+ start 2 (heap-ref/check! (+ 3 (heap-ref/check! (+ loc 1))))))])]
       [(free) (void)]
       [else (error 'make-pointers-to-2nd-gen-roots "wrong tag at ~a" start)])]))

(define (copy/alloc n some-roots more-roots)
  (define next (find-free-space 2nd-gen-start n))
  (cond
    [next
      (heap-set! volume-word (+ n (heap-ref volume-word)))
      (heap-set! step-count-word (+ n (heap-ref step-count-word)))
      (traverse/roots-white (get-root-set))
      ;; scan young generation for pointers to be roots
      ;; should be less frequent
      (make-pointers-to-2nd-gen-roots 2)
      (when (>= (heap-ref step-count-word) step-length)
        (traverse/incre-mark (next/cont)))
      (let ([loc (find-free-space next n)])
        (if loc
          loc
          (error 'copy/alloc "old generation gc failed")))]
    [else (error 'copy/alloc "old generation gc failed")]))

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

(define (free/mark-white! i)
  (when (< i 2nd-gen-size)
    (case (heap-ref i)
      [(pair) (heap-set! i 'white-pair)
              (free/mark-white! (+ i 3))]
      [(flat) (heap-set! i 'white-flat)
              (free/mark-white! (+ i 2))]
      [(proc) (heap-set! i 'white-proc)
              (free/mark-white! (+ i 3 (heap-ref (+ i 2))))]
      [(vector) (heap-set! i 'white-vector)
                (free/mark-white! (+ i 2 (heap-ref (+ i 1))))]
      [(struct) (heap-set! i 'white-struct)
                (free/mark-white! (+ i 4))]
      [(struct-instance) (heap-set! i 'white-struct-instance)
                         (free/mark-white! (+ i 2 (heap-ref (+ 3 (heap-ref (+ i 1))))))]
      [(white-pair cont) (heap-set! i 'free)
                         (heap-set! (+ i 1) 'free)
                         (heap-set! (+ i 2) 'free)
                         (heap-set! volume-word (- (heap-ref volume-word) 3))
                         (free/mark-white! (+ i 3))]
      [(white-flat) (heap-set! i 'free)
                    (heap-set! (+ i 1) 'free)
                    (heap-set! volume-word (- (heap-ref volume-word) 2))
                    (free/mark-white! (+ i 2))]
      [(white-proc) (define closure-size (heap-ref (+ i 2)))
                    (for ([dx (in-range 0 (+ closure-size 3))])
                      (heap-set! (+ i dx) 'free))
                    (heap-set! volume-word (- (heap-ref volume-word) (+ 3 closure-size)))
                    (free/mark-white! (+ i 3 closure-size))]
      [(white-vector) (define size (heap-ref (+ i 1)))
                      (for ([dx (in-range (+ size 2))])
                           (heap-set! (+ i dx) 'free))
                      (heap-set! volume-word (- (heap-ref volume-word) (+ 2 size)))
                      (free/mark-white! (+ i 2 size))]
      [(white-struct) (heap-set! i 'free)
                      (heap-set! (+ i 1) 'free)
                      (heap-set! (+ i 2) 'free)
                      (heap-set! (+ i 3) 'free)
                      (heap-set! volume-word (- (heap-ref volume-word) 4))
                      (free/mark-white! (+ i 4))]
      [(white-struct-instance) (heap-set! i 'free)
                               (heap-set! (+ i 1) 'free)
                               (define fields-size (heap-ref (+ 3 (heap-ref (+ i 1)))))
                               (for ([dx (in-range 0 fields-size)])
                                    (heap-set! (+ i dx) 'free))
                               (heap-set! volume-word (- (heap-ref volume-word) (+ 2 fields-size)))
                               (free/mark-white! (+ i 2 fields-size))]
      [(free) (free/mark-white! (+ i 1))]
      [else (error 'free-white! "unknown tag ~s" (heap-ref i))])))

(define (traverse/incre-mark loc) ;; loc of cont
  (unless (or (equal? 'cont (heap-ref loc))
              (= loc 0))
    (error 'traverse/incre-mark "not cont at ~a" loc))
  (cond
    [(= loc 0) (heap-set! step-count-word 0)
               (heap-set! tracing-head-word 0)
               (heap-set! status-word 'free/mark-white!)
               (free/mark-white! 2nd-gen-start)
               (check/tag 2nd-gen-start)
               (heap-set! status-word 'not-in-gc)
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

(define (heap-cont/check)
  (check/cont 2nd-gen-start)
  (heap-check 2nd-gen-start))

;; check what allocated to cont
(define (check/cont-alloc loc thing)
  (cond
    [(= loc tracing-head-word)
     (unless (or (equal? (heap-ref thing) 'cont)
                 (= thing 0))
       (error 'check/cont-alloc "not a cont at location ~a" thing))]
    [else (error 'check/cont-alloc "meant to check location 3")]))

(define (heap-check loc)
  (when (< loc 2nd-gen-size)
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
  (when (< loc 2nd-gen-size)
    (case (heap-ref loc)
      [(white-pair) (check/tag (+ loc 3))]
      [(white-flat) (check/tag (+ loc 2))]
      [(white-proc) (check/tag (+ loc 3 (heap-ref (+ loc 2))))]
      [(white-vector) (check/tag (+ loc 2 (heap-ref (+ loc 1))))]
      [(white-struct) (check/tag (+ loc 4))]
      [(white-struct-instance) (check/tag (+ loc 2 (heap-ref (+ 3 (heap-ref (+ loc 1))))))]
      [(free) (check/tag (+ loc 1))]
      [else (error 'check/tag "wrong tag at ~a" loc)])))

(define (check/cont loc)
  (when (< loc 2nd-gen-size)
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
  (define loc (heap-ref tracing-head-word))
  (cond
    [(= loc 0) 0]
    [else
      (check/cont-alloc tracing-head-word (heap-ref (+ loc 2)))
      (heap-set! tracing-head-word (heap-ref (+ loc 2)))
      (heap-cont/check)
      loc]))

(define (push/cont ptr)
  (mark-grey ptr)
  (case (heap-ref ptr)
    [(pair flat proc vector strut struct-instance) (void)]
    [else
      (define loc (find-free-space 2nd-gen-start 3))
      (define head (heap-ref tracing-head-word))
      (heap-set! loc 'cont)
      (heap-set! (+ loc 1) ptr)
      (heap-set! (+ loc 2) head)
      (check/cont-alloc tracing-head-word loc)
      (heap-set! tracing-head-word loc)
      (heap-cont/check)]))

(define (continue/incre-mark)
  (if (step/finished?)
    (heap-set! step-count-word 0)
    (traverse/incre-mark (next/cont))))

(define (clean/cont loc)
  (heap-set! loc 'free)
  (heap-set! (+ loc 1) 'free)
  (heap-set! (+ loc 2) 'free)
  (heap-cont/check))

(define (step/count n)
  (heap-set! step-count-word (- (heap-ref step-count-word) n))
  (heap-cont/check))

(define (step/finished?)
  (<= (heap-ref step-count-word) 0))

#|
(print-only-errors #t)
(define test-heap1 (make-vector 48 'f))
;; init-allocator
(test (with-heap test-heap1
                 (init-allocator)
                 test-heap1)
      (vector 2 'left 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 43 'free
              'free 'free 'free 'free))
;; alloc
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (alloc 2 #f #f))
      4)
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (alloc 2 #f #f)
                 test-heap1)
      (vector 6 'left 'flat 0
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 43 'free
              'free 'free 'free 'free))
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:alloc-flat 0)
                 (alloc 2 #f #f))
      7)
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:alloc-flat 0)
                 (alloc 2 #f #f)
                 test-heap1)
      (vector 9 'right 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 43 'free
              'free 'free 'free 'free))
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (alloc 3 2 2))
      4)
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:alloc-flat 0)
                 (alloc 3 2 2)
                 test-heap1)
      (vector 10 'right 'frwd 12
              'flat 0 'free 'free
              'free 'free 'free 'free
              'flat 0 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 43 'free
              'free 'free 'free 'free))
;; gc:alloc-flat
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 test-heap1)
      (vector 4 'left 'flat 0
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 43 'free
              'free 'free 'free 'free))
(let ([v (make-vector 56 'f)])
  (test (with-heap v
                   (init-allocator)
                   (gc:alloc-flat 0)
                   (gc:alloc-flat 0)
                   (gc:alloc-flat 0)
                   (gc:alloc-flat 0)
                   v)
        (vector 10 'right 'free 'free
                'free 'free 'free 'free
                'flat 0 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 50 'free 'free
                'free 'free 'free 'free)))
;; gc:cons
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:cons 2 2)
                 test-heap1)
      (vector 5 'left 'pair 2
              2 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 43 'free
              'free 'free 'free 'free))
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:cons 2 2)
                 test-heap1)
      (vector 7 'left 'flat 0
              'pair 2 2 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 43 'free
              'free 'free 'free 'free))
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:alloc-flat 0)
                 (gc:cons 2 2)
                 test-heap1)
      (vector 10 'right 'free 'free
              'free 'free 'free 'pair
              12 12 'free 'free
              'flat 0 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 43 'free
              'free 'free 'free 'free))
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:alloc-flat 1)
                 (gc:cons 2 4))
      7)
(let ([v (make-vector 56 'f)])
  (test (with-heap v
                   (init-allocator)
                   (gc:alloc-flat 0)
                   (gc:alloc-flat 0)
                   (gc:alloc-flat 0)
                   (gc:cons 6 6)
                   v)
        (vector 11 'right 'free 'free 
                'free 'free 'free 'free 
                'pair 14 14 'free 
                'free 'free 'flat 0
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 50 'free 'free
                'free 'free 'free 'free)))
;; gc:closure
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:closure 'f (vector))
                 test-heap1)
      (vector 7 'left 'flat 0
              'proc 'f 0 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 43 'free
              'free 'free 'free 'free))
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:closure 'f (vector 2)))
      7)
(let ([v (make-vector 56 'f)])
  (test (with-heap v
                   (init-allocator)
                   (gc:alloc-flat 0)
                   (gc:alloc-flat 0)
                   (gc:closure 'f (vector 2))
                   v)
        (vector 12 'right 'free 'free 
                'free 'free 'free 'free 
                'proc 'f 1 14 
                'free 'free 'flat 0
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 50 'free 'free
                'free 'free 'free 'free)))
;; combination
(define test-heap3 (make-vector 56 'f))
(test (with-heap test-heap3
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:alloc-flat 0)
                 (gc:closure 'f (vector))
                 (gc:alloc-flat 0)
                 test-heap3)
      (vector 13 'right 'free 'free
              'free 'free 'free 'free
              'proc 'f 0 'flat
              0 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 50 'free 'free
              'free 'free 'free 'free))
(test (with-heap test-heap3
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:alloc-flat 0)
                 (gc:closure 'f (vector))
                 (change-active-semi-space)
                 (forward/loc 8)
                 test-heap3)
      (vector 5 'left 'proc 'f 
              0 'free 'free 'free 
              'frwd 2 0 'free 
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 50 'free 'free
              'free 'free 'free 'free))
(test (with-heap test-heap3
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:alloc-flat 0)
                 (gc:closure 'f (vector))
                 (gc:alloc-flat 0)
                 (gc:cons 8 8)
                 test-heap3)
      (vector 5 'left 'pair 14
              14 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'proc 'f
              0 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 50 'free 'free
              'free 'free 'free 'free))
(test (with-heap test-heap3
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:alloc-flat 0)
                 (gc:closure 'f (vector 2))
                 test-heap3)
      (vector 12 'right 'free 'free
              'free 'free 'free 'free
              'proc 'f 1 14
              'free 'free 'flat 0
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 50 'free 'free
              'free 'free 'free 'free))
(test (with-heap test-heap3
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:alloc-flat 0)
                 (gc:closure 'f (vector 2))
                 (gc:cons 8 14)
                 test-heap3)
      (vector 5 'left 'pair 16
              14 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'flat 0
              'proc 'f 1 14
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 50 'free 'free
              'free 'free 'free 'free))
|#
