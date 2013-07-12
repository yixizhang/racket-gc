#lang plai/gc2/collector
(require racket/stream)

;; config for collection
(define step-length 10)
(define 1st-gen-size "size of young generation")
(define 2nd-gen-size "size of old generation")
(define 2nd-gen-start "start of old generation")
(define 2nd-gen-alloc-start "start position to allocate objects in old generation")
(define alloc-word "start of objects allocation for young generation")
(define status-word "current status of old generation incremental gc: in/out")
(define gc-phase "current phase of old generation incremental gc: marking/not")
(define free-list-head "head of free list")
(define step-count-word "work count for each tracing round")
(define tracing-head-word "head of tracing tree")
(define table-start-word "start position of table for cross-generation pointers")

(define (init-allocator)
  (unless (= 0 (modulo (heap-size) 256))
    (error 'init-allocator "heap size is not power of 256"))
  (for ([i (in-range 0 (heap-size))])
    (heap-set! i 'free))
  (set! 1st-gen-size (round (* (heap-size) 1/4)))
  (set! 2nd-gen-size (heap-size))
  (set! alloc-word 0)

  (define table-size (round (* (heap-size) 1/8)))
  (set! 2nd-gen-start (+ 1st-gen-size table-size))

  (set! 2nd-gen-alloc-start (+ 4 2nd-gen-start))
  (set! status-word 2nd-gen-start)
  (set! gc-phase 'not)
  (set! free-list-head (+ 1 2nd-gen-start))
  (set! step-count-word (+ 2 2nd-gen-start))
  (set! tracing-head-word (+ 3 2nd-gen-start))
  (set! table-start-word 1st-gen-size)

  (heap-set! alloc-word 1)
  (heap-set! status-word 'out)
  (heap-set! 2nd-gen-alloc-start 'free-n)
  (heap-set! (+ 1 2nd-gen-alloc-start) #f)
  (heap-set! (+ 2 2nd-gen-alloc-start) (- 2nd-gen-size 2nd-gen-alloc-start))
  (heap-set! free-list-head 2nd-gen-alloc-start)
  (heap-set! step-count-word 0)
  (heap-set! tracing-head-word #f)
  (heap-set! table-start-word (+ table-start-word 1)))

;; gc:deref : loc -> heap-value
;; must signal an error if fl-loc doesn't point to a flat value
(define (gc:deref fl-loc)
  (cond
    [(gc:flat? fl-loc) (heap-ref (+ fl-loc 1))]
    [else (error 'gc:deref "non-flat @ ~s" fl-loc)]))

;; track/loc : loc -> loc
;; if loc points to a flat or pair or proc, then return loc
;; else if loc points to a frwd, return the frwd address
(define (track/loc loc)
  (case (heap-ref loc)
    [(free) (error 'track/loc "wrong tag ~s @ ~a" (heap-ref loc) loc)]
    [(frwd) (heap-ref (+ loc 1))]
    [else loc]))

;; gc:alloc-flat : heap-value -> loc
(define (gc:alloc-flat fv)
  (define ptr (alloc 2 #f #f))
  (heap-set! ptr 'flat)
  (heap-set! (+ ptr 1) fv)
  ptr)

;; ->location : (or/c location? root?) . -> . location?
(define (->location thing)
  (cond
    [(location? thing) thing]
    [(root? thing) (read-root thing)]))

;; gc:cons : loc loc -> loc
;; hd and tl are guaranteed to have been earlier
;; results from either gc:alloc-flat or gc:cons
(define (gc:cons hd tl)
  (define ptr (alloc 3 hd tl))
  (define hd/loc (->location hd))
  (define tl/loc (->location tl))
  (define head (track/loc hd/loc))
  (define tail (track/loc tl/loc))
  (when (and (= ptr 1)
             (or (need-forwarding-pointers? hd/loc)
                 (need-forwarding-pointers? tl/loc)))
    (free-1st-gen))
  (heap-set! ptr 'pair)
  (heap-set! (+ ptr 1) head)
  (heap-set! (+ ptr 2) tail)
  ptr)

;; gc:first : loc -> loc
;; must signal an error of pr-loc does not point to a pair
(define (gc:first pr-loc)
  (if (gc:cons? pr-loc)
      (heap-ref (+ (track/loc pr-loc) 1))
      (error 'first "non pair @ ~s" pr-loc)))

;; gc:rest : loc -> loc
;; must signal an error of pr-loc does not point to a pair
(define (gc:rest pr-loc)
  (if (gc:cons? pr-loc)
      (heap-ref (+ (track/loc pr-loc) 2))
      (error 'rest "non pair @ ~s" pr-loc)))

;; gc:flat? : loc -> boolean
;; loc is guaranteed to have been an earlier
;; result from either gc:alloc-flat or gc:cons
(define (gc:flat? loc)
  (case (heap-ref loc)
    [(flat grey-flat white-flat) #t]
    [(frwd) (gc:flat? (heap-ref (+ loc 1)))]
    [else #f]))

;; gc:cons? : loc -> boolean
;; loc is guaranteed to have been an earlier
;; result from either gc:alloc-flat or gc:cons
(define (gc:cons? loc)
  (case (heap-ref loc)
    [(pair grey-pair white-pair) #t]
    [(frwd) (gc:cons? (heap-ref (+ loc 1)))]
    [else #f]))

;; gc:set-first! : loc loc -> void
;; must signal an error of pr-loc does not point to a pair
(define (gc:set-first! pr-loc new)
  (cond
    [(gc:cons? pr-loc)
     (define loc (track/loc pr-loc))
     (heap-set! (+ loc 1) new)
     (when (and (2nd-gen? loc)
                (1st-gen? new))
       (table/alloc (+ loc 1) new))]
    [else (error 'set-first! "non pair @ ~s" pr-loc)]))

;; gc:set-rest! : loc loc -> void
;; must signal an error of pr-loc does not point to a pair
(define (gc:set-rest! pr-loc new) 
  (cond
    [(gc:cons? pr-loc)
     (define loc (track/loc pr-loc))
     (heap-set! (+ loc 2) new)
     (when (and (2nd-gen? loc)
                (1st-gen? new))
       (table/alloc (+ loc 2) new))]
    [else (error 'set-rest! "non pair @ ~s" pr-loc)])) 

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
  (when (and (= next 1)
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
      (heap-ref (+ (track/loc loc) 1))
      (error 'gc:closure-code-ptr "non closure at ~a" loc)))

;; gc:closure-env-ref : loc number -> loc
;; given a location returned from an earlier allocation
;; check to see if it is a closure; if not signal an
;; error. if so, return the 'i'th variable in the closure
(define (gc:closure-env-ref loc i)
  (if (gc:closure? loc)
      (heap-ref (+ (track/loc loc) 3 i))
      (error 'gc:closure-env-ref "non closure at ~a" loc)))

;; gc:closure? : loc -> boolean
;; determine if a previously allocated location 
;; holds a closure
(define (gc:closure? loc)
  (case (heap-ref loc)
    [(proc grey-proc white-proc) #t]
    [(frwd) (gc:closure? (heap-ref (+ loc 1)))]
    [else #f]))

;; vector related
(define (gc:vector length loc)
  (define next (alloc (+ length 2) loc #f))
  (define l (track/loc loc))
  (when (and (= next 1)
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
    [(frwd) (gc:vector? (heap-ref (+ loc 1)))]
    [else #f]))

(define (gc:vector-length loc)
  (if (gc:vector? loc)
      (heap-ref (+ (track/loc loc) 1))
      (error 'gc:vector-length "non vector @ ~s" loc)))

(define (gc:vector-ref loc number)
  (unless (gc:vector? loc)
    (error 'gc:vector-ref "non vector @ ~s" loc))

  (define v-loc (track/loc loc))
  (cond
    [(< number (gc:vector-length loc)) (heap-ref (+ v-loc 2 number))]
    [else (error 'gc:vector-ref 
                 "vector @ ~s index ~s out of range"
                 v-loc
                 number)]))

(define (gc:vector-set! loc number thing)
  (unless (gc:vector? loc)
    (error 'gc:vector-set! "non vector @ ~s" loc))

  (define v-loc (track/loc loc))
  (cond 
    [(< number (gc:vector-length v-loc)) 
     (heap-set! (+ v-loc 2 number) thing)
     (when (and (2nd-gen? v-loc)
                (1st-gen? thing))
       (table/alloc (+ v-loc 2 number) thing))]
    [else (error 'gc:vector-set! 
                 "vector @ ~s index ~s out of range"
                 v-loc 
                 number)]))

;; struct related
(define (gc:alloc-struct name parent fields-count)
  (define next (alloc 4 parent #f))
  (define p (and parent (track/loc parent)))
  (when (and (= next 1)
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
    [(frwd) (struct? (heap-ref (+ loc 1)))]
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

(define (gc:struct-set! s instance index value)
  (unless (gc:struct-pred s instance)
    (error 'gc:struct-select "value at ~a is not an instance of ~a" 
           instance
           (heap-ref (+ 1 s))))
  (heap-set! (+ instance 2 index) value))

(define (table/alloc pointer target)
  (define next (heap-ref table-start-word))
  (cond
    [(>= (+ next 2) (heap-size))
     (forward/pointers (+ 1 table-start-word))
     (heap-set! (+ 1 table-start-word) pointer)
     (heap-set! (+ 2 table-start-word) target)
     (heap-set! table-start-word (+ 3 table-start-word))]
    [else
     (heap-set! next pointer)
     (heap-set! (+ next 1) target)
     (heap-set! table-start-word (+ next 2))]))

;; alloc : number[size] roots roots -> loc
(define (alloc n some-roots more-roots)
  (define addr (heap-ref alloc-word))
  (cond 
    [(enough-spaces-on-young-heap? addr n)
     (heap-set! alloc-word (+ addr n))
     addr]
    [else
     (collect-garbage some-roots more-roots)
     (unless (or (need-forwarding-pointers? some-roots)
                 (need-forwarding-pointers? more-roots))
       (free-1st-gen))
     (unless (enough-spaces-on-young-heap? 1 n)
       (error 'alloc "object is larget than young generation"))
     (heap-set! alloc-word (+ 1 n))
     1]))

(define (enough-spaces-on-young-heap? start size)
  (<= (+ start size) 1st-gen-size))

(define (need-forwarding-pointers? thing)
  (cond
    [(list? thing) (ormap need-forwarding-pointers? thing)]
    [(root? thing) (1st-gen? (read-root thing))]
    [(number? thing) (1st-gen? thing)]
    [else thing]))

;; collect-garbage : roots roots -> void
(define (collect-garbage some-roots more-roots)
  ;; young->old live objects copying
  (make-pointers-to-2nd-gen-roots 1)
  (traverse/roots (get-root-set))
  (traverse/roots some-roots)
  (traverse/roots more-roots)
  (forward/pointers (+ 1 table-start-word))

  ;; only free/mark-white! when tree traversal is done
  (when (equal? #f (heap-ref tracing-head-word))
    (free/mark-white! 2nd-gen-alloc-start #f #f #f)))

(define (traverse/roots thing)
  (cond
    [(list? thing)
     (for-each traverse/roots thing)]
    [(root? thing)
     (cond
       [(1st-gen? (read-root thing)) (forward/roots thing)]
       [else (trace/roots-iff-white thing)])]
    [(number? thing)
     (cond
       [(1st-gen? thing) (forward/ref (forward/loc thing))]
       [else (when (white? thing) (push/cont thing))])]))

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
     (case (heap-ref loc)
       [(flat) 
        (define new-addr (copy/alloc 2 #f #f))
        (heap-set! new-addr 'flat)
        (heap-set! (+ new-addr 1) (heap-ref (+ loc 1))) 
        (heap-set! loc 'frwd)
        (heap-set! (+ loc 1) new-addr)
        new-addr]
       [(pair) 
        (define new-addr (copy/alloc 3
                                     (heap-ref (+ loc 1))
                                     (heap-ref (+ loc 2))))
        (heap-set! new-addr 'pair)
        (heap-set! (+ new-addr 1) (track/loc (heap-ref (+ loc 1))))
        (heap-set! (+ new-addr 2) (track/loc (heap-ref (+ loc 2))))
        (heap-set! loc 'frwd)
        (heap-set! (+ loc 1) new-addr)
        new-addr]
       [(proc) 
        (define length (+ 3 (heap-ref (+ loc 2))))
        (define free-vars (build-vector (- length 3)
                                        (lambda (i)
                                          (heap-ref (+ loc 3 i)))))
        (define new-addr (copy/alloc length free-vars '()))
        (heap-set! new-addr 'proc)
        (heap-set! (+ 1 new-addr) (heap-ref (+ 1 loc)))
        (heap-set! (+ 2 new-addr) (heap-ref (+ 2 loc)))
        (for ([x (in-range 3 length)])
             (heap-set! (+ new-addr x) (track/loc (heap-ref (+ loc x)))))
        (heap-set! loc 'frwd)
        (heap-set! (+ loc 1) new-addr)
        new-addr]
       [(vector) 
        (define var-count (heap-ref (+ loc 1)))
        (define vars (build-vector var-count
                                   (lambda (i)
                                     (heap-ref (+ loc 2 i)))))
        (define new-addr (copy/alloc (+ 2 var-count) vars '()))
        (heap-set! new-addr 'vector)
        (heap-set! (+ 1 new-addr) (heap-ref (+ loc 1)))
        (for ([x (in-range var-count)])
             (heap-set! (+ new-addr 2 x) (track/loc (heap-ref (+ loc 2 x)))))
        (heap-set! loc 'frwd)
        (heap-set! (+ loc 1) new-addr)
        new-addr]
       [(struct) 
        (define new-addr (copy/alloc 4 (heap-ref (+ loc 2)) #f))
        (heap-set! new-addr 'struct)
        (heap-set! (+ new-addr 1) (heap-ref (+ loc 1)))
        (define parent (heap-ref (+ loc 2)))
        (heap-set! (+ new-addr 2) (and parent (track/loc parent)))
        (heap-set! (+ new-addr 3) (heap-ref (+ loc 3)))
        (heap-set! loc 'frwd)
        (heap-set! (+ loc 1) new-addr)
        new-addr]
       [(struct-instance) 
        (define var-count (heap-ref (+ 3 (heap-ref (+ loc 1)))))
        (define vars (build-vector var-count
                                   (lambda (i)
                                     (heap-ref (+ loc 2 i)))))
        (define new-addr (copy/alloc (+ 2 var-count) vars '()))
        (heap-set! new-addr 'struct-instance)
        (heap-set! (+ new-addr 1) (track/loc (heap-ref (+ loc 1))))
        (for ([x (in-range var-count)])
             (heap-set! (+ new-addr 2 x) (track/loc (heap-ref (+ loc 2 x)))))
        (heap-set! loc 'frwd)
        (heap-set! (+ loc 1) new-addr)
        new-addr]
       [(frwd) (heap-ref (+ loc 1))]
       [else (error 'forward/loc "wrong tag ~s at ~a" (heap-ref loc) loc)])]
    [else loc]))

;; forward/ref : loc -> loc
;; move the referenced object to the other semi-space
;; and return the new addr of moved object
(define (forward/ref loc)
  (unless (2nd-gen? loc)
    (error 'forward/loc "wrong location ~s" loc))
  (case (heap-ref loc)
    [(flat grey-flat white-flat) (void)]
    [(pair grey-pair white-pair) 
     (gc:set-first! loc (forward/loc (heap-ref (+ loc 1))))
     (gc:set-rest! loc (forward/loc (heap-ref (+ loc 2))))
     (forward/ref (heap-ref (+ loc 1)))
     (forward/ref (heap-ref (+ loc 2)))]
    [(proc grey-proc white-proc) 
     (define fv-count (heap-ref (+ loc 2)))
     (for ([x (in-range 0 fv-count)])
          (define l (+ loc 3 x))
          (heap-set! l (forward/loc (heap-ref l)))
          (forward/ref (heap-ref l)))]
    [(vector grey-vector white-vector) 
     (define var-count (heap-ref (+ loc 1)))
     (for ([x (in-range var-count)])
          (define l (+ loc 2 x))
          (heap-set! l (forward/loc (heap-ref l)))
          (forward/ref (heap-ref l)))]
    [(struct grey-struct white-struct) 
     (define parent (heap-ref (+ loc 2)))
     (when parent
       (heap-set! (+ loc 2) (forward/loc parent))
       (forward/ref (heap-ref (+ loc 2))))]
    [(struct-instance grey-struct-instance white-struct-instance) 
     (define fields-count (heap-ref (+ 3 (heap-ref (+ loc 1)))))
     (heap-set! (+ loc 1) (forward/loc (heap-ref (+ loc 1))))
     (forward/ref (heap-ref (+ loc 1)))
     (for ([x (in-range fields-count)])
          (define l (+ loc 2 x))
          (heap-set! l (forward/loc (heap-ref l)))
          (forward/ref (heap-ref l)))]
    [else (error 'forward/ref "wrong tag at ~a" loc)]))

(define (forward/pointers loc)
  (cond
    [(or (= loc 2nd-gen-start)
         (equal? 'free (heap-ref loc)))
     (heap-set! table-start-word (add1 table-start-word))]
    [else
     (define new-addr (forward/loc (heap-ref (+ loc 1))))
     (heap-set! (heap-ref loc) new-addr)
     (forward/ref new-addr)
     (heap-set! loc 'free)
     (heap-set! (+ loc 1) 'free)
     (forward/pointers (+ loc 2))]))

(define (free-1st-gen)
  (for ([i (in-range 1 1st-gen-size)])
    (heap-set! i 'free)))

(define (1st-gen? loc)
  (and (>= loc 1)
       (< loc 1st-gen-size)))

(define (2nd-gen? loc)
  (and (>= loc 2nd-gen-alloc-start)
       (< loc 2nd-gen-size)))

;; find-free-space : find free space by traversing free space list
;; layout := free-2 next
;;        |  free-n next size
;; next := (or/c location? #f)
;; free-list-head : holds the value of head of free space list
;;                | #f means already runs out of free space
;;
;; before give a series of free spaces to alloc function
;; must update the free space list
;; iff its the first free space in list must also udpate free-list-head as well
(define (find-free-space start prev size)
  (local [(define (next-in-free-list loc)
            (heap-ref (+ loc 1)))
          (define (update-next-in-prev prev loc)
            (heap-set! (if prev
                              (+ prev 1)
                              free-list-head)
                          loc))]
    (cond
      [(not start) #f]
      [else
       (case (heap-ref start)
         [(free-2)
          (cond
            [(= size 2)
             (update-next-in-prev prev (next-in-free-list start))
             start]
            [else (find-free-space (heap-ref (+ start 1)) start size)])]
         [(free-n)
          (define length (heap-ref (+ start 2)))
          (cond
            [(= size length)
             (update-next-in-prev prev (next-in-free-list start))
             start]
            [(< size length)
             (define new-free (+ start size))
             (define new-size (- length size))
             (cond
               [(= new-size 1)
                (update-next-in-prev prev (next-in-free-list start))
                (heap-set! new-free 'free)
                start]
               [else
                (update-next-in-prev prev new-free)
                (cond
                  [(= new-size 2)
                   (heap-set! new-free 'free-2)
                   (heap-set! (+ new-free 1) (heap-ref (+ start 1)))]
                  [else (heap-set! new-free 'free-n)
                        (heap-set! (+ new-free 1) (heap-ref (+ start 1)))
                        (heap-set! (+ new-free 2) new-size)])
                start])]
            [else (find-free-space (heap-ref (+ start 1)) start size)])]
         [else (error 'find-free-space "wrong tag @ ~s" start)])])))

(define (make-pointers-to-2nd-gen-roots start)
  (cond
    [(= start 1st-gen-size) (void)]
    [else
     (case (heap-ref start)
       [(flat) (make-pointers-to-2nd-gen-roots (+ start 2))]
       [(pair) (define one-loc (heap-ref (+ start 1)))
               (when (2nd-gen? one-loc) (trace/roots-iff-white one-loc))
               (define another-loc (heap-ref (+ start 2)))
               (when (2nd-gen? another-loc) (trace/roots-iff-white another-loc))
               (make-pointers-to-2nd-gen-roots (+ start 3))]
       [(proc) (define fv-counts (heap-ref (+ start 2)))
               (for ([i (in-range fv-counts)])
                 (define loc (heap-ref (+ start 3 i)))
                 (when (2nd-gen? loc) (trace/roots-iff-white loc)))
               (make-pointers-to-2nd-gen-roots (+ start 3 fv-counts))]
       [(vector) (define element-count (heap-ref (+ start 1)))
                 (for ([i (in-range element-count)])
                   (define loc (heap-ref (+ start 2 i)))
                   (when (2nd-gen? loc) (trace/roots-iff-white loc)))
                 (make-pointers-to-2nd-gen-roots (+ start 2 element-count))]
       [(struct) (define parent (heap-ref (+ start 2)))
                 (when (and parent (2nd-gen? parent))
                   (trace/roots-iff-white parent))
                 (make-pointers-to-2nd-gen-roots (+ start 4))]
       [(struct-instance) 
        (define fields-count (heap-ref (+ 3 (heap-ref (+ start 1)))))
        (define struct-loc (heap-ref (+ start 1)))
        (when (2nd-gen? struct-loc)
          (trace/roots-iff-white struct-loc))
        (for ([i (in-range fields-count)])
             (define loc (heap-ref (+ start 2 i)))
             (when (2nd-gen? loc) (trace/roots-iff-white loc)))
        (make-pointers-to-2nd-gen-roots (+ start 2 fields-count))]
       [(frwd) (define loc (heap-ref (+ start 1)))
               (traverse/roots loc)
               (case (heap-ref loc)
                 [(flat) (make-pointers-to-2nd-gen-roots (+ start 2))]
                 [(pair) (make-pointers-to-2nd-gen-roots (+ start 3))]
                 [(proc) (make-pointers-to-2nd-gen-roots (+ start 3 (heap-ref (+ loc 2))))]
                 [(vector) (make-pointers-to-2nd-gen-roots (+ start 2 (heap-ref (+ loc 1))))]
                 [(struct) (make-pointers-to-2nd-gen-roots (+ start 4))]
                 [(struct-instance) (make-pointers-to-2nd-gen-roots (+ start 2 (heap-ref (+ 3 (heap-ref (+ loc 1))))))])]
       [(free) (void)]
       [else (error 'make-pointers-to-2nd-gen-roots "wrong tag at ~a" start)])]))

;; check if any spot within free slots are taken
;; in order to detect heap-stack crash
(define (check/free loc size)
  (case (heap-ref loc)
    [(free-2) #t]
    [(free-n) (stream-andmap (lambda (x)
                               (equal? 'free (heap-ref (+ loc x))))
                             (in-range 3 size))]
    [else (error 'check/free "wrong tag @ ~s" loc)]))

(define (copy/alloc n some-roots more-roots)
  (define next (find-free-space (heap-ref free-list-head) #f n))
  (cond
    [next
      (unless (check/free next n)
        (error 'copy/alloc "collection crashed because old heap hit tracing stack @ ~s" next))

      ;; incremental collection
      (heap-set! step-count-word (+ n (heap-ref step-count-word)))
      (when (>= (heap-ref step-count-word) step-length)
        (traverse/incre-mark (next/cont)))

      next]
#|
      (let ([loc (find-free-space (heap-ref free-list-head) #f n)])
        (if loc
          loc
          (error 'copy/alloc "old generation gc failed")))]
|#
    [else (error 'copy/alloc "old generation gc failed")]))

(define (trace/roots-iff-white thing)
  (cond
    [(list? thing)
     (for-each trace/roots-iff-white thing)]
    [(root? thing)
     (define loc (read-root thing))
     (when (white? loc)
       (push/cont loc))]
    [(number? thing)
     (when (white? thing)
       (push/cont thing))]))

;; object-length : location -> number
(define (object-length loc)
  (define tag (heap-ref loc))
  (case tag
    [(free) 1]
    [(free-2) 2]
    [(free-n) (heap-ref (+ loc 2))]
    [(flat white-flat) 2]
    [(pair white-pair) 3]
    [(proc white-proc) (+ 3 (heap-ref (+ loc 2)))]
    [(vector white-vector) (+ 2 (heap-ref (+ loc 1)))]
    [(struct white-struct) 4]
    [(struct-instance white-struct-instance) (+ 2 (heap-ref (+ 3 (heap-ref (+ loc 1)))))]
    [else (error 'object-length "wrong tag ~s @ ~s" tag loc)]))

(define (mark-white! i)
  (case (heap-ref i)
    [(flat) (heap-set! i 'white-flat)]
    [(pair) (heap-set! i 'white-pair)]
    [(proc) (heap-set! i 'white-proc)]
    [(vector) (heap-set! i 'white-vector)]
    [(struct) (heap-set! i 'white-struct)]
    [(struct-instance) (heap-set! i 'white-struct-instance)]
    [else (error 'mark-white! "wrong tag at ~a" i)]))

;; for free-n spaces, free its rest slots besides the first three
;; free-rest : location -> void
(define (free-rest loc)
  (unless (equal? 'free-n (heap-ref loc))
    (error 'free-rest "wrong tag @ ~s" loc))

  (for ([i (in-range 3 (heap-ref (+ loc 2)))])
       #:break (equal? 'free (heap-ref (+ loc i)))
       (heap-set! (+ loc i) 'free)))

(define (free/mark-white! loc prev last-start spaces-so-far)
  (unless (or (and last-start spaces-so-far)
              (not (or last-start spaces-so-far)))
    (error 'free/mark-white! 
           "cumulating info are incorrect, last-start: ~s, spaces-so-far: ~s" 
           last-start spaces-so-far))
  
  (cond
    [(>= loc 2nd-gen-size)
     (cond
       [(and last-start spaces-so-far)
        (cond
          [(= 1 spaces-so-far) (void)]
          [(= 2 spaces-so-far) (heap-set! last-start 'free-2)
                               (heap-set! (+ 1 last-start) #f)
                               (heap-set! (if prev (+ prev 1) free-list-head)
                                             last-start)]
          [else (heap-set! last-start 'free-n)
                (heap-set! (+ 1 last-start) #f)
                (heap-set! (+ 2 last-start) spaces-so-far)
                (heap-set! (if prev (+ prev 1) free-list-head)
                              last-start)
                (free-rest last-start)])]
       [else (void)])]
    [else
      (define tag (heap-ref loc))
      (case tag
        [(flat pair proc vector struct struct-instance)
         (mark-white! loc)
         
         (define length (object-length loc))
         (cond
           [(and last-start
                 spaces-so-far
                 (= 1 spaces-so-far))
            (free/mark-white! (+ loc length) prev #f #f)]
           [(and last-start 
                 spaces-so-far
                 (>= spaces-so-far 2))
            (cond
              [(= 2 spaces-so-far) (heap-set! last-start 'free-2)
                                   (heap-set! (+ last-start 1) #f)]
              [else (heap-set! last-start 'free-n)
                    (heap-set! (+ last-start 1) #f)
                    (heap-set! (+ last-start 2) spaces-so-far)])
            (if prev
                (heap-set! (+ prev 1) last-start)
                (heap-set! free-list-head last-start))
            (free/mark-white! (+ loc length) last-start #f #f)]
           [else (free/mark-white! (+ loc length) prev #f #f)])]
        [(white-flat white-pair white-proc white-vector white-struct white-struct-instance
         free free-2 free-n)
         (define length (object-length loc))
         (cond 
           [(and last-start spaces-so-far)
            (free/mark-white! (+ loc length) prev last-start (+ spaces-so-far length))]
           [else (free/mark-white! (+ loc length) prev loc length)])]
        [else (error 'free/mark-white! "wrong tag at ~a" loc)])]))

(define (traverse/incre-mark loc) ;; loc of cont
  (cond
    [(not loc) (heap-set! step-count-word 0)]
    [else (case (heap-ref loc)
            [(flat grey-flat) (mark-black loc)
                              (step/count 2)
                              (continue/incre-mark)]
            [(pair grey-pair) (mark-black loc)
                              (step/count 3)
                              (push/cont (heap-ref (+ loc 2)))
                              (push/cont (heap-ref (+ loc 1)))
                              (continue/incre-mark)]
            [(proc grey-proc) (mark-black loc)
                              (define closure-size (heap-ref (+ loc 2)))
                              (step/count (+ 3 closure-size))
                              (for ([i (in-range closure-size)])
                                   (push/cont (heap-ref (+ loc 3 i))))
                              (continue/incre-mark)]
            [(vector grey-vector) (mark-black loc)
                                  (define size (heap-ref (+ loc 1)))
                                  (step/count (+ 2 size))
                                  (for ([i (in-range size)])
                                       (push/cont (heap-ref (+ loc 2 i))))
                                  (continue/incre-mark)]
            [(struct grey-struct) (mark-black loc)
                                  (step/count 4)
                                  (define parent (heap-ref (+ loc 2)))
                                  (when parent (push/cont parent))
                                  (continue/incre-mark)]
            [(struct-instance grey-struct-instance) (mark-black loc)
                                                    (define fv-count (heap-ref (+ 3 (heap-ref (+ loc 1)))))
                                                    (step/count (+ 2 fv-count))
                                                    (push/cont (heap-ref (+ loc 1)))
                                                    (for ([i (in-range fv-count)])
                                                         (push/cont (heap-ref (+ loc 2 i))))
                                                    (continue/incre-mark)]
            [else (error 'traverse/incre-mark "wrong tag @ ~s" loc)])]))

(define (mark-black loc)
  (case (heap-ref loc)
    [(grey-flat)
     (heap-set! loc 'flat)]
    [(grey-pair)
     (mark-grey (heap-ref (+ loc 1)))
     (mark-grey (heap-ref (+ loc 2)))
     (heap-set! loc 'pair)]
    [(grey-proc)
     (for ([i (in-range (heap-ref (+ loc 2)))])
          (mark-grey (heap-ref (+ loc 3 i))))
     (heap-set! loc 'proc)]
    [(grey-vector)
     (for ([i (in-range (heap-ref (+ loc 1)))])
          (mark-grey (heap-ref (+ loc 2 i))))
     (heap-set! loc 'vector)]
    [(grey-struct)
     (define parent (heap-ref (+ loc 2)))
     (when parent (mark-grey parent))
     (heap-set! loc 'struct)]
    [(grey-struct-instance)
     (define fv-count (heap-ref (+ 3 (heap-ref (+ loc 1)))))
     (mark-grey (heap-ref (+ loc 1)))
     (for ([i (in-range fv-count)])
          (mark-grey (heap-ref (+ loc 2 i))))
     (heap-set! loc 'struct-instance)]
    [(pair flat proc struct struct-instance white-pair white-flat white-proc white-struct white-struct-instance cont) (void)]))

(define (mark-grey loc)
  (case (heap-ref loc)
    [(white-pair) (heap-set! loc 'grey-pair)]
    [(white-flat) (heap-set! loc 'grey-flat)]
    [(white-proc) (heap-set! loc 'grey-proc)]
    [(white-vector) (heap-set! loc 'grey-vector)]
    [(white-struct) (heap-set! loc 'grey-struct)]
    [(white-struct-instance) (heap-set! loc 'grey-struct-instance)]
    [(pair flat proc struct struct-instance grey-pair grey-flat grey-proc grey-struct grey-struct-instance cont) (void)]))

(define (heap-cont/check)
  (check/tag 2nd-gen-alloc-start)
  (heap-check 2nd-gen-alloc-start))

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
      [else 
        (cond
          [(location? loc) (check/tag-helper loc)]
          [else (error 'check "unknown tag @ ~a" loc)])])))

(define (check/tag loc)
  (when (< loc 2nd-gen-size)
    (case (heap-ref loc)
      [(pair white-pair grey-pair) (check/tag (+ loc 3))]
      [(flat white-flat grey-flat) (check/tag (+ loc 2))]
      [(proc white-proc grey-proc) 
       (define closure-size (heap-ref (+ loc 2)))
       (check/tag (+ loc 3 closure-size))]
      [(vector white-vector grey-vector)
       (define size (heap-ref (+ loc 1)))
       (check/tag (+ loc 2 size))]
      [(struct white-struct grey-struct) (check/tag (+ loc 4))]
      [(struct-instance white-struct-instance grey-struct-instance)
       (define fields-num (heap-ref (+ 3 (heap-ref (+ loc 1)))))
       (check/tag (+ loc 2 fields-num))]
      [(free) (check/tag (+ loc 1))]
      [else 
        (cond
          [(location? loc) (check/tag-helper loc)]
          [else (error 'check/tag "wrong tag at ~a" loc)])])))

(define (check/tag-helper loc)
  (for ([x (in-range loc 2nd-gen-size)])
       (unless (location? (heap-ref x))
         (error 'check/tag-helper "wrong value @ ~s, should be location" x))))

;; white? : location? -> boolean?
(define (white? loc)
  (case (heap-ref loc)
    [(white-pair white-proc white-flat white-vector white-struct white-struct-instance)
     true]
    [else false]))

(define (next/cont)
  (define loc (heap-ref tracing-head-word))
  (cond
    [(equal? loc #f) #f]
    [else
      (define ptr (heap-ref loc))
      (clean/cont loc)
      ptr]))

(define (push/cont ptr)
  (mark-grey ptr)
  (case (heap-ref ptr)
    [(pair flat proc vector strut struct-instance) (void)]
    [else
      (define loc (heap-ref tracing-head-word))
      (define next (if loc 
                       (- loc 1) 
                       #f))
      (when (and next 
                 (not (equal? 'free (heap-ref next))))
        (error 'push/cont "collection crashed @ ~s" next))

      (cond
        [(not next) (define stack-start (- 2nd-gen-size 1))
                    (heap-set! stack-start ptr)
                    (heap-set! tracing-head-word stack-start)]
        [else (heap-set! next ptr)
              (heap-set! tracing-head-word next)])]))

(define (continue/incre-mark)
  (if (step/finished?)
    (heap-set! step-count-word 0)
    (traverse/incre-mark (next/cont))))

(define (clean/cont loc)
  (define next (+ loc 1))
  (when (> next 2nd-gen-size)
    (error 'clean/cont "stack is out of bound"))

  (cond
    [(= next 2nd-gen-size) (heap-set! tracing-head-word #f)]
    [else (heap-set! tracing-head-word (+ loc 1))])
  (heap-set! loc 'free))

(define (step/count n)
  (heap-set! step-count-word (- (heap-ref step-count-word) n)))

(define (step/finished?)
  (<= (heap-ref step-count-word) 0))
