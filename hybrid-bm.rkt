#lang plai/gc2/collector

;; config for collection
(define step-length 10)
(define 1st-gen-size "size of young generation")
(define 2nd-gen-size "size of old generation")
(define 2nd-gen-start "start position to allocate objects in old generation")
(define status-word "current phase of old generation incremental gc")
(define volume-word "current total size of objects in old generation")
(define step-count-word "work count for each tracing round")
(define tracing-head-word "head of tracing tree")
(define table-start-word "start position of table for cross-generation pointers")

;; recording vars for benchmarking
(define peak-heap-size 0)
(define heap-size-check-time 0)
(define total-heap-size 0)
(define peak-heap-operations 0)
(define current-heap-operations 0)
(define heap-operation-check-time 0)
(define total-heap-operations 0)

(define (init-allocator)
  (for ([i (in-range 0 (heap-size))])
    (heap-set!/bm i 'free))
  (set! 1st-gen-size (round (* (heap-size) 1/4)))
  (set! 2nd-gen-size (round (* (heap-size) 7/8)))
  (set! 2nd-gen-start (+ 4 1st-gen-size))
  (set! status-word 1st-gen-size)
  (set! volume-word (+ 1 1st-gen-size))
  (set! step-count-word (+ 2 1st-gen-size))
  (set! tracing-head-word (+ 3 1st-gen-size))
  (set! table-start-word 2nd-gen-size)
  (heap-set!/bm status-word 'out)
  (heap-set!/bm volume-word 0)
  (heap-set!/bm step-count-word 0)
  (heap-set!/bm tracing-head-word 0)
  (heap-set!/bm table-start-word (+ table-start-word 1)))

;; gc:deref : loc -> heap-value
;; must signal an error if fl-loc doesn't point to a flat value
(define (gc:deref fl-loc)
  (cond
    [(gc:flat? fl-loc) (heap-ref/bm (+ fl-loc 1))]
    [else (error 'gc:deref "non-flat @ ~s" fl-loc)]))

;; track/loc : loc -> loc
;; if loc points to a flat or pair or proc, then return loc
;; else if loc points to a frwd, return the frwd address
(define (track/loc loc)
  (case (heap-ref/bm loc)
    [(free) (error 'track/loc "wrong tag ~s @ ~a" (heap-ref/bm loc) loc)]
    [(frwd) (heap-ref/bm (+ loc 1))]
    [else loc]))

;; gc:alloc-flat : heap-value -> loc
(define (gc:alloc-flat fv)
  (define ptr (alloc 2 #f #f))
  (heap-set!/bm ptr 'flat)
  (heap-set!/bm (+ ptr 1) fv)
  ptr)

;; gc:cons : loc loc -> loc
;; hd and tl are guaranteed to have been earlier
;; results from either gc:alloc-flat or gc:cons
(define (gc:cons hd tl)
  (define ptr (alloc 3 hd tl))
  (define head (track/loc hd))
  (define tail (track/loc tl))
  (when (and (= ptr 0)
             (or (need-forwarding-pointers? hd)
                 (need-forwarding-pointers? tl)))
    (free-1st-gen))
  (heap-set!/bm ptr 'pair)
  (heap-set!/bm (+ ptr 1) head)
  (heap-set!/bm (+ ptr 2) tail)
  ptr)

;; gc:first : loc -> loc
;; must signal an error of pr-loc does not point to a pair
(define (gc:first pr-loc)
  (if (equal? (heap-ref/bm pr-loc) 'pair)
      (heap-ref/bm (+ (track/loc pr-loc) 1))
      (error 'first "non pair @ ~s" pr-loc)))

;; gc:rest : loc -> loc
;; must signal an error of pr-loc does not point to a pair
(define (gc:rest pr-loc)
  (if (equal? (heap-ref/bm pr-loc) 'pair)
      (heap-ref/bm (+ (track/loc pr-loc) 2))
      (error 'rest "non pair @ ~s" pr-loc)))

;; gc:flat? : loc -> boolean
;; loc is guaranteed to have been an earlier
;; result from either gc:alloc-flat or gc:cons
(define (gc:flat? loc)
  (case (heap-ref/bm loc)
    [(flat grey-flat white-flat) #t]
    [(frwd) (gc:flat? (heap-ref/bm (+ loc 1)))]
    [else #f]))

;; gc:cons? : loc -> boolean
;; loc is guaranteed to have been an earlier
;; result from either gc:alloc-flat or gc:cons
(define (gc:cons? loc)
  (case (heap-ref/bm loc)
    [(pair grey-pair white-pair) #t]
    [(frwd) (gc:cons? (heap-ref/bm (+ loc 1)))]
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
  (when (and (= next 0)
             (need-forwarding-pointers? fv-vars))
    (free-1st-gen))
  (heap-set!/bm next 'proc)
  (heap-set!/bm (+ next 1) code-ptr)
  (heap-set!/bm (+ next 2) fv-count)
  (for ([x (in-range 0 fv-count)])
    (heap-set!/bm (+ next 3 x)
               (vector-ref updated-free-vars x)))
  next)

;; gc:closure-code-ptr : loc -> heap-value
;; given a location returned from an earlier allocation
;; check to see if it is a closure; if not signal an
;; error. if so, return the code-ptr
(define (gc:closure-code-ptr loc)
  (if (gc:closure? loc)
      (heap-ref/bm (+ (track/loc loc) 1))
      (error 'gc:closure-code-ptr "non closure at ~a" loc)))

;; gc:closure-env-ref : loc number -> loc
;; given a location returned from an earlier allocation
;; check to see if it is a closure; if not signal an
;; error. if so, return the 'i'th variable in the closure
(define (gc:closure-env-ref loc i)
  (if (gc:closure? loc)
      (heap-ref/bm (+ (track/loc loc) 3 i))
      (error 'gc:closure-env-ref "non closure at ~a" loc)))

;; gc:closure? : loc -> boolean
;; determine if a previously allocated location 
;; holds a closure
(define (gc:closure? loc)
  (case (heap-ref/bm loc)
    [(proc grey-proc white-proc) #t]
    [(frwd) (gc:closure? (heap-ref/bm (+ loc 1)))]
    [else #f]))

;; vector related
(define (gc:vector length loc)
  (define next (alloc (+ length 2) loc #f))
  (define l (track/loc loc))
  (when (and (= next 0)
             (need-forwarding-pointers? loc))
    (free-1st-gen))
  (heap-set!/bm next 'vector)
  (heap-set!/bm (+ next 1) length)
  (for ([i (in-range length)])
    (heap-set!/bm (+ next 2 i) l))
  next)

(define (gc:vector? loc)
  (case (heap-ref/bm loc)
    [(vector grey-vector white-vector) #t]
    [(frwd) (gc:vector? (heap-ref/bm (+ loc 1)))]
    [else #f]))

(define (gc:vector-length loc)
  (if (gc:vector? loc)
      (heap-ref/bm (+ (track/loc loc) 1))
      (error 'gc:vector-length "non vector @ ~s" loc)))

(define (gc:vector-ref loc number)
  (unless (gc:vector? loc)
    (error 'gc:vector-ref "non vector @ ~s" loc))

  (define v-loc (track/loc loc))
  (cond
    [(< number (gc:vector-length loc)) (heap-ref/bm (+ v-loc 2 number))]
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
     (heap-set!/bm (+ v-loc 2 number) thing)
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
  (when (and (= next 0)
             (need-forwarding-pointers? parent))
    (free-1st-gen))
  (heap-set!/bm next 'struct)
  (heap-set!/bm (+ next 1) name)
  (heap-set!/bm (+ next 2) p)
  (heap-set!/bm (+ next 3) fields-count)
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
  (heap-set!/bm (+ next 1) ss)
  (for ([x (in-range 0 fv-count)])
    (heap-set!/bm (+ next 2 x)
               (vector-ref fields-value x)))
  (heap-set!/bm next 'struct-instance)
  next)

(define (struct? loc)
  (case (heap-ref/bm loc)
    [(struct grey-struct white-struct) #t]
    [(frwd) (struct? (heap-ref/bm (+ loc 1)))]
    [else #f]))

(define (gc:struct-pred s instance)
  (and (struct? s)
       (gc:struct-pred-helper s (heap-ref/bm (+ instance 1)))))

(define (gc:struct-pred-helper target s)
  (and s
       (or (= target s)
           (gc:struct-pred-helper target (heap-ref/bm (+ s 2))))))

(define (gc:struct-select s instance index)
  (unless (gc:struct-pred s instance)
    (error 'gc:struct-select "value at ~a is not an instance of ~a" 
           instance
           (heap-ref/bm (+ 1 s))))
  (heap-ref/bm (+ instance 2 index)))

(define (table/alloc pointer target)
  (define next (heap-ref/bm table-start-word))
  (cond
    [(>= (+ next 2) (heap-size))
     (forward/pointers (+ 1 table-start-word))
     (heap-set!/bm (+ 1 table-start-word) pointer)
     (heap-set!/bm (+ 2 table-start-word) target)
     (heap-set!/bm table-start-word (+ 3 table-start-word))]
    [else
     (heap-set!/bm next pointer)
     (heap-set!/bm (+ next 1) target)
     (heap-set!/bm table-start-word (+ next 2))]))

;; alloc : number[size] roots roots -> loc
(define (alloc n some-roots more-roots)
  (define addr (find-free-space 0 1st-gen-size n))
  (cond 
    [addr addr]
    [else
     (collect-garbage some-roots more-roots)
     (unless (or (need-forwarding-pointers? some-roots)
                 (need-forwarding-pointers? more-roots))
       (free-1st-gen))
     (unless (<= (+ 0 n) 1st-gen-size)
       (error 'alloc "object is larget than young generation"))
     0]))

(define (need-forwarding-pointers? thing)
  (cond
    [(list? thing) (ormap 1st-gen? thing)]
    [(root? thing) (1st-gen? (read-root thing))]
    [(number? thing) (1st-gen? thing)]
    [else thing]))

;; collect-garbage : roots roots -> void
(define (collect-garbage some-roots more-roots)

  ;; preparation for heap operations benchmarks
  (heap-set!/bm status-word 'in)
  (set! current-heap-operations 0)

  ;; young->old live objects copying
  (make-pointers-to-2nd-gen-roots 0)
  (traverse/roots (get-root-set))
  (traverse/roots some-roots)
  (traverse/roots more-roots)
  (forward/pointers (+ 1 table-start-word))
  ;; only free/mark-white! when tree traversal is done
  (when (= 0 (heap-ref/bm tracing-head-word))
    (free/mark-white! 2nd-gen-start))
  ;; ensure heap operations are only recorded during collection phase
  (heap-set!/bm status-word 'out)

  ;; metrics recording and print-out
  (set! heap-size-check-time (add1 heap-size-check-time))
  (set! total-heap-size (+ (heap-ref volume-word) total-heap-size))
  (printf "peak heap size is ~s%, average heap size is ~s%\n"
          (round (/ (* 100 peak-heap-size) (- 2nd-gen-size 1st-gen-size)))
          (round (/ (* 100 (/ total-heap-size heap-size-check-time)) 
                    (- 2nd-gen-size 1st-gen-size))))
  (set! heap-operation-check-time (add1 heap-operation-check-time))
  (when (> current-heap-operations peak-heap-operations)
    (set! peak-heap-operations current-heap-operations))
  (set! total-heap-operations (+ current-heap-operations total-heap-operations))
  (printf "heap operations: peak ~s, average ~s, total ~s\n"
          peak-heap-operations
          (round (/ total-heap-operations heap-operation-check-time))
          total-heap-operations))

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
     (case (heap-ref/bm loc)
       [(flat) 
        (define new-addr (copy/alloc 2 #f #f))
        (heap-set!/bm new-addr 'flat)
        (heap-set!/bm (+ new-addr 1) (heap-ref/bm (+ loc 1))) 
        (heap-set!/bm loc 'frwd)
        (heap-set!/bm (+ loc 1) new-addr)
        new-addr]
       [(pair) 
        (define new-addr (copy/alloc 3
                                     (heap-ref/bm (+ loc 1))
                                     (heap-ref/bm (+ loc 2))))
        (heap-set!/bm new-addr 'pair)
        (heap-set!/bm (+ new-addr 1) (track/loc (heap-ref/bm (+ loc 1))))
        (heap-set!/bm (+ new-addr 2) (track/loc (heap-ref/bm (+ loc 2))))
        (heap-set!/bm loc 'frwd)
        (heap-set!/bm (+ loc 1) new-addr)
        new-addr]
       [(proc) 
        (define length (+ 3 (heap-ref/bm (+ loc 2))))
        (define free-vars (build-vector (- length 3)
                                        (lambda (i)
                                          (heap-ref/bm (+ loc 3 i)))))
        (define new-addr (copy/alloc length free-vars '()))
        (heap-set!/bm new-addr 'proc)
        (heap-set!/bm (+ 1 new-addr) (heap-ref/bm (+ 1 loc)))
        (heap-set!/bm (+ 2 new-addr) (heap-ref/bm (+ 2 loc)))
        (for ([x (in-range 3 length)])
             (heap-set!/bm (+ new-addr x) (track/loc (heap-ref/bm (+ loc x)))))
        (heap-set!/bm loc 'frwd)
        (heap-set!/bm (+ loc 1) new-addr)
        new-addr]
       [(vector) 
        (define var-count (heap-ref/bm (+ loc 1)))
        (define vars (build-vector var-count
                                   (lambda (i)
                                     (heap-ref/bm (+ loc 2 i)))))
        (define new-addr (copy/alloc (+ 2 var-count) vars '()))
        (heap-set!/bm new-addr 'vector)
        (heap-set!/bm (+ 1 new-addr) (heap-ref/bm (+ loc 1)))
        (for ([x (in-range var-count)])
             (heap-set!/bm (+ new-addr 2 x) (track/loc (heap-ref/bm (+ loc 2 x)))))
        (heap-set!/bm loc 'frwd)
        (heap-set!/bm (+ loc 1) new-addr)
        new-addr]
       [(struct) 
        (define new-addr (copy/alloc 4 (heap-ref/bm (+ loc 2)) #f))
        (heap-set!/bm new-addr 'struct)
        (heap-set!/bm (+ new-addr 1) (heap-ref/bm (+ loc 1)))
        (define parent (heap-ref/bm (+ loc 2)))
        (heap-set!/bm (+ new-addr 2) (and parent (track/loc parent)))
        (heap-set!/bm (+ new-addr 3) (heap-ref/bm (+ loc 3)))
        (heap-set!/bm loc 'frwd)
        (heap-set!/bm (+ loc 1) new-addr)
        new-addr]
       [(struct-instance) 
        (define var-count (heap-ref/bm (+ 3 (heap-ref/bm (+ loc 1)))))
        (define vars (build-vector var-count
                                   (lambda (i)
                                     (heap-ref/bm (+ loc 2 i)))))
        (define new-addr (copy/alloc (+ 2 var-count) vars '()))
        (heap-set!/bm new-addr 'struct-instance)
        (heap-set!/bm (+ new-addr 1) (track/loc (heap-ref/bm (+ loc 1))))
        (for ([x (in-range var-count)])
             (heap-set!/bm (+ new-addr 2 x) (track/loc (heap-ref/bm (+ loc 2 x)))))
        (heap-set!/bm loc 'frwd)
        (heap-set!/bm (+ loc 1) new-addr)
        new-addr]
       [(frwd) (heap-ref/bm (+ loc 1))]
       [else (error 'forward/loc "wrong tag ~s at ~a" (heap-ref/bm loc) loc)])]
    [else loc]))

;; forward/ref : loc -> loc
;; move the referenced object to the other semi-space
;; and return the new addr of moved object
(define (forward/ref loc)
  (unless (2nd-gen? loc)
    (error 'forward/loc "wrong location ~s" loc))
  (case (heap-ref/bm loc)
    [(flat grey-flat white-flat) (void)]
    [(pair grey-pair white-pair) 
     (gc:set-first! loc (forward/loc (heap-ref/bm (+ loc 1))))
     (gc:set-rest! loc (forward/loc (heap-ref/bm (+ loc 2))))
     (forward/ref (heap-ref/bm (+ loc 1)))
     (forward/ref (heap-ref/bm (+ loc 2)))]
    [(proc grey-proc white-proc) 
     (define fv-count (heap-ref/bm (+ loc 2)))
     (for ([x (in-range 0 fv-count)])
          (define l (+ loc 3 x))
          (heap-set!/bm l (forward/loc (heap-ref/bm l)))
          (forward/ref (heap-ref/bm l)))]
    [(vector grey-vector white-vector) 
     (define var-count (heap-ref/bm (+ loc 1)))
     (for ([x (in-range var-count)])
          (define l (+ loc 2 x))
          (heap-set!/bm l (forward/loc (heap-ref/bm l)))
          (forward/ref (heap-ref/bm l)))]
    [(struct grey-struct white-struct) 
     (define parent (heap-ref/bm (+ loc 2)))
     (when parent
       (heap-set!/bm (+ loc 2) (forward/loc parent))
       (forward/ref (heap-ref/bm (+ loc 2))))]
    [(struct-instance grey-struct-instance white-struct-instance) 
     (define fields-count (heap-ref/bm (+ 3 (heap-ref/bm (+ loc 1)))))
     (heap-set!/bm (+ loc 1) (forward/loc (heap-ref/bm (+ loc 1))))
     (forward/ref (heap-ref/bm (+ loc 1)))
     (for ([x (in-range fields-count)])
          (define l (+ loc 2 x))
          (heap-set!/bm l (forward/loc (heap-ref/bm l)))
          (forward/ref (heap-ref/bm l)))]
    [else (error 'forward/ref "wrong tag at ~a" loc)]))

(define (forward/pointers loc)
  (cond
    [(= loc (heap-size)) (void)]
    [(equal? 'free (heap-ref/bm loc)) (void)]
    [else
     (define new-addr (forward/loc (heap-ref/bm (+ loc 1))))
     (heap-set!/bm (heap-ref/bm loc) new-addr)
     (forward/ref new-addr)
     (heap-set!/bm loc 'free)
     (heap-set!/bm (+ loc 1) 'free)
     (forward/pointers (+ loc 2))]))

(define (free-1st-gen)
  (for ([i (in-range 0 1st-gen-size)])
    (heap-set!/bm i 'free)))

(define (1st-gen? loc)
  (and (>= loc 0)
       (< loc 1st-gen-size)))

(define (2nd-gen? loc)
  (and (>= loc 1st-gen-size)
       (< loc 2nd-gen-size)))

(define (heap-ref/bm loc)
  (case (heap-ref status-word)
    [(in)
     (set! current-heap-operations (add1 current-heap-operations))
     (heap-ref loc)]
    [else (heap-ref loc)]))

(define (heap-set!/bm loc thing)
  (case (heap-ref/bm status-word)
    [(in)
     (set! current-heap-operations (add1 current-heap-operations))
     (heap-set! loc thing)]
    [else (heap-set! loc thing)]))

(define (find-free-space start end size)
  (cond
    [(= start end) #f]
    [else
     (case (heap-ref/bm start)
       [(free) (if (n-free-blocks? start end size)
                   start
                   (find-free-space (+ start 1) end size))]
       [(flat grey-flat white-flat) (find-free-space (+ start 2) end size)]
       [(pair grey-pair white-pair cont) (find-free-space (+ start 3) end size)]
       [(proc grey-proc white-proc)
        (find-free-space
         (+ start 3 (heap-ref/bm (+ start 2)))
         end
         size)]
       [(vector grey-vector white-vector)
        (find-free-space
          (+ start 2 (heap-ref/bm (+ start 1)))
          end
          size)]
       [(struct grey-struct white-struct) (find-free-space (+ start 4) end size)]
       [(struct-instance grey-struct-instance white-struct-instance)
        (find-free-space
          (+ start 2 (heap-ref/bm (+ 3 (heap-ref/bm (+ start 1)))))
          end
          size)]
       [(frwd)
        (case (heap-ref/bm (+ 1 start))
          [(flat grey-flat white-flat) (find-free-space (+ start 2) end size)]
          [(pair grey-pair white-pair) (find-free-space (+ start 3) end size)]
          [(proc grey-proc white-proc) 
           (find-free-space (+ start 3 (heap-ref/bm (+ (heap-ref/bm (+ start 1)) 2)))
                            end
                            size)]
          [(vector grey-vector white-vector)
           (find-free-space (+ start 2 (heap-ref/bm (+ (heap-ref/bm (+ 1 start)) 1)))
                            end
                            size)]
          [(struct grey-struct white-struct) (find-free-space (+ start 4) end size)]
          [(struct-instance grey-struct-instance white-struct-instance)
           (find-free-space (+ start 2 (heap-ref/bm (+ 3 (heap-ref/bm (+ (heap-ref/bm (+ 1 start)) 1)))))
                            end
                            size)]
          [else (error 'find-free-space "wrong fowarded tag at ~a" (heap-ref/bm (+ 1 start)))])]
       [else (error 'find-free-space "wrong tag at ~a" start)])]))

(define (n-free-blocks? start end size)
  (cond
    [(= size 0) #t]
    [(= start end) #f]
    [else 
     (and (eq? 'free (heap-ref/bm start))
          (n-free-blocks? (+ start 1)
                          end
                          (- size 1)))]))

(define (make-pointers-to-2nd-gen-roots start)
  (cond
    [(= start 1st-gen-size) (void)]
    [else
     (case (heap-ref/bm start)
       [(flat) (make-pointers-to-2nd-gen-roots (+ start 2))]
       [(pair) (define one-loc (heap-ref/bm (+ start 1)))
               (when (2nd-gen? one-loc) (trace/roots-iff-white one-loc))
               (define another-loc (heap-ref/bm (+ start 2)))
               (when (2nd-gen? another-loc) (trace/roots-iff-white another-loc))
               (make-pointers-to-2nd-gen-roots (+ start 3))]
       [(proc) (define fv-counts (heap-ref/bm (+ start 2)))
               (for ([i (in-range fv-counts)])
                 (define loc (heap-ref/bm (+ start 3 i)))
                 (when (2nd-gen? loc) (trace/roots-iff-white loc)))
               (make-pointers-to-2nd-gen-roots (+ start 3 fv-counts))]
       [(vector) (define element-count (heap-ref/bm (+ start 1)))
                 (for ([i (in-range element-count)])
                   (define loc (heap-ref/bm (+ start 2 i)))
                   (when (2nd-gen? loc) (trace/roots-iff-white loc)))
                 (make-pointers-to-2nd-gen-roots (+ start 2 element-count))]
       [(struct) (define parent (heap-ref/bm (+ start 2)))
                 (when (and parent (2nd-gen? parent))
                   (trace/roots-iff-white parent))
                 (make-pointers-to-2nd-gen-roots (+ start 4))]
       [(struct-instance) 
        (define fields-count (heap-ref/bm (+ 3 (heap-ref/bm (+ start 1)))))
        (define struct-loc (heap-ref/bm (+ start 1)))
        (when (2nd-gen? struct-loc)
          (trace/roots-iff-white struct-loc))
        (for ([i (in-range fields-count)])
             (define loc (heap-ref/bm (+ start 2 i)))
             (when (2nd-gen? loc) (trace/roots-iff-white loc)))
        (make-pointers-to-2nd-gen-roots (+ start 2 fields-count))]
       [(frwd) (define loc (heap-ref/bm (+ start 1)))
               (case (heap-ref/bm loc)
                 [(flat) (make-pointers-to-2nd-gen-roots (+ start 2))]
                 [(pair) (make-pointers-to-2nd-gen-roots (+ start 3))]
                 [(proc) (make-pointers-to-2nd-gen-roots (+ start 3 (heap-ref/bm (+ loc 2))))]
                 [(vector) (make-pointers-to-2nd-gen-roots (+ start 2 (heap-ref/bm (+ loc 1))))]
                 [(struct) (make-pointers-to-2nd-gen-roots (+ start 4))]
                 [(struct-instance) (make-pointers-to-2nd-gen-roots (+ start 2 (heap-ref/bm (+ 3 (heap-ref/bm (+ loc 1))))))])]
       [(free) (void)]
       [else (error 'make-pointers-to-2nd-gen-roots "wrong tag at ~a" start)])]))

(define (copy/alloc n some-roots more-roots)
  (define next (find-free-space 2nd-gen-start 2nd-gen-size n))
  (cond
    [next
      ;; update peak heap size
      (heap-set! volume-word (+ n (heap-ref volume-word)))
      (when (> (heap-ref volume-word) peak-heap-size)
        (set! peak-heap-size (heap-ref volume-word)))

      ;; incremental collection
      (heap-set!/bm step-count-word (+ n (heap-ref/bm step-count-word)))
      (when (>= (heap-ref/bm step-count-word) step-length)
        (traverse/incre-mark (next/cont)))

      (let ([loc (find-free-space next 2nd-gen-size n)])
        (if loc
          loc
          (error 'copy/alloc "old generation gc failed")))]
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

(define (free/mark-white! i)
  (when (< i 2nd-gen-size)
    (case (heap-ref/bm i)
      [(pair) (heap-set!/bm i 'white-pair)
              (free/mark-white! (+ i 3))]
      [(flat) (heap-set!/bm i 'white-flat)
              (free/mark-white! (+ i 2))]
      [(proc) (heap-set!/bm i 'white-proc)
              (free/mark-white! (+ i 3 (heap-ref/bm (+ i 2))))]
      [(vector) (heap-set!/bm i 'white-vector)
                (free/mark-white! (+ i 2 (heap-ref/bm (+ i 1))))]
      [(struct) (heap-set!/bm i 'white-struct)
                (free/mark-white! (+ i 4))]
      [(struct-instance) (heap-set!/bm i 'white-struct-instance)
                         (free/mark-white! (+ i 2 (heap-ref/bm (+ 3 (heap-ref/bm (+ i 1))))))]
      [(white-pair) (heap-set!/bm i 'free)
                         (heap-set!/bm (+ i 1) 'free)
                         (heap-set!/bm (+ i 2) 'free)
                         (heap-set! volume-word (- (heap-ref volume-word) 3))
                         (free/mark-white! (+ i 3))]
      [(white-flat) (heap-set!/bm i 'free)
                    (heap-set!/bm (+ i 1) 'free)
                    (heap-set! volume-word (- (heap-ref volume-word) 2))
                    (free/mark-white! (+ i 2))]
      [(white-proc) (define closure-size (heap-ref/bm (+ i 2)))
                    (for ([dx (in-range 0 (+ closure-size 3))])
                      (heap-set!/bm (+ i dx) 'free))
                    (heap-set! volume-word (- (heap-ref volume-word) (+ 3 closure-size)))
                    (free/mark-white! (+ i 3 closure-size))]
      [(white-vector) (define size (heap-ref/bm (+ i 1)))
                      (for ([dx (in-range (+ size 2))])
                           (heap-set!/bm (+ i dx) 'free))
                      (heap-set! volume-word (- (heap-ref volume-word) (+ 2 size)))
                      (free/mark-white! (+ i 2 size))]
      [(white-struct) (heap-set!/bm i 'free)
                      (heap-set!/bm (+ i 1) 'free)
                      (heap-set!/bm (+ i 2) 'free)
                      (heap-set!/bm (+ i 3) 'free)
                      (heap-set! volume-word (- (heap-ref volume-word) 4))
                      (free/mark-white! (+ i 4))]
      [(white-struct-instance) (define fields-size (heap-ref/bm (+ 3 (heap-ref/bm (+ i 1)))))
                               (for ([dx (in-range 0 (+ 2 fields-size))])
                                    (heap-set!/bm (+ i dx) 'free))
                               (heap-set! volume-word (- (heap-ref volume-word) (+ 2 fields-size)))
                               (free/mark-white! (+ i 2 fields-size))]
      [(free) (free/mark-white! (+ i 1))]
      [else (error 'free-white! "unknown tag ~s" (heap-ref/bm i))])))

(define (traverse/incre-mark loc) ;; loc of cont
  (unless (or (equal? 'cont (heap-ref/bm loc))
              (= loc 0))
    (error 'traverse/incre-mark "not cont at ~a" loc))
  (cond
    [(= loc 0) (heap-set!/bm step-count-word 0)
               (heap-set!/bm tracing-head-word 0)]
    [else (define ptr (heap-ref/bm (+ loc 1)))
          (case (heap-ref/bm ptr)
            [(flat grey-flat) (mark-black ptr)
                              (step/count 2)
                              (clean/cont loc)
                              (continue/incre-mark)]
            [(pair grey-pair) (mark-black ptr)
                              (step/count 3)
                              (push/cont (heap-ref/bm (+ ptr 2)))
                              (push/cont (heap-ref/bm (+ ptr 1)))
                              (clean/cont loc)
                              (continue/incre-mark)]
            [(proc grey-proc) (mark-black ptr)
                              (define closure-size (heap-ref/bm (+ ptr 2)))
                              (step/count (+ 3 closure-size))
                              (for ([i (in-range closure-size)])
                                   (push/cont (heap-ref/bm (+ ptr 3 i))))
                              (clean/cont loc)
                              (continue/incre-mark)]
            [(vector grey-vector) (mark-black ptr)
                                  (define size (heap-ref/bm (+ ptr 1)))
                                  (step/count (+ 2 size))
                                  (for ([i (in-range size)])
                                       (push/cont (heap-ref/bm (+ ptr 2 i))))
                                  (clean/cont loc)
                                  (continue/incre-mark)]
            [(struct grey-struct) (mark-black ptr)
                                  (step/count 4)
                                  (define parent (heap-ref/bm (+ ptr 2)))
                                  (when parent (push/cont parent))
                                  (clean/cont loc)
                                  (continue/incre-mark)]
            [(struct-instance grey-struct-instance) (mark-black ptr)
                                                    (define fv-count (heap-ref/bm (+ 3 (heap-ref/bm (+ ptr 1)))))
                                                    (step/count (+ 2 fv-count))
                                                    (push/cont (heap-ref/bm (+ ptr 1)))
                                                    (for ([i (in-range fv-count)])
                                                         (push/cont (heap-ref/bm (+ ptr 2 i))))
                                                    (clean/cont loc)
                                                    (continue/incre-mark)]
            [else (error 'traverse/incre-mark "cont at ~a leads to a wrong tag at ~a" loc ptr)])]))

(define (mark-black loc)
  (case (heap-ref/bm loc)
    [(grey-flat)
     (heap-set!/bm loc 'flat)]
    [(grey-pair)
     (mark-grey (heap-ref/bm (+ loc 1)))
     (mark-grey (heap-ref/bm (+ loc 2)))
     (heap-set!/bm loc 'pair)]
    [(grey-proc)
     (for ([i (in-range (heap-ref/bm (+ loc 2)))])
          (mark-grey (heap-ref/bm (+ loc 3 i))))
     (heap-set!/bm loc 'proc)]
    [(grey-vector)
     (for ([i (in-range (heap-ref/bm (+ loc 1)))])
          (mark-grey (heap-ref/bm (+ loc 2 i))))
     (heap-set!/bm loc 'vector)]
    [(grey-struct)
     (define parent (heap-ref/bm (+ loc 2)))
     (when parent (mark-grey parent))
     (heap-set!/bm loc 'struct)]
    [(grey-struct-instance)
     (define fv-count (heap-ref/bm (+ 3 (heap-ref/bm (+ loc 1)))))
     (mark-grey (heap-ref/bm (+ loc 1)))
     (for ([i (in-range fv-count)])
          (mark-grey (heap-ref/bm (+ loc 2 i))))
     (heap-set!/bm loc 'struct-instance)]
    [(pair flat proc struct struct-instance white-pair white-flat white-proc white-struct white-struct-instance cont) (void)]))

(define (mark-grey loc)
  (case (heap-ref/bm loc)
    [(white-pair) (heap-set!/bm loc 'grey-pair)]
    [(white-flat) (heap-set!/bm loc 'grey-flat)]
    [(white-proc) (heap-set!/bm loc 'grey-proc)]
    [(white-vector) (heap-set!/bm loc 'grey-vector)]
    [(white-struct) (heap-set!/bm loc 'grey-struct)]
    [(white-struct-instance) (heap-set!/bm loc 'grey-struct-instance)]
    [(pair flat proc struct struct-instance grey-pair grey-flat grey-proc grey-struct grey-struct-instance cont) (void)]))

(define (heap-cont/check)
  (check/cont 2nd-gen-start)
  (heap-check 2nd-gen-start))

;; check what allocated to cont
(define (check/cont-alloc loc thing)
  (cond
    [(= loc tracing-head-word)
     (unless (or (equal? (heap-ref/bm thing) 'cont)
                 (= thing 0))
       (error 'check/cont-alloc "not a cont at location ~a" thing))]
    [else (error 'check/cont-alloc "meant to check location 3")]))

(define (heap-check loc)
  (when (< loc 2nd-gen-size)
    (case (heap-ref/bm loc)
      [(pair) 
       (if (or (white? (heap-ref/bm (+ loc 1)))
               (white? (heap-ref/bm (+ loc 2))))
         (error 'heap-check "black object points to white object at ~a" loc)
         (heap-check (+ loc 3)))]
      [(flat) 
       (heap-check (+ loc 2))]
      [(proc)
       (define closure-size (heap-ref/bm (+ loc 2)))
       (case closure-size
         [(free)
          (heap-check (+ loc 3))]
         [else
           (for ([i (in-range closure-size)])
                (when (white? (heap-ref/bm (+ loc 3 i)))
                  (error 'heap-check "black object points to white object at ~a" loc)))
           (heap-check (+ loc 3 closure-size))])]
      [(vector)
       (define size (heap-ref/bm (+ loc 1)))
       (case size
         [(free)
          (heap-check (+ loc 2))]
         [else
           (for ([i (in-range size)])
                (when (white? (heap-ref/bm (+ loc 2 i)))
                  (error 'heap-check "black object points to white object at ~a" loc)))
           (heap-check (+ loc 2 size))])]
      [(struct)
       (define parent (heap-ref/bm (+ loc 2)))
       (when (and parent 
                  (white? parent))
         (error 'heap-check "black object points to white object at ~a" loc))
       (heap-check (+ loc 4))]
      [(struct-instance)
       (when (white? (heap-ref/bm (+ loc 1)))
         (error 'heap-check "black object points to white object at ~a" loc))
       (define field-count (heap-ref/bm (+ 3 (heap-ref/bm (+ loc 1)))))
       (for ([i (in-range field-count)])
         (when (white? (heap-ref/bm (+ loc 2 i)))
           (error 'heap-check "black object points to white object at ~a" loc)))
       (heap-check (+ loc 2 field-count))]
      [(white-pair grey-pair)
       (heap-check (+ loc 3))]
      [(white-flat grey-flat)
       (heap-check (+ loc 2))]
      [(white-proc grey-proc)
       (heap-check (+ loc 3 (heap-ref/bm (+ loc 2))))]
      [(white-vector grey-vector)
       (heap-check (+ loc 2 (heap-ref/bm (+ loc 1))))]
      [(white-struct grey-struct)
       (heap-check (+ loc 4))]
      [(white-struct-instance grey-struct-instance)
       (heap-check (+ loc 2 (heap-ref/bm (+ 3 (heap-ref/bm (+ 1 loc))))))]
      [(free) (heap-check (+ loc 1))]
      [(cont) (heap-check (+ loc 3))]
      [else (error 'check "unknown tag @ ~a" loc)])))

(define (check/tag loc)
  (when (< loc 2nd-gen-size)
    (case (heap-ref/bm loc)
      [(white-pair) (check/tag (+ loc 3))]
      [(white-flat) (check/tag (+ loc 2))]
      [(white-proc) (check/tag (+ loc 3 (heap-ref/bm (+ loc 2))))]
      [(white-vector) (check/tag (+ loc 2 (heap-ref/bm (+ loc 1))))]
      [(white-struct) (check/tag (+ loc 4))]
      [(white-struct-instance) (check/tag (+ loc 2 (heap-ref/bm (+ 3 (heap-ref/bm (+ loc 1))))))]
      [(free) (check/tag (+ loc 1))]
      [else (error 'check/tag "wrong tag at ~a" loc)])))

(define (check/cont loc)
  (when (< loc 2nd-gen-size)
    (case (heap-ref/bm loc)
      [(pair white-pair grey-pair) (check/cont (+ loc 3))]
      [(flat white-flat grey-flat) (check/cont (+ loc 2))]
      [(proc white-proc grey-proc) 
       (define closure-size (heap-ref/bm (+ loc 2)))
       (check/cont (+ loc 3 closure-size))]
      [(vector white-vector grey-vector)
       (define size (heap-ref/bm (+ loc 1)))
       (check/cont (+ loc 2 size))]
      [(struct white-struct grey-struct) (check/cont (+ loc 4))]
      [(struct-instance white-struct-instance grey-struct-instance)
       (define fields-num (heap-ref/bm (+ 3 (heap-ref/bm (+ loc 1)))))
       (check/cont (+ loc 2 fields-num))]
      [(free) (check/cont (+ loc 1))]
      [(cont) (check-helper/cont (heap-ref/bm (+ loc 2)))]
      [else (error 'check/cont "wrong tag at ~a" loc)])))

(define (check-helper/cont loc)
  (cond
    [(equal? 'cont (heap-ref/bm loc))
     (check-helper/cont (heap-ref/bm (+ loc 2)))]
    [(= loc 0) (void)]
    [else
      (error 'check/cont "cont links to wrong tag at ~a" loc)]))

;; white? : location? -> boolean?
(define (white? loc)
  (case (heap-ref/bm loc)
    [(white-pair white-proc white-flat white-vector white-struct white-struct-instance)
     true]
    [else false]))

(define (next/cont)
  (define loc (heap-ref/bm tracing-head-word))
  (cond
    [(= loc 0) 0]
    [else
      (heap-set!/bm tracing-head-word (heap-ref/bm (+ loc 2)))
      loc]))

(define (push/cont ptr)
  (mark-grey ptr)
  (case (heap-ref/bm ptr)
    [(pair flat proc vector strut struct-instance) (void)]
    [else
      (define loc (find-free-space 2nd-gen-start 2nd-gen-size 3))
      (define head (heap-ref/bm tracing-head-word))
      (heap-set!/bm loc 'cont)
      (heap-set!/bm (+ loc 1) ptr)
      (heap-set!/bm (+ loc 2) head)

      ;; update peak heap size
      (heap-set! volume-word (+ 3 (heap-ref volume-word)))
      (when (> (heap-ref volume-word) peak-heap-size)
        (set! peak-heap-size (heap-ref volume-word)))

      (heap-set!/bm tracing-head-word loc)]))

(define (continue/incre-mark)
  (if (step/finished?)
    (heap-set!/bm step-count-word 0)
    (traverse/incre-mark (next/cont))))

(define (clean/cont loc)
  (heap-set!/bm loc 'free)
  (heap-set!/bm (+ loc 1) 'free)
  (heap-set!/bm (+ loc 2) 'free)
  (heap-set! volume-word (- (heap-ref volume-word) 3)))

(define (step/count n)
  (heap-set!/bm step-count-word (- (heap-ref/bm step-count-word) n)))

(define (step/finished?)
  (<= (heap-ref/bm step-count-word) 0))

(print-only-errors #t)
;; test init
(define (test-init heap-size)
  (set! 1st-gen-size (round (* heap-size 1/4)))
  (set! 2nd-gen-size (round (* heap-size 7/8)))
  (set! 2nd-gen-start (+ 4 1st-gen-size))
  (set! status-word 1st-gen-size)
  (set! volume-word (+ 1 1st-gen-size))
  (set! step-count-word (+ 2 1st-gen-size))
  (set! tracing-head-word (+ 3 1st-gen-size))
  (set! table-start-word 2nd-gen-size)
  (heap-set!/bm status-word 'out)
  (heap-set!/bm volume-word 0)
  (heap-set!/bm step-count-word 0)
  (heap-set!/bm tracing-head-word 0)
  (heap-set!/bm table-start-word (+ table-start-word 1)))
;; test for forward/pointers
(let ([test-heap (vector 2 'left 'flat 2
                         'free 'free 'free 'free
                         'out 0 0 0
                         'vector 1 2 'free
                         'free 'free 'free 'free
                         'free 'free 'free 'free
                         'free 'free 'free 'free
                         29 14 2 'free)])
  (test (with-heap test-heap
                   (test-init 32)
                   (forward/pointers 29)
                   test-heap)
        (vector 2 'left 'frwd 15
                'free 'free 'free 'free
                'out 2 2 0
                'vector 1 15 'flat
                2 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                29 'free 'free 'free)))
