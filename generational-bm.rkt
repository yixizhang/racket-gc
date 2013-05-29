#lang plai/gc2/collector

;; config for collection
(define volume-word "current total size of objects in old generation")
(define status-word "in/out of collection")
(define table-start "start position of table for intergenerational pointers")

;; recordig vars for benchmarking
(define peak-heap-size 0)
(define heap-size-check-time 0)
(define total-heap-size 0)
(define peak-heap-operations 0)
(define current-heap-operations 0)
(define heap-operation-check-time 0)
(define total-heap-operations 0)

;; init-allocator : -> void
(define (init-allocator)
  (set! volume-word 0)
  (set! status-word 1)
  (heap-set!/bm volume-word 0)
  (heap-set!/bm status-word 'out)
  (for ([i (in-range 2 (heap-size))])
    (heap-set!/bm i 'free))
  (set! table-start (round (* (heap-size) 7/8)))
  (heap-set!/bm table-start (+ table-start 1)))

;; 1st gen takes 1/4 of entire heap
(define (1st-gen-size)
  (round (* (heap-size) 1/4)))
(define (2nd-gen-size)
  (round (* (heap-size) 7/8)))

;; gc:deref : loc -> heap-value
;; must signal an error if fl-loc doesn't point to a flat value
(define (gc:deref fl-loc)
  (case (heap-ref/bm fl-loc)
    [(flat) (heap-ref/bm (+ fl-loc 1))]
    [(frwd) (gc:deref (heap-ref/bm (+ fl-loc 1)))]
    [else (error 'gc:deref
                 "non-flat @ ~s"
                 fl-loc)]))

;; track/loc : loc -> loc
;; if loc points to a flat or pair or proc, then return loc
;; else if loc points to a frwd, return the frwd address
(define (track/loc loc)
  (case (heap-ref/bm loc)
    [(flat pair proc vector struct struct-instance) loc]
    [(frwd) (heap-ref/bm (+ loc 1))]
    [else (error 'track/loc "wrong tag ~s at ~a" (heap-ref/bm loc) loc)]))

;; gc:alloc-flat : heap-value -> loc
(define (gc:alloc-flat fv)
  (define ptr (alloc 2 #f #f))
  (heap-set!/bm ptr 'flat)
  (heap-set!/bm (+ ptr 1) fv)
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
  (when (and (= ptr 2)
             (or (need-forwarding-pointers? hd/loc)
                 (need-forwarding-pointers? tl/loc)))
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
    [(flat) #t]
    [(frwd) (gc:flat? (heap-ref/bm (+ loc 1)))]
    [else #f]))

;; gc:cons? : loc -> boolean
;; loc is guaranteed to have been an earlier
;; result from either gc:alloc-flat or gc:cons
(define (gc:cons? loc)
  (case (heap-ref/bm loc)
    [(pair) #t]
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
  (when (and (= next 2)
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
    [(proc) #t]
    [(frwd) (gc:closure? (heap-ref/bm (+ loc 1)))]
    [else #f]))

;; vector related
(define (gc:vector length loc)
  (define next (alloc (+ length 2) loc #f))
  (define l (track/loc loc))
  (when (and (= next 2)
             (need-forwarding-pointers? loc))
    (free-1st-gen))
  (heap-set!/bm next 'vector)
  (heap-set!/bm (+ next 1) length)
  (for ([i (in-range length)])
    (heap-set!/bm (+ next 2 i) l))
  next)

(define (gc:vector? loc)
  (case (heap-ref/bm loc)
    [(vector) #t]
    [(frwd) (gc:vector? (heap-ref/bm (+ loc 1)))]
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
    [(< number (gc:vector-length loc)) 
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
  (when (and (= next 2)
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

(define (gc:struct-pred s instance)
  (and (equal? (heap-ref s) 'struct)
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
  (define next (heap-ref/bm (2nd-gen-size)))
  (cond
    [(>= (+ next 2) (heap-size))
     (forward/pointers (+ 1 table-start))
     (heap-set!/bm (+ 1 table-start) pointer)
     (heap-set!/bm (+ 2 table-start) target)
     (heap-set!/bm table-start (+ 3 table-start))]
    [else
     (heap-set!/bm next pointer)
     (heap-set!/bm (+ next 1) target)
     (heap-set!/bm (2nd-gen-size) (+ next 2))]))

;; alloc : number[size] roots roots -> loc
(define (alloc n some-roots more-roots)
  (define addr (find-free-space 2 (1st-gen-size) n))
  (cond 
    [addr addr]
    [else
     (collect-garbage some-roots more-roots)
     (unless (or (need-forwarding-pointers? some-roots)
                 (need-forwarding-pointers? more-roots))
       (free-1st-gen))
     (unless (<= (+ 2 n) (1st-gen-size))
       (error 'alloc "no space"))
     2]))

(define (need-forwarding-pointers? thing)
  (cond
    [(list? thing) (ormap 1st-gen? thing)]
    [(root? thing) (1st-gen? (read-root thing))]
    [(number? thing) (1st-gen? thing)]
    [else thing]))

(define (find-free-space start end size)
  (cond
    [(= start end) #f]
    [else
     (case (heap-ref/bm start)
       [(free) (if (n-free-blocks? start end size)
                   start
                   (find-free-space (+ start 1) end size))]
       [(flat) (find-free-space (+ start 2) end size)]
       [(pair) (find-free-space (+ start 3) end size)]
       [(proc)
        (find-free-space
         (+ start 3 (heap-ref/bm (+ start 2)))
         end
         size)]
       [(vector) 
        (find-free-space (+ start 2 (heap-ref/bm (+ start 1)))
                         end
                         size)]
       [(struct) (find-free-space (+ start 4) end size)]
       [(struct-instance) 
        (find-free-space (+ start 2 (heap-ref/bm (+ 3 (heap-ref/bm (+ start 1)))))
                         end
                         size)]
       [else (error 'find-free-space "ack ~s" start)])]))

(define (n-free-blocks? start end size)
  (cond
    [(= size 0) #t]
    [(= start end) #f]
    [else 
     (and (eq? 'free (heap-ref/bm start))
          (n-free-blocks? (+ start 1)
                          end
                          (- size 1)))]))

(define (2nd-gen-gc some-roots more-roots)
  (define start (1st-gen-size))
  (mark-white! start)
  (traverse/roots (get-root-set))
  (traverse/roots some-roots)
  (traverse/roots more-roots)
  (make-pointers-to-2nd-gen-roots 2)
  (free-white! start))

(define (make-pointers-to-2nd-gen-roots start)
  (cond
    [(= start (1st-gen-size))
     (void)]
    [else
     (case (heap-ref/bm start)
       [(flat) (make-pointers-to-2nd-gen-roots (+ start 2))]
       [(pair) (define one-loc (heap-ref/bm (+ start 1)))
               (when (2nd-gen? one-loc) (traverse/roots one-loc))
               (define another-loc (heap-ref/bm (+ start 2)))
               (when (2nd-gen? another-loc) (traverse/roots another-loc))
               (make-pointers-to-2nd-gen-roots (+ start 3))]
       [(proc) (define fv-counts (heap-ref/bm (+ start 2)))
               (for ([i (in-range fv-counts)])
                 (define loc (heap-ref/bm (+ start 3 i)))
                 (when (2nd-gen? loc) (traverse/roots loc)))
               (make-pointers-to-2nd-gen-roots (+ start 3 fv-counts))]
       [(vector) (define element-count (heap-ref/bm (+ start 1)))
                 (for ([i (in-range element-count)])
                   (define loc (heap-ref/bm (+ start 2 i)))
                   (when (2nd-gen? loc) (traverse/roots loc)))
                 (make-pointers-to-2nd-gen-roots (+ start 2 element-count))]
       [(struct) (define parent (heap-ref/bm (+ start 2)))
                 (when (and parent (2nd-gen? parent))
                   (traverse/roots parent))
                 (make-pointers-to-2nd-gen-roots (+ start 4))]
       [(struct-instance) (define fields-count (heap-ref/bm (+ 3 (heap-ref/bm (+ start 1)))))
                          (define struct-loc (heap-ref/bm (+ start 1)))
                          (when (2nd-gen? struct-loc)
                            (traverse/roots struct-loc))
                          (for ([i (in-range fields-count)])
                            (define loc (heap-ref/bm (+ start 2 i)))
                            (when (2nd-gen? loc) (traverse/roots loc)))
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

(define (mark-white! i)
  (when (< i (2nd-gen-size))
    (case (heap-ref/bm i)
      [(flat) (heap-set!/bm i 'white-flat)
              (mark-white! (+ i 2))]
      [(pair) (heap-set!/bm i 'white-pair)
              (mark-white! (+ i 3))]
      [(proc) (heap-set!/bm i 'white-proc)
              (mark-white! (+ i 3 (heap-ref/bm (+ i 2))))]
      [(vector) (heap-set!/bm i 'white-vector)
                (mark-white! (+ i 2 (heap-ref/bm (+ i 1))))]
      [(struct) (heap-set!/bm i 'white-struct)
                (mark-white! (+ i 4))]
      [(struct-instance) (heap-set!/bm i 'white-struct-instance)
                         (mark-white! (+ i 2 (heap-ref/bm (+ 3 (heap-ref/bm (+ i 1))))))]
      [(free) (mark-white! (+ i 1))]
      [else (error 'mark-white! "wrong tag at ~a" i)])))

(define (traverse/roots thing)
  (cond
    [(list? thing)
     (for-each traverse/roots thing)]
    [(root? thing)
     (traverse/loc (read-root thing))]
    [(number? thing)
     (traverse/loc thing)]))

(define (traverse/loc loc)
  (when (2nd-gen? loc)
    (case (heap-ref/bm loc)
      [(white-flat)
       (heap-set!/bm loc 'flat)]
      [(white-pair)
       (heap-set!/bm loc 'pair)
       (traverse/loc (heap-ref/bm (+ loc 1)))
       (traverse/loc (heap-ref/bm (+ loc 2)))]
      [(white-proc)
       (heap-set!/bm loc 'proc)
       (for ([x (in-range (heap-ref/bm (+ loc 2)))])
         (traverse/loc (heap-ref/bm (+ loc 3 x))))]
      [(white-vector)
       (heap-set!/bm loc 'vector)
       (for ([x (in-range (heap-ref/bm (+ loc 1)))])
         (traverse/loc (heap-ref/bm (+ loc 2 x))))]
      [(white-struct)
       (heap-set!/bm loc 'struct)
       (define parent (heap-ref/bm (+ loc 2)))
       (when parent (traverse/loc parent))]
      [(white-struct-instace)
       (heap-set!/bm loc 'struct-instance)
       (traverse/loc (heap-ref/bm (+ loc 1)))
       (for ([x (in-range (heap-ref/bm (+ 3 (heap-ref/bm (+ loc 1)))))])
         (traverse/loc (heap-ref/bm (+ loc 2 x))))]
      [(pair flat proc vector struct struct-instance) (void)]
      [else (error 'traverse/loc "wrong tag at ~a" loc)])))

(define (free-white! i)
  (when (< i (2nd-gen-size))
    (case (heap-ref/bm i)
      [(flat) (free-white! (+ i 2))]
      [(pair) (free-white! (+ i 3))]
      [(proc) (free-white! (+ i 3 (heap-ref/bm (+ i 2))))]
      [(white-flat) (heap-set!/bm i 'free)
                    (heap-set!/bm (+ i 1) 'free)
                    (heap-set! volume-word (- (heap-ref volume-word) 2))
                    (free-white! (+ i 2))]
      [(white-pair) (heap-set!/bm i 'free)
                    (heap-set!/bm (+ i 1) 'free)
                    (heap-set!/bm (+ i 2) 'free)
                    (heap-set! volume-word (- (heap-ref volume-word) 3))
                    (free-white! (+ i 3))]
      [(white-proc) (define closure-size (heap-ref/bm (+ i 2)))
                    (for ([x (in-range (+ closure-size 3))])
                      (heap-set!/bm (+ i x) 'free))
                    (heap-set! volume-word (- (heap-ref volume-word) (+ closure-size 3)))
                    (free-white! (+ i 3 closure-size))]
      [(white-vector) (define size (heap-ref/bm (+ i 1)))
                      (for ([x (in-range (+ size 2))])
                        (heap-set!/bm (+ i x) 'free))
                      (heap-set! volume-word (- (heap-ref volume-word) (+ size 2)))
                      (free-white! (+ i 2 size))]
      [(white-struct) (for ([x (in-range 4)])
                        (heap-set!/bm (+ i x)))
                      (heap-set! volume-word (- (heap-ref volume-word) 4))
                      (free-white! (+ i 4))]
      [(white-struct-instance) (define fields-count (heap-ref/bm (+ 3 (heap-ref/bm (+ i 1)))))
                               (for ([x (in-range (+ 2 fields-count))])
                                    (heap-set!/bm (+ i x)))
                               (heap-set! volume-word (- (heap-ref volume-word) (+ fields-count 2)))
                               (free-white! (+ i 2 fields-count))]
      [(free) (free-white! (+ i 1))]
      [else (error 'free-white! "wrong tag at ~a" i)])))

;; collect-garbage : roots roots -> void
(define (collect-garbage some-roots more-roots)
  ;; preparation for heap operations benchmarks
  (heap-set!/bm status-word 'in)
  (set! current-heap-operations 0)

  (forward/roots (get-root-set))
  (forward/roots some-roots)
  (forward/roots more-roots)
  (forward/pointers (+ 1 (2nd-gen-size)))

  (heap-set!/bm status-word 'out)
  ;; metrics recording and print-out
  (set! heap-size-check-time (add1 heap-size-check-time))
  (set! total-heap-size (+ (heap-ref volume-word) total-heap-size))
  (printf "peak heap size is ~s%, average heap size is ~s%\n"
          (round (/ (* 100 peak-heap-size) (- (2nd-gen-size) (1st-gen-size))))
          (round (/ (* 100 (/ total-heap-size heap-size-check-time)) 
                    (- (2nd-gen-size) (1st-gen-size)))))
  (set! heap-operation-check-time (add1 heap-operation-check-time))
  (when (> current-heap-operations peak-heap-operations)
    (set! peak-heap-operations current-heap-operations))
  (set! total-heap-operations (+ current-heap-operations total-heap-operations))
  (printf "heap operations: peak ~s, average ~s, total ~s\n"
          peak-heap-operations
          (round (/ total-heap-operations heap-operation-check-time))
          total-heap-operations))

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
       [(flat) (define new-addr (copy/alloc 2 #f #f))
               (heap-set!/bm new-addr 'flat)
               (heap-set!/bm (+ new-addr 1) (heap-ref/bm (+ loc 1))) 
               (heap-set!/bm loc 'frwd)
               (heap-set!/bm (+ loc 1) new-addr)
               new-addr]
       [(pair) (define new-addr (copy/alloc 3
                                            (heap-ref/bm (+ loc 1))
                                            (heap-ref/bm (+ loc 2))))
               (heap-set!/bm new-addr 'pair)
               (heap-set!/bm (+ new-addr 1) (track/loc (heap-ref/bm (+ loc 1))))
               (heap-set!/bm (+ new-addr 2) (track/loc (heap-ref/bm (+ loc 2))))
               (heap-set!/bm loc 'frwd)
               (heap-set!/bm (+ loc 1) new-addr)
               new-addr]
       [(proc) (define length (+ 3 (heap-ref/bm (+ loc 2))))
               (define free-vars (build-vector (- length 3)
                                               (lambda (i)
                                                 (heap-ref/bm (+ loc 3 i)))))
               (define new-addr (copy/alloc length free-vars '()))
               (for ([x (in-range 0 3)])
                 (heap-set!/bm (+ new-addr x) (heap-ref/bm (+ loc x))))
               (for ([x (in-range 3 length)])
                 (heap-set!/bm (+ new-addr x) (track/loc (heap-ref/bm (+ loc x)))))
               (heap-set!/bm loc 'frwd)
               (heap-set!/bm (+ loc 1) new-addr)
               new-addr]
       [(vector) (define var-count (heap-ref/bm (+ loc 1)))
                 (define vars (build-vector var-count
                                            (lambda (i)
                                              (heap-ref/bm (+ loc 2 i)))))
                 (define new-addr (copy/alloc (+ 2 var-count) vars '()))
                 (for ([x (in-range 0 2)])
                   (heap-set!/bm (+ new-addr x) (heap-ref/bm (+ loc x))))
                 (for ([x (in-range var-count)])
                   (heap-set!/bm (+ new-addr 2 x) (track/loc (heap-ref/bm (+ loc 2 x)))))
                 (heap-set!/bm loc 'frwd)
                 (heap-set!/bm (+ loc 1) new-addr)
                 new-addr]
       [(struct) (define new-addr (copy/alloc 4 (heap-ref/bm (+ loc 2)) #f))
                 (heap-set!/bm new-addr 'struct)
                 (heap-set!/bm (+ new-addr 1) (heap-ref/bm (+ loc 1)))
                 (define parent (heap-ref/bm (+ loc 2)))
                 (heap-set!/bm (+ new-addr 2) (and parent (track/loc parent)))
                 (heap-set!/bm (+ new-addr 3) (heap-ref/bm (+ loc 3)))
                 (heap-set!/bm loc 'frwd)
                 (heap-set!/bm (+ loc 1) new-addr)
                 new-addr]
       [(struct-instance) (define var-count (heap-ref/bm (+ 3 (heap-ref/bm (+ loc 1)))))
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

(define (copy/alloc n some-roots more-roots)
  (define next (find-free-space (1st-gen-size) (2nd-gen-size) n))
  (cond
    [next
      ;; update peak heap size
      (heap-set! volume-word (+ n (heap-ref volume-word)))
      (when (> (heap-ref volume-word) peak-heap-size)
        (set! peak-heap-size (heap-ref volume-word)))
      next]
    [else
     (2nd-gen-gc some-roots more-roots)
     (define next (find-free-space (1st-gen-size) (2nd-gen-size) n))
     (unless next (error 'copy/alloc "no space"))
     next]))

;; forward/ref : loc -> loc
;; move the referenced object to the other semi-space
;; and return the new addr of moved object
(define (forward/ref loc)
  (case (heap-ref/bm loc)
    [(flat) (void)]
    [(pair) (gc:set-first! loc (forward/loc (heap-ref/bm (+ loc 1))))
            (gc:set-rest! loc (forward/loc (heap-ref/bm (+ loc 2))))
            (forward/ref (heap-ref (+ loc 1)))
            (forward/ref (heap-ref (+ loc 2)))]
    [(proc) (define fv-count (heap-ref/bm (+ loc 2)))
            (for ([x (in-range 0 fv-count)])
              (define l (+ loc 3 x))
              (heap-set!/bm l (forward/loc (heap-ref/bm l)))
              (forward/ref (heap-ref/bm l)))]
    [(vector) (define var-count (heap-ref/bm (+ loc 1)))
              (for ([x (in-range var-count)])
                (define l (+ loc 2 x))
                (heap-set!/bm l (forward/loc (heap-ref/bm l)))
                (forward/ref (heap-ref/bm l)))]
    [(struct) (define parent (heap-ref/bm (+ loc 2)))
              (when parent
                (heap-set!/bm (+ loc 2) (forward/loc parent))
                (forward/ref (heap-ref/bm (+ loc 2))))]
    [(struct-instance) (define fields-count (heap-ref/bm (+ 3 (heap-ref/bm (+ loc 1)))))
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
     (heap-set!/bm (+ loc 1) new-addr)
     (forward/ref new-addr)
     (heap-set!/bm loc 'free)
     (heap-set!/bm (+ loc 1) 'free)
     (forward/pointers (+ loc 2))]))

(define (free-1st-gen)
  (for ([i (in-range 2 (1st-gen-size))])
    (heap-set!/bm i 'free)))

(define (1st-gen? loc)
  (and (>= loc 2)
       (< loc (1st-gen-size))))

(define (2nd-gen? loc)
  (and (>= loc (1st-gen-size))
       (< loc (2nd-gen-size))))

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
