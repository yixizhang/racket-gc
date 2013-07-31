#lang plai/gc2/collector

;; config for collection
(define alloc-word "next location of young generation allocation")
(define status-word "in/out of collection")
(define free-list-head "a word holds the location of head of free slots list")
(define table-start "start position of table for intergenerational pointers")

;; init-allocator : -> void
(define (init-allocator)
  (set! alloc-word 0)
  (set! status-word 1)
  (heap-set! alloc-word 2)
  (heap-set! status-word 'out)
  (for ([i (in-range 2 (heap-size))])
    (heap-set! i 'free))
  (set! free-list-head (2nd-gen-size))
  (heap-set! (1st-gen-size) 'free-n)
  (heap-set! (+ 1 (1st-gen-size)) #f)
  (heap-set! (+ 2 (1st-gen-size)) (- (2nd-gen-size) (1st-gen-size)))
  (heap-set! free-list-head (1st-gen-size))
  (set! table-start (+ 1 (2nd-gen-size)))
  (heap-set! table-start (+ table-start 1)))

;; 1st gen takes 1/4 of entire heap
(define (1st-gen-size)
  (let ([s (round (* (heap-size) 1/4))])
    (if (odd? s)
        (add1 s)
        s)))
(define (2nd-gen-size)
  (round (* (heap-size) 7/8)))

;; -> loc loc
(define (to-space)
  ;; returns start and end of to-space
  (let ([next (heap-ref alloc-word)]
        [half (add1 (round (/ (1st-gen-size) 2)))])
    (if (< next half)
        (values 2 half)
        (values half (1st-gen-size)))))
(define (from-space)
  ;; returns start and end of from-space
  (let ([next (heap-ref alloc-word)]
        [half (add1 (round (/ (1st-gen-size) 2)))])
    (if (< next half)
        (values half (1st-gen-size))
        (values 2 half))))
 
(define (at-to-space? loc)
  (let-values ([(begin end) (to-space)])
    (and (>= loc begin)
         (< loc end))))
 
(define (2nd-gen? loc)
  (and (>= loc (1st-gen-size))
       (< loc (2nd-gen-size))))
 
;; gc:deref : loc -> heap-value
;; must signal an error if fl-loc doesn't point to a flat value
(define (gc:deref fl-loc)
  (case (heap-ref fl-loc)
    [(flat) (heap-ref (+ fl-loc 1))]
    [(frwd) (gc:deref (heap-ref (+ fl-loc 1)))]
    [else (error 'gc:deref
                 "non-flat @ ~s"
                 fl-loc)]))

;; track/loc : loc -> loc
;; if loc points to a flat or pair or proc, then return loc
;; else if loc points to a frwd, return the frwd address
(define (track/loc loc)
  (case (heap-ref loc)
    [(flat pair proc vector struct struct-instance) loc]
    [(frwd) (heap-ref (+ loc 1))]
    [else (error 'track/loc "wrong tag ~s at ~a" (heap-ref loc) loc)]))

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
  (heap-set! ptr 'pair)
  (heap-set! (+ ptr 1) head)
  (heap-set! (+ ptr 2) tail)
  ptr)

;; gc:first : loc -> loc
;; must signal an error of pr-loc does not point to a pair
(define (gc:first pr-loc)
  (if (equal? (heap-ref pr-loc) 'pair)
      (heap-ref (+ (track/loc pr-loc) 1))
      (error 'first "non pair @ ~s" pr-loc)))

;; gc:rest : loc -> loc
;; must signal an error of pr-loc does not point to a pair
(define (gc:rest pr-loc)
  (if (equal? (heap-ref pr-loc) 'pair)
      (heap-ref (+ (track/loc pr-loc) 2))
      (error 'rest "non pair @ ~s" pr-loc)))

;; gc:flat? : loc -> boolean
;; loc is guaranteed to have been an earlier
;; result from either gc:alloc-flat or gc:cons
(define (gc:flat? loc)
  (case (heap-ref loc)
    [(flat) #t]
    [(frwd) (gc:flat? (heap-ref (+ loc 1)))]
    [else #f]))

;; gc:cons? : loc -> boolean
;; loc is guaranteed to have been an earlier
;; result from either gc:alloc-flat or gc:cons
(define (gc:cons? loc)
  (case (heap-ref loc)
    [(pair) #t]
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
                (at-to-space? new))
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
                (at-to-space? new))
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
  (define updated-free-vars 
    (for/vector ([v (in-vector free-vars)])
                (track/loc v)))
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
    [(proc) #t]
    [(frwd) (gc:closure? (heap-ref (+ loc 1)))]
    [else #f]))

;; vector related
(define (gc:vector length loc)
  (define next (alloc (+ length 2) loc #f))
  (define l (track/loc loc))
  (heap-set! next 'vector)
  (heap-set! (+ next 1) length)
  (for ([i (in-range length)])
    (heap-set! (+ next 2 i) l))
  next)

(define (gc:vector? loc)
  (case (heap-ref loc)
    [(vector) #t]
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
    [(< number (gc:vector-length loc)) 
     (heap-set! (+ v-loc 2 number) thing)
     (when (and (2nd-gen? v-loc)
                (at-to-space? thing))
       (table/alloc (+ v-loc 2 number) thing))]
    [else (error 'gc:vector-set! 
                 "vector @ ~s index ~s out of range"
                 v-loc 
                 number)]))

;; struct related
(define (gc:alloc-struct name parent fields-count)
  (define next (alloc 4 parent #f))
  (define p (and parent (track/loc parent)))
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
  (define fields 
    (for/vector ([v (in-vector fields-value)])
                (track/loc v)))
  (heap-set! (+ next 1) ss)
  (for ([x (in-range 0 fv-count)])
    (heap-set! (+ next 2 x)
               (vector-ref fields x)))
  (heap-set! next 'struct-instance)
  next)

(define (gc:struct? loc)
  (case (heap-ref loc)
    [(struct) #t]
    [(frwd) (gc:struct? (heap-ref (+ loc 1)))]
    [else #f]))

(define (gc:struct-instance? loc)
  (case (heap-ref loc)
    [(struct-instance) #t]
    [(frwd) (gc:struct-instance? (heap-ref (+ loc 1)))]
    [else #f]))

(define (gc:struct-pred s instance)
  (and (gc:struct? s)
       (gc:struct-instance? instance)
       (let loop ([target s] [type (heap-ref (+ instance 1))])
         (and type
              (or (= target type)
                  (loop target (heap-ref (+ type 2))))))))

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
  (define next (heap-ref table-start))
  (cond
    [(>= (+ next 2) (heap-size))
     (forward/pointers (+ 1 table-start))
     (heap-set! (+ 1 table-start) pointer)
     (heap-set! (+ 2 table-start) target)
     (heap-set! table-start (+ 3 table-start))]
    [else
     (heap-set! next pointer)
     (heap-set! (+ next 1) target)
     (heap-set! table-start (+ next 2))]))

;; alloc : number[size] roots roots -> loc
(define (alloc n some-roots more-roots)
  (define addr (heap-ref alloc-word))
  (cond 
    [(enough-to-space? addr n)
     (heap-set! alloc-word (+ addr n))
     addr]
    [else
     (collect-garbage some-roots more-roots)
     (switch/sweep-tospace n)]))

;; loc number -> bool
(define (enough-to-space? start size)
  (define-values (begin end) (to-space))
  (< (+ start size) end))

;; number -> loc
(define (switch/sweep-tospace number)
  (define-values (begin end) (from-space))
  (for ([i (in-range begin end)])
       (heap-set! i 'free))
  (heap-set! alloc-word (+ begin number))
  begin)

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

(define (2nd-gen-gc some-roots more-roots)
  (define start (1st-gen-size))
  (mark-white! start)
  (traverse/roots (get-root-set))
  (traverse/roots some-roots)
  (traverse/roots more-roots)
  (make-pointers-to-2nd-gen-roots)
  (free-white! start #f #f #f))

(define (make-pointers-to-2nd-gen-roots)
  (define-values (begin end) (values 2 (1st-gen-size)))
  (let loop ([start begin])
    (cond
      [(= start end) (void)]
      [else
        (case (heap-ref start)
          [(flat) (loop (+ start 2))]
          [(pair) (define one-loc (heap-ref (+ start 1)))
                  (when (2nd-gen? one-loc) (traverse/roots one-loc))
                  (define another-loc (heap-ref (+ start 2)))
                  (when (2nd-gen? another-loc) (traverse/roots another-loc))
                  (loop (+ start 3))]
          [(proc) (define fv-counts (heap-ref (+ start 2)))
                  (for ([i (in-range fv-counts)])
                       (define loc (heap-ref (+ start 3 i)))
                       (when (2nd-gen? loc) (traverse/roots loc)))
                  (loop (+ start 3 fv-counts))]
          [(vector) (define element-count (heap-ref (+ start 1)))
                    (for ([i (in-range element-count)])
                         (define loc (heap-ref (+ start 2 i)))
                         (when (2nd-gen? loc) (traverse/roots loc)))
                    (loop (+ start 2 element-count))]
          [(struct) (define parent (heap-ref (+ start 2)))
                    (when (and parent (2nd-gen? parent))
                      (traverse/roots parent))
                    (loop (+ start 4))]
          [(struct-instance) (define fields-count (heap-ref (+ 3 (heap-ref (+ start 1)))))
                             (define struct-loc (heap-ref (+ start 1)))
                             (when (2nd-gen? struct-loc)
                               (traverse/roots struct-loc))
                             (for ([i (in-range fields-count)])
                                  (define loc (heap-ref (+ start 2 i)))
                                  (when (2nd-gen? loc) (traverse/roots loc)))
                             (loop (+ start 2 fields-count))]
          [(frwd) (define loc (heap-ref (+ start 1)))
                  (traverse/roots loc)
                  (case (heap-ref loc)
                    [(flat) (loop (+ start 2))]
                    [(pair) (loop (+ start 3))]
                    [(proc) (loop (+ start 3 (heap-ref (+ loc 2))))]
                    [(vector) (loop (+ start 2 (heap-ref (+ loc 1))))]
                    [(struct) (loop (+ start 4))]
                    [(struct-instance) (loop (+ start 2 (heap-ref (+ 3 (heap-ref (+ loc 1))))))])]
          [(free) (loop (+ start 1))]
          [else (error 'make-pointers-to-2nd-gen-roots "wrong tag at ~a" start)])])))

(define (mark-white! i)
  (when (< i (2nd-gen-size))
    (case (heap-ref i)
      [(flat) (heap-set! i 'white-flat)
              (mark-white! (+ i 2))]
      [(pair) (heap-set! i 'white-pair)
              (mark-white! (+ i 3))]
      [(proc) (heap-set! i 'white-proc)
              (mark-white! (+ i 3 (heap-ref (+ i 2))))]
      [(vector) (heap-set! i 'white-vector)
                (mark-white! (+ i 2 (heap-ref (+ i 1))))]
      [(struct) (heap-set! i 'white-struct)
                (mark-white! (+ i 4))]
      [(struct-instance) (heap-set! i 'white-struct-instance)
                         (mark-white! (+ i 2 (heap-ref (+ 3 (heap-ref (+ i 1))))))]
      [(free) (mark-white! (+ i 1))]
      [(free-2) (mark-white! (+ i 2))]
      [(free-n) (mark-white! (+ i (heap-ref (+ i 2))))]
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
    (case (heap-ref loc)
      [(white-flat)
       (heap-set! loc 'flat)]
      [(white-pair)
       (heap-set! loc 'pair)
       (traverse/loc (heap-ref (+ loc 1)))
       (traverse/loc (heap-ref (+ loc 2)))]
      [(white-proc)
       (heap-set! loc 'proc)
       (for ([x (in-range (heap-ref (+ loc 2)))])
         (traverse/loc (heap-ref (+ loc 3 x))))]
      [(white-vector)
       (heap-set! loc 'vector)
       (for ([x (in-range (heap-ref (+ loc 1)))])
         (traverse/loc (heap-ref (+ loc 2 x))))]
      [(white-struct)
       (heap-set! loc 'struct)
       (define parent (heap-ref (+ loc 2)))
       (when parent (traverse/loc parent))]
      [(white-struct-instance)
       (heap-set! loc 'struct-instance)
       (traverse/loc (heap-ref (+ loc 1)))
       (for ([x (in-range (heap-ref (+ 3 (heap-ref (+ loc 1)))))])
         (traverse/loc (heap-ref (+ loc 2 x))))]
      [(pair flat proc vector struct struct-instance) (void)]
      [else (error 'traverse/loc "wrong tag at ~a" loc)])))

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

;; free spaces by constructing free-list
;; free-white! : location location location number -> void
(define (free-white! loc prev last-start spaces-so-far)
  (unless (or (and last-start spaces-so-far)
              (not (or last-start spaces-so-far)))
    (error 'free-white! 
           "cumulating info are incorrect, last-start: ~s, spaces-so-far: ~s" 
           last-start spaces-so-far))
  
  (cond
    [(>= loc (2nd-gen-size))
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
                              last-start)])]
       [else (void)])]
    [else
      (define tag (heap-ref loc))
      (case tag
        [(flat pair proc vector struct struct-instance)
         (define length (object-length loc))
         (cond
           [(and last-start
                 spaces-so-far
                 (= 1 spaces-so-far))
            (free-white! (+ loc length) prev #f #f)]
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
            (free-white! (+ loc length) last-start #f #f)]
           [else (free-white! (+ loc length) prev #f #f)])]
        [(white-flat white-pair white-proc white-vector white-struct white-struct-instance
                     free free-2 free-n)
         (define length (object-length loc))
         (cond 
           [(and last-start spaces-so-far)
            (free-white! (+ loc length) prev last-start (+ spaces-so-far length))]
           [else (free-white! (+ loc length) prev loc length)])]
        [else (error 'free-white! "wrong tag at ~a" loc)])]))

;; collect-garbage : roots roots -> void
(define (collect-garbage some-roots more-roots)
  (forward/roots (get-root-set))
  (forward/roots some-roots)
  (forward/roots more-roots)
  (forward/pointers (+ 1 table-start)))

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
    [(at-to-space? loc)
     (case (heap-ref loc)
       [(flat) (define new-addr (copy/alloc 2 #f #f))
               (heap-set! new-addr 'flat)
               (heap-set! (+ new-addr 1) (heap-ref (+ loc 1))) 
               (heap-set! loc 'frwd)
               (heap-set! (+ loc 1) new-addr)
               new-addr]
       [(pair) (define new-addr (copy/alloc 3
                                            (heap-ref (+ loc 1))
                                            (heap-ref (+ loc 2))))
               (heap-set! new-addr 'pair)
               (heap-set! (+ new-addr 1) (track/loc (heap-ref (+ loc 1))))
               (heap-set! (+ new-addr 2) (track/loc (heap-ref (+ loc 2))))
               (heap-set! loc 'frwd)
               (heap-set! (+ loc 1) new-addr)
               new-addr]
       [(proc) (define length (+ 3 (heap-ref (+ loc 2))))
               (define free-vars (build-vector (- length 3)
                                               (lambda (i)
                                                 (heap-ref (+ loc 3 i)))))
               (define new-addr (copy/alloc length free-vars '()))
               (for ([x (in-range 0 3)])
                 (heap-set! (+ new-addr x) (heap-ref (+ loc x))))
               (for ([x (in-range 3 length)])
                 (heap-set! (+ new-addr x) (track/loc (heap-ref (+ loc x)))))
               (heap-set! loc 'frwd)
               (heap-set! (+ loc 1) new-addr)
               new-addr]
       [(vector) (define var-count (heap-ref (+ loc 1)))
                 (define vars (build-vector var-count
                                            (lambda (i)
                                              (heap-ref (+ loc 2 i)))))
                 (define new-addr (copy/alloc (+ 2 var-count) vars '()))
                 (for ([x (in-range 0 2)])
                   (heap-set! (+ new-addr x) (heap-ref (+ loc x))))
                 (for ([x (in-range var-count)])
                   (heap-set! (+ new-addr 2 x) (track/loc (heap-ref (+ loc 2 x)))))
                 (heap-set! loc 'frwd)
                 (heap-set! (+ loc 1) new-addr)
                 new-addr]
       [(struct) (define new-addr (copy/alloc 4 (heap-ref (+ loc 2)) #f))
                 (heap-set! new-addr 'struct)
                 (heap-set! (+ new-addr 1) (heap-ref (+ loc 1)))
                 (define parent (heap-ref (+ loc 2)))
                 (heap-set! (+ new-addr 2) (and parent (track/loc parent)))
                 (heap-set! (+ new-addr 3) (heap-ref (+ loc 3)))
                 (heap-set! loc 'frwd)
                 (heap-set! (+ loc 1) new-addr)
                 new-addr]
       [(struct-instance) (define var-count (heap-ref (+ 3 (heap-ref (+ loc 1)))))
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

(define (copy/alloc n some-roots more-roots)
  (define next (find-free-space (heap-ref free-list-head) #f n))
  (cond
    [next
     next]
    [else
     (2nd-gen-gc some-roots more-roots)
     (define next (find-free-space (heap-ref free-list-head) #f n))
     (unless next (error 'copy/alloc "no space"))
     next]))

;; forward/ref : loc -> loc
;; move the referenced object to the other semi-space
;; and return the new addr of moved object
(define (forward/ref loc)
  (case (heap-ref loc)
    [(flat) (void)]
    [(pair) (gc:set-first! loc (forward/loc (heap-ref (+ loc 1))))
            (gc:set-rest! loc (forward/loc (heap-ref (+ loc 2))))
            (forward/ref (heap-ref (+ loc 1)))
            (forward/ref (heap-ref (+ loc 2)))]
    [(proc) (define fv-count (heap-ref (+ loc 2)))
            (for ([x (in-range 0 fv-count)])
              (define l (+ loc 3 x))
              (heap-set! l (forward/loc (heap-ref l)))
              (forward/ref (heap-ref l)))]
    [(vector) (define var-count (heap-ref (+ loc 1)))
              (for ([x (in-range var-count)])
                (define l (+ loc 2 x))
                (heap-set! l (forward/loc (heap-ref l)))
                (forward/ref (heap-ref l)))]
    [(struct) (define parent (heap-ref (+ loc 2)))
              (when parent
                (heap-set! (+ loc 2) (forward/loc parent))
                (forward/ref (heap-ref (+ loc 2))))]
    [(struct-instance) (define fields-count (heap-ref (+ 3 (heap-ref (+ loc 1)))))
                       (heap-set! (+ loc 1) (forward/loc (heap-ref (+ loc 1))))
                       (forward/ref (heap-ref (+ loc 1)))
                       (for ([x (in-range fields-count)])
                         (define l (+ loc 2 x))
                         (heap-set! l (forward/loc (heap-ref l)))
                         (forward/ref (heap-ref l)))]
    [(frwd) (forward/ref (heap-ref (+ 1 loc)))]
    [else (error 'forward/ref "wrong tag at ~a" loc)]))

(define (forward/pointers loc)
  (cond
    [(or (= loc (heap-size))
         (equal? 'free (heap-ref loc)))
     (heap-set! table-start (add1 table-start))]
    [else
     (define new-addr (forward/loc (heap-ref (+ loc 1))))
     (heap-set! (+ loc 1) new-addr)
     (forward/ref new-addr)
     (heap-set! loc 'free)
     (heap-set! (+ loc 1) 'free)
     (forward/pointers (+ loc 2))]))
