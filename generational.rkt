#lang plai/gc2/collector

;; init-allocator : -> void
;; 2 pointers : allocation pointer
;;              active-semi-space pointer
(define (init-allocator)
  (heap-set! 0 2) ;; allocation pointer
  (heap-set! 1 'left) ;; active-semi-space : left -> left side, right -> right side
  (for ([i (in-range 2 (heap-size))])
    (heap-set! i 'free))
  (define table-start (round (* (heap-size) 7/8)))
  (heap-set! table-start (+ table-start 1)))

;; 1st gen takes 1/4 of entire heap
(define (mid) 
  (+ 1 (round (* (heap-size) 1/8))))
(define (1st-gen-size)
  (round (* (heap-size) 1/4)))
(define (2nd-gen-size)
  (round (* (heap-size) 7/8)))

;; gc:deref : loc -> heap-value
;; must signal an error if fl-loc doesn't point to a flat value
(define (gc:deref fl-loc)
  (case (heap-ref/check! fl-loc)
    [(flat) (heap-ref/check! (+ fl-loc 1))]
    [(frwd) (gc:deref (heap-ref/check! (+ fl-loc 1)))]
    [else (error 'gc:deref
                 "non-flat @ ~s"
                 fl-loc)]))

;; track/loc : loc -> loc
;; if loc points to a flat or pair or proc, then return loc
;; else if loc points to a frwd, return the frwd address
(define (track/loc loc)
  (case (heap-ref/check! loc)
    [(flat pair proc vector struct struct-instance) loc]
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
  (heap-set! ptr 'pair)
  (heap-set! (+ ptr 1) (track/loc hd))
  (heap-set! (+ ptr 2) (track/loc tl))
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
    [(flat) #t]
    [(frwd) (gc:flat? (heap-ref/check! (+ loc 1)))]
    [else #f]))

;; gc:cons? : loc -> boolean
;; loc is guaranteed to have been an earlier
;; result from either gc:alloc-flat or gc:cons
(define (gc:cons? loc)
  (case (heap-ref/check! loc)
    [(pair) #t]
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
  (heap-set! next 'proc)
  (heap-set! (+ next 1) code-ptr)
  (heap-set! (+ next 2) fv-count)
  (for ([x (in-range 0 fv-count)])
    (heap-set! (+ next 3 x)
               (track/loc (vector-ref free-vars x))))
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
    [(proc) #t]
    [(frwd) (gc:closure? (heap-ref/check! (+ loc 1)))]
    [else #f]))

;; vector related
(define (gc:vector length loc)
  (define next (alloc (+ length 2) #f #f))
  (heap-set! next 'vector)
  (heap-set! (+ next 1) length)
  (for ([i (in-range length)])
    (heap-set! (+ next 2 i) loc))
  next)

(define (gc:vector? loc)
  (equal? (heap-ref loc) 'vector))

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
  (heap-set! next 'struct)
  (heap-set! (+ next 1) name)
  (heap-set! (+ next 2) parent)
  (heap-set! (+ next 3) fields-count)
  next)

(define (gc:alloc-struct-instance s fields-value)
  (define fv-count (vector-length fields-value))
  (define next (alloc (+ fv-count 2)
                      s
                      (vector->roots fields-value)))
  (heap-set! (+ next 1) s)
  (for ([x (in-range 0 fv-count)])
    (heap-set! (+ next 2 x)
               (vector-ref fields-value x)))
  (heap-set! next 'struct-instance)
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
  (define next (heap-ref/check! (2nd-gen-size)))
  (cond
    [(>= (+ next 2) (heap-size))
     (error 'table/alloc "no space for indirection table")]
    [else
     (heap-set! next pointer)
     (heap-set! (+ next 1) target)
     (heap-set! (2nd-gen-size) (+ next 2))]))

;; alloc : number[size] roots roots -> loc
(define (alloc n some-roots more-roots)
  (define addr (heap-ref/check! 0))
  (cond 
    [(<= (+ addr n) (1st-gen-size))
     (heap-set! 0 (+ addr n))
     addr]
    [else
     (collect-garbage some-roots more-roots)
     (free-1st-gen)
     (define next (heap-ref/check! 0))
     (unless (<= (+ next n) (1st-gen-size))
       (error 'alloc "no space"))
     (heap-set! 0 (+ next n))
     next]))

(define (find-free-space start size)
  (cond
    [(= start (2nd-gen-size)) #f]
    [else
     (case (heap-ref/check! start)
       [(free) (if (n-free-blocks? start size)
                   start
                   (find-free-space (+ start 1) size))]
       [(flat) (find-free-space (+ start 2) size)]
       [(pair) (find-free-space (+ start 3) size)]
       [(proc)
        (find-free-space
         (+ start 3 (heap-ref/check! (+ start 2)))
         size)]
       [(vector) (find-free-space (+ start 2 (heap-ref/check! (+ start 1)))
                                  size)]
       [(struct) (find-free-space (+ start 4) size)]
       [(struct-instance) (find-free-space (+ start 2 (heap-ref/check! (+ 3 (heap-ref/check! (+ start 1))))) size)]
       [else
        (error 'find-free-space "ack ~s" start)])]))

(define (n-free-blocks? start size)
  (cond
    [(= size 0) #t]
    [(= start (2nd-gen-size)) #f]
    [else 
     (and (eq? 'free (heap-ref/check! start))
          (n-free-blocks? (+ start 1)
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
     (case (heap-ref/check! start)
       [(flat) (make-pointers-to-2nd-gen-roots (+ start 2))]
       [(pair) (define one-loc (heap-ref/check! (+ start 1)))
               (when (2nd-gen? one-loc) (traverse/roots one-loc))
               (define another-loc (heap-ref/check! (+ start 2)))
               (when (2nd-gen? another-loc) (traverse/roots another-loc))
               (make-pointers-to-2nd-gen-roots (+ start 3))]
       [(proc) (define fv-counts (heap-ref/check! (+ start 2)))
               (for ([i (in-range fv-counts)])
                 (define loc (heap-ref/check! (+ start 3 i)))
                 (when (2nd-gen? loc) (traverse/roots loc)))
               (make-pointers-to-2nd-gen-roots (+ start 3 fv-counts))]
       [(vector) (define element-count (heap-ref/check! (+ start 1)))
                 (for ([i (in-range element-count)])
                   (define loc (heap-ref/check! (+ start 2 i)))
                   (when (2nd-gen? loc) (traverse/roots loc)))
                 (make-pointers-to-2nd-gen-roots (+ start 2 element-count))]
       [(struct) (define parent (heap-ref/check! (+ start 2)))
                 (when (and parent (2nd-gen? parent))
                   (traverse/roots parent))
                 (make-pointers-to-2nd-gen-roots (+ start 4))]
       [(struct-instance) (define fields-count (heap-ref/check! (+ 3 (heap-ref/check! (+ start 1)))))
                          (define struct-loc (heap-ref/check! (+ start 1)))
                          (when (2nd-gen? struct-loc)
                            (traverse/roots struct-loc))
                          (for ([i (in-range fields-count)])
                            (define loc (heap-ref/check! (+ start 2 i)))
                            (when (2nd-gen? loc) (traverse/roots loc)))
                          (make-pointers-to-2nd-gen-roots (+ start 2 fields-count))]
       [(free) (void)]
       [else (error 'make-pointers-to-2nd-gen-roots "wrong tag at ~a" start)])]))

(define (mark-white! i)
  (when (< i (2nd-gen-size))
    (case (heap-ref/check! i)
      [(flat) (heap-set! i 'white-flat)
              (mark-white! (+ i 2))]
      [(pair) (heap-set! i 'white-pair)
              (mark-white! (+ i 3))]
      [(proc) (heap-set! i 'white-proc)
              (mark-white! (+ i 3 (heap-ref/check! (+ i 2))))]
      [(vector) (heap-set! i 'white-vector)
                (mark-white! (+ i 2 (heap-ref/check! (+ i 1))))]
      [(struct) (heap-set! i 'white-struct)
                (mark-white! (+ i 4))]
      [(struct-instance) (heap-set! i 'white-struct-instance)
                         (mark-white! (+ i 2 (heap-ref/check! (+ 3 (heap-ref/check! (+ i 1))))))]
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
    (case (heap-ref/check! loc)
      [(white-flat)
       (heap-set! loc 'flat)]
      [(white-pair)
       (heap-set! loc 'pair)
       (traverse/loc (heap-ref/check! (+ loc 1)))
       (traverse/loc (heap-ref/check! (+ loc 2)))]
      [(white-proc)
       (heap-set! loc 'proc)
       (for ([x (in-range (heap-ref/check! (+ loc 2)))])
         (traverse/loc (heap-ref/check! (+ loc 3 x))))]
      [(white-vector)
       (heap-set! loc 'vector)
       (for ([x (in-range (heap-ref/check! (+ loc 1)))])
         (traverse/loc (heap-ref/check! (+ loc 2 x))))]
      [(white-struct)
       (heap-set! loc 'struct)
       (define parent (heap-ref/check! (+ loc 2)))
       (when parent (traverse/loc parent))]
      [(white-struct-instace)
       (heap-set! loc 'struct-instance)
       (traverse/loc (heap-ref/check! (+ loc 1)))
       (for ([x (in-range (heap-ref/check! (+ 3 (heap-ref/check! (+ loc 1)))))])
         (traverse/loc (heap-ref/check! (+ loc 2 x))))]
      [(pair flat proc vector struct struct-instance) (void)]
      [else (error 'traverse/loc "wrong tag at ~a" loc)])))

(define (free-white! i)
  (when (< i (2nd-gen-size))
    (case (heap-ref/check! i)
      [(flat) (free-white! (+ i 2))]
      [(pair) (free-white! (+ i 3))]
      [(proc) (free-white! (+ i 3 (heap-ref/check! (+ i 2))))]
      [(white-flat) (heap-set! i 'free)
                    (heap-set! (+ i 1) 'free)
                    (free-white! (+ i 2))]
      [(white-pair) (heap-set! i 'free)
                    (heap-set! (+ i 1) 'free)
                    (heap-set! (+ i 2) 'free)
                    (free-white! (+ i 3))]
      [(white-proc) (define closure-size (heap-ref/check! (+ i 2)))
                    (for ([x (in-range (+ closure-size 3))])
                      (heap-set! (+ i x) 'free))
                    (free-white! (+ i 3 closure-size))]
      [(white-vector) (define size (heap-ref/check! (+ i 1)))
                      (for ([x (in-range (+ size 2))])
                        (heap-set! (+ i x) 'free))
                      (free-white! (+ i 2 size))]
      [(white-struct) (for ([x (in-range 4)])
                        (heap-set! (+ i x)))
                      (free-white! (+ i 4))]
      [(white-struct-instance) (define fields-count (heap-ref/check! (+ 3 (heap-ref/check! (+ i 1)))))
                               (for ([x (in-range (+ 2 fields-count))])
                                 (heap-set! (+ i x)))
                               (free-white! (+ i 2 fields-count))]
      [(free) (free-white! (+ i 1))]
      [else (error 'free-white! "wrong tag at ~a" i)])))

;; collect-garbage : roots roots -> void
(define (collect-garbage some-roots more-roots)
  (forward/roots (get-root-set))
  (forward/roots some-roots)
  (forward/roots more-roots)
  (forward/pointers (+ 1 (2nd-gen-size))))

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

(define (copy/alloc n some-roots more-roots)
  (define next (find-free-space (1st-gen-size) n))
  (cond
    [next next]
    [else
     (2nd-gen-gc some-roots more-roots)
     (define next (find-free-space (1st-gen-size) n))
     (unless next (error 'copy/alloc "no space"))
     next]))

;; forward/ref : loc -> loc
;; move the referenced object to the other semi-space
;; and return the new addr of moved object
(define (forward/ref loc)
  (case (heap-ref/check! loc)
    [(flat) (void)]
    [(pair) (gc:set-first! loc (forward/loc (heap-ref/check! (+ loc 1))))
            (gc:set-rest! loc (forward/loc (heap-ref/check! (+ loc 2))))
            (forward/ref (heap-ref (+ loc 1)))
            (forward/ref (heap-ref (+ loc 2)))]
    [(proc) (define fv-count (heap-ref/check! (+ loc 2)))
            (for ([x (in-range 0 fv-count)])
              (define l (+ loc 3 x))
              (heap-set! l (forward/loc (heap-ref/check! l)))
              (forward/ref (heap-ref/check! l)))]
    [(vector) (define var-count (heap-ref/check! (+ loc 1)))
              (for ([x (in-range var-count)])
                (define l (+ loc 2 x))
                (heap-set! l (forward/loc (heap-ref/check! l)))
                (forward/ref (heap-ref/check! l)))]
    [(struct) (define parent (heap-ref/check! (+ loc 2)))
              (when parent
                (heap-set! (+ loc 2) (forward/loc parent))
                (forward/ref (heap-ref/check! (+ loc 2))))]
    [(struct-instance) (define fields-count (heap-ref/check! (+ 3 (heap-ref/check! (+ loc 1)))))
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
  (for ([i (in-range 2 (1st-gen-size))])
    (heap-set! i 'free))
  (heap-set! 0 2))

(define (1st-gen? loc)
  (and (>= loc 2)
       (< loc (1st-gen-size))))

(define (2nd-gen? loc)
  (and (>= loc (1st-gen-size))
       (< loc (2nd-gen-size))))

(define (heap-ref/check! loc)
  (unless (number? loc)
    (error 'heap-ref/check! "should be a location? at ~a" loc))
  (heap-ref loc))

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
