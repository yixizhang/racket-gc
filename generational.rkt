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
    [(flat pair proc) loc]
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
  (when (or (at-from-space? hd)
            (at-from-space? tl))
    (free-from-space))
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

(define (table/alloc pointer target)
  (define next (heap-ref/check! (2nd-gen-size)))
  (cond
    [(>= (+ next 2) (heap-size))
     (error 'table/alloc "no space for indirection table")]
    [else
      (heap-set! next pointer)
      (heap-set! (+ next 1) target)
      (heap-set! (2nd-gen-size) (+ next 2))]))

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
  (when (at-from-space? fv-vars)
    (free-from-space))
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

;; alloc : number[size] roots roots -> loc
(define (alloc n some-roots more-roots)
  (define addr (heap-ref/check! 0))
  (cond 
    [(<= (+ addr n) (semi-space-limit))
     (heap-set! 0 (+ addr n))
     addr]
    [else
     (collect-garbage some-roots more-roots)
     (copy-live-to-older some-roots more-roots)
     (define next (heap-ref/check! 0))
     (unless (<= (+ next n) (semi-space-limit))
       (error 'alloc "no space"))
     (heap-set! 0 (+ next n))
     (unless (or (at-from-space? some-roots)
                 (at-from-space? more-roots))
       (free-from-space))
     next]))

;; copy live objects to older generation heap
;; if old generation is full when copying live objects over
;; trigger gc for old generation
;; reset to-space, e.g. alloc-pointer set to 2 | mid
(define (copy-live-to-older some-roots more-roots)
  (copy/roots (get-root-set))
  (copy/roots some-roots)
  (copy/roots more-roots)
  (case (heap-ref/check! 1)
    [(left)
     (copy/rest 2)
     (update/pointers (+ 1 (2nd-gen-size)))
     (for ([x (in-range 2 (mid))])
          (heap-set! x 'free))
     (heap-set! 0 2)]
    [(right)
     (copy/rest (mid))
     (update/pointers (+ 1 (2nd-gen-size)))
     (for ([x (in-range (mid) (1st-gen-size))])
          (heap-set! x 'free))
     (heap-set! 0 (mid))]))

(define (update/pointers loc)
  (cond
    [(or (= loc (heap-size))
         (equal? 'free (heap-ref/check! loc)))
     (define start (2nd-gen-size))
     (for ([i (in-range (+ 1 start) (heap-size))])
          (heap-set! i 'free))
     (heap-set! start (+ 1 start))]
    [else
      (define l (heap-ref/check! (+ loc 1)))
      (case (heap-ref/check! l)
        [(frwd)
         (heap-set! (heap-ref/check! loc)
                    (heap-ref/check! (+ l 1)))
         (update/pointers (+ loc 2))]
        [else (error 'update/pointers "wrong tag ~s at ~a" (heap-ref/check! loc) loc)])]))

(define (copy/roots thing)
  (cond
    [(list? thing)
     (for-each copy/roots thing)]
    [(root? thing)
     (set-root! thing (copy/loc (read-root thing)))]
    [(number? thing)
     (case (heap-ref/check! thing)
       [(frwd) 
        (define ptr (copy/loc (heap-ref/check! (+ 1 thing))))
        (unless (void? ptr) (heap-set! (+ 1 thing) ptr))]
       [(flat pair proc)
        (unless (2nd-gen? thing)
          (error 'copy/roots "~s at ~a shouldn't be roots" (heap-ref/check! thing) thing))
        thing]
       [else 
         (error 'copy/roots "wrong tag ~s at ~a when copying roots" (heap-ref/check! thing) thing)])]))

(define (copy/loc loc)
  (when (1st-gen? loc)
    (case (heap-ref/check! loc)
      [(frwd) (define l (heap-ref/check! (+ loc 1)))
              (unless (2nd-gen? l)
                (error 'copy/loc 
                       "copied object at ~a should be at older generation" 
                       l))
              l]
      [(flat) (define next (copy/alloc 2 #f #f))
              (heap-set! next 'flat)
              (heap-set! (+ next 1) (heap-ref/check! (+ loc 1)))
              (heap-set! loc 'frwd)
              (heap-set! (+ loc 1) next)
              next]
      [(pair) (define next (copy/alloc 3 
                                       (heap-ref/check! (+ loc 1))
                                       (heap-ref/check! (+ loc 2))))
              (heap-set! next 'pair)
              (heap-set! (+ next 1) (heap-ref/check! (+ loc 1)))
              (heap-set! (+ next 2) (heap-ref/check! (+ loc 2)))
              (heap-set! loc 'frwd)
              (heap-set! (+ loc 1) next)
              next]
      [(proc) (define fv-count (heap-ref/check! (+ loc 2)))
              (define free-vars (build-vector fv-count
                                              (lambda (i)
                                                (heap-ref/check! (+ loc 3 i)))))
              (define next (copy/alloc (+ 3 fv-count)
                                       (vector->roots free-vars)
                                       '()))
              (heap-set! next 'proc)
              (heap-set! (+ next 1) (heap-ref/check! (+ loc 1)))
              (heap-set! (+ next 2) (heap-ref/check! (+ loc 2)))
              (for ([x (in-range fv-count)])
                   (heap-set! (+ next 3 x) (heap-ref/check! (+ loc 3 x))))
              (heap-set! loc 'frwd)
              (heap-set! (+ loc 1) next)
              next]
      [else (error 'copy/loc "wrong tag at ~a" loc)])))

(define (copy/rest loc)
  (case (heap-ref/check! loc)
    [(frwd)
     (define l (heap-ref/check! (+ loc 1)))
     (case (heap-ref/check! l)
       [(flat) (copy/rest (+ loc 2))]
       [(pair) (copy/rest (+ loc 3))]
       [(proc) (copy/rest (+ loc 3 (heap-ref/check! (+ l 2))))])]
    [(flat) (copy/loc loc)
            (copy/rest (+ loc 2))]
    [(pair) (copy/loc loc)
            (copy/rest (+ loc 3))]
    [(proc) (copy/loc loc)
            (copy/rest (+ loc 3 (heap-ref/check! (+ loc 2))))]
    [(free) (void)]
    [else (error 'copy/rest "wrong tag at ~a" loc)]))

(define (copy/alloc n some-roots more-roots)
  (define next (find-free-space (1st-gen-size) n))
  (cond
    [next next]
    [else
      (2nd-gen-gc some-roots more-roots)
      (define next (find-free-space (1st-gen-size) n))
      (unless next (error 'copy/alloc "no space"))
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
  (make-pointers-to-2nd-gen-roots (semi-space-start))
  (free-white! start))

(define (make-pointers-to-2nd-gen-roots start)
  (case (heap-ref/check! start)
    [(flat) (make-pointers-to-2nd-gen-roots (+ start 2))]
    [(pair) (define loc (heap-ref/check! (+ start 1)))
            (when (2nd-gen? loc) (traverse/roots loc))
            (define loc (heap-ref/check! (+ start 2)))
            (when (2nd-gen? loc) (traverse/roots loc))
            (make-pointers-to-2nd-gen-roots (+ start 3))]
    [(proc) (define fv-counts (heap-ref/check! (+ start 2)))
            (for ([i (in-range fv-counts)])
                 (define loc (heap-ref/check! (+ start 3 i)))
                 (when (2nd-gen? loc) (traverse/roots loc)))
            (make-pointers-to-2nd-gen-roots (+ start 3 fv-counts))]
    [(free) (void)]
    [else (error 'make-pointers-to-2nd-gen-roots "wrong tag at ~a" start)]))

(define (mark-white! i)
  (when (< i (2nd-gen-size))
    (case (heap-ref/check! i)
      [(flat) (heap-set! i 'white-flat)
              (mark-white! (+ i 2))]
      [(pair) (heap-set! i 'white-pair)
              (mark-white! (+ i 3))]
      [(proc) (heap-set! i 'white-proc)
              (mark-white! (+ i 3 (heap-ref/check! (+ i 2))))]
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
      [(pair flat proc) (void)]
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
      [(free) (free-white! (+ i 1))]
      [else (error 'free-white! "wrong tag at ~a" i)])))

;; collect-garbage : roots roots -> void
(define (collect-garbage some-roots more-roots)
  (change-active-semi-space)
  (forward/roots (get-root-set))
  (forward/roots some-roots)
  (forward/roots more-roots)
  (forward/pointers (+ 1 (2nd-gen-size)))
  (forward/ref (semi-space-start)))

;; semi-space-limit -> integer 
;; find limit of current semi-heap
(define (semi-space-limit)
  (if (equal? (heap-ref/check! 1) 'left) (mid) (1st-gen-size)))

;; semi-space-start
(define (semi-space-start)
  (if (equal? (heap-ref/check! 1) 'left) 2 (mid)))

;; change-active-semi-space -> void
(define (change-active-semi-space)
  (case (heap-ref/check! 1)
    [(left)
     (heap-set! 1 'right)
     (heap-set! 0 (mid))]
    [(right)
     (heap-set! 1 'left)
     (heap-set! 0 2)]))

;; forward/roots : loc/(listof loc) -> loc
;; move every thing reachable from 'roots'
;; to the to space
(define (forward/roots thing)
  (cond
    [(list? thing)
     (for-each forward/roots thing)]
    [(root? thing)
     (set-root! thing (forward/loc (read-root thing)))]
    [(number? thing)
     (forward/loc thing)]))

;; forward/loc : loc -> loc
;; move object to the other semi-space
;; and return the new addr of moved object
(define (forward/loc loc)
  (cond
    [(and (1st-gen? loc)
          (not (at-to-space? loc)))
     (case (heap-ref/check! loc)
       [(flat) (define new-addr (gc/alloc 2))
               (heap-set! new-addr 'flat)
               (heap-set! (+ new-addr 1) (heap-ref/check! (+ loc 1))) 
               (heap-set! loc 'frwd)
               (heap-set! (+ loc 1) new-addr)
               new-addr]
       [(pair) (define new-addr (gc/alloc 3))
               (heap-set! new-addr 'pair)
               (heap-set! (+ new-addr 1) (track/loc (heap-ref/check! (+ loc 1))))
               (heap-set! (+ new-addr 2) (track/loc (heap-ref/check! (+ loc 2))))
               (heap-set! loc 'frwd)
               (heap-set! (+ loc 1) new-addr)
               new-addr]
       [(proc) (define length (+ 3 (heap-ref/check! (+ loc 2))))
               (define new-addr (gc/alloc length))
               (for ([x (in-range 0 3)])
                    (heap-set! (+ new-addr x) (heap-ref/check! (+ loc x))))
               (for ([x (in-range 3 length)])
                    (heap-set! (+ new-addr x) (track/loc (heap-ref/check! (+ loc x)))))
               (heap-set! loc 'frwd)
               (heap-set! (+ loc 1) new-addr)
               new-addr]
       [(frwd) (heap-ref/check! (+ loc 1))]
       [else (error 'forward/loc "wrong tag ~s at ~a" (heap-ref/check! loc) loc)])]
    [else loc]))

;; gc/alloc : num[size] -> loc
(define (gc/alloc n)
  (define addr (heap-ref/check! 0))
  (unless (<= (+ addr n) (semi-space-limit))
    (error 'gc/alloc "no space"))
  (heap-set! 0 (+ addr n))
  addr)

;; forward/ref : loc -> loc
;; move the referenced object to the other semi-space
;; and return the new addr of moved object
(define (forward/ref loc)
  (cond
    [(= loc (heap-ref/check! 0)) (void)]
    [else
      (case (heap-ref/check! loc)
        [(flat) (forward/ref (+ loc 2))]
        [(pair) (gc:set-first! loc (forward/loc (heap-ref/check! (+ loc 1))))
                (gc:set-rest! loc (forward/loc (heap-ref/check! (+ loc 2))))
                (forward/ref (+ loc 3))]
        [(proc) (define fv-count (heap-ref/check! (+ loc 2)))
                (for ([x (in-range 0 fv-count)])
                     (define l (+ loc 3 x))
                     (heap-set! l (forward/loc (heap-ref/check! l))))
                (forward/ref (+ loc 3 fv-count))]
        [else (error 'forward/ref "wrong tag at ~a" loc)])]))

(define (forward/pointers loc)
  (cond
    [(= loc (heap-size)) (void)]
    [(equal? 'free (heap-ref/check! loc)) (void)]
    [else
      (heap-set! (+ loc 1) (forward/loc (heap-ref/check! (+ loc 1))))
      (forward/pointers (+ loc 2))]))

;; free the from space 
;; after moved all live objects and their offsprings 
;; over to space
(define (free-from-space)
  (case (heap-ref/check! 1)
    [(left)
     (for ([i (in-range (mid) (1st-gen-size))])
       (heap-set! i 'free))]
    [(right)
     (for ([i (in-range 2 (mid))])
       (heap-set! i 'free))]))

(define (at-to-space? loc)
  (case (heap-ref/check! 1)
    [(left) (and (>= loc 2)
                 (< loc (mid)))]
    [(right) (and (>= loc (mid))
                  (< loc (1st-gen-size)))]))

(define (at-from-space? thing)
  (cond
    [(list? thing)
     (ormap at-from-space? thing)]
    [(root? thing)
     (not (at-to-space? (read-root thing)))]
    [(number? thing)
     (not (at-to-space? thing))]
    [(not thing)
     thing]))

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
