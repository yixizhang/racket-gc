#lang plai/gc2/collector

;; init-allocator : -> void
;; 2 pointers : allocation pointer
;;              active-semi-space pointer
(define (init-allocator)
  (heap-set! 0 2) ;; allocation pointer
  (heap-set! 1 'left) ;; active-semi-space : left -> left side, right -> right side
  (for ([i (in-range 2 (heap-size))])
    (heap-set! i 'free)))

(define (mid) 
  (+ 1 (round (* (heap-size) 1/2))))

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
    [(flat pair proc) loc]
    [(frwd) (heap-ref (+ loc 1))]
    [else (error 'track/loc "wrong tag at ~a" loc)]))

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
  (if (equal? (heap-ref pr-loc) 'pair)
      (heap-ref (+ pr-loc 1))
      (error 'first "non pair @ ~s" pr-loc)))

;; gc:rest : loc -> loc
;; must signal an error of pr-loc does not point to a pair
(define (gc:rest pr-loc)
  (if (equal? (heap-ref pr-loc) 'pair)
      (heap-ref (+ pr-loc 2))
      (error 'first "non pair @ ~s" pr-loc)))

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
  (if (equal? (heap-ref pr-loc) 'pair)
      (heap-set! (+ pr-loc 1) new)
      (error 'set-first! "non pair")))

;; gc:set-rest! : loc loc -> void
;; must signal an error of pr-loc does not point to a pair
(define (gc:set-rest! pr-ptr new) 
  (if (equal? (heap-ref pr-ptr) 'pair)
      (heap-set! (+ pr-ptr 2) new)
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
  (when (ormap at-from-space? fv-vars)
    (free-from-space))
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

;; alloc : number[size] roots roots -> loc
(define (alloc n some-roots more-roots)
  (define addr (heap-ref 0))
  (cond 
    [(<= (+ addr n) (space-limit))
     (heap-set! 0 (+ addr n))
     addr]
    [else
     (collect-garbage some-roots more-roots)
     (define next (heap-ref 0))
     (unless (<= (+ next n) (space-limit))
       (error 'alloc "no space"))
     (heap-set! 0 (+ next n))
     ;; free from space if no needed forward info lies there
     (unless (or (at-from-space? some-roots)
                 (at-from-space? more-roots))
       (free-from-space))
     next]))

;; collect-garbage : roots roots -> void
(define (collect-garbage some-roots more-roots)
  (change-active-semi-space)
  (forward/roots (get-root-set))
  (forward/roots some-roots)
  (forward/roots more-roots)
  (forward/ref (semi-space-start)))

;; space-limit -> integer 
;; find limit of current semi-heap
(define (space-limit)
  (if (equal? (heap-ref 1) 'left) (mid) (heap-size)))

;; semi-space-start
(define (semi-space-start)
  (if (equal? (heap-ref 1) 'left) 2 (mid)))

;; change-active-semi-space -> void
(define (change-active-semi-space)
  (case (heap-ref 1)
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
    [(at-to-space? loc) loc]
    [else
      (case (heap-ref loc)
        [(flat) (define new-addr (gc/alloc 2))
                (heap-set! new-addr 'flat)
                (heap-set! (+ new-addr 1) (heap-ref (+ loc 1))) 
                (heap-set! loc 'frwd)
                (heap-set! (+ loc 1) new-addr)
                new-addr]
        [(pair) (define new-addr (gc/alloc 3))
                (heap-set! new-addr 'pair)
                (heap-set! (+ new-addr 1) (track/loc (heap-ref (+ loc 1))))
                (heap-set! (+ new-addr 2) (track/loc (heap-ref (+ loc 2))))
                (heap-set! loc 'frwd)
                (heap-set! (+ loc 1) new-addr)
                new-addr]
        [(proc) (define length (+ 3 (heap-ref (+ loc 2))))
                (define new-addr (gc/alloc length))
                (for ([x (in-range 0 3)])
                     (heap-set! (+ new-addr x) (heap-ref (+ loc x))))
                (for ([x (in-range 3 length)])
                     (heap-set! (+ new-addr x) (track/loc (heap-ref (+ loc x)))))
                (heap-set! loc 'frwd)
                (heap-set! (+ loc 1) new-addr)
                new-addr]
        [(frwd) (heap-ref (+ loc 1))]
        [else (error 'forward/loc "wrong tag at ~a" loc)])]))

;; gc/alloc : num[size] -> loc
(define (gc/alloc n)
  (define addr (heap-ref 0))
  (unless (<= (+ addr n) (space-limit))
    (error 'gc/alloc "no space"))
  (heap-set! 0 (+ addr n))
  addr)

;; forward/ref : loc -> loc
;; move the referenced object to the other semi-space
;; and return the new addr of moved object
(define (forward/ref loc)
  (cond
    [(= loc (heap-ref 0)) (void)]
    [else
      (case (heap-ref loc)
        [(flat) (forward/ref (+ loc 2))]
        [(pair) (gc:set-first! loc (forward/loc (heap-ref (+ loc 1))))
                (gc:set-rest! loc (forward/loc (heap-ref (+ loc 2))))
                (forward/ref (+ loc 3))]
        [(proc) (define fv-count (heap-ref (+ loc 2)))
                (for ([x (in-range 0 fv-count)])
                     (define l (+ loc 3 x))
                     (heap-set! l (forward/loc (heap-ref l))))
                (forward/ref (+ loc 3 fv-count))]
        [else (error 'forward/ref "wrong tag at ~a" loc)])]))

;; free the from space 
;; after moved all live objects and their offsprings 
;; over to space
(define (free-from-space)
  (case (heap-ref 1)
    [(left)
     (for ([i (in-range (mid) (heap-size))])
       (heap-set! i 'free))]
    [(right)
     (for ([i (in-range 2 (mid))])
       (heap-set! i 'free))]))

(define (at-to-space? loc)
  (case (heap-ref 1)
    [(left) (and (>= loc 2)
                 (< loc (mid)))]
    [(right) (and (>= loc (mid))
                  (< loc (heap-size)))]))

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

(print-only-errors #t)
(define test-heap1 (make-vector 12 'f))
;; init-allocator
(test (with-heap test-heap1
                 (init-allocator)
                 test-heap1)
      (vector 2 'left 'free 'free
              'free 'free 'free 'free
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
      (vector 12 'right 'frwd 7
              'flat 0 'free 'flat
              0 'free 'free 'free))
;; gc:alloc-flat
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 test-heap1)
      (vector 4 'left 'flat 0
              'free 'free 'free 'free
              'free 'free 'free 'free))
(let ([v (make-vector 14 'f)])
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
                'free 'free)))
;; gc:cons
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:cons 2 2)
                 test-heap1)
      (vector 5 'left 'pair 2
              2 'free 'free 'free
              'free 'free 'free 'free))
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:cons 2 2)
                 test-heap1)
      (vector 7 'left 'flat 0
              'pair 2 2 'free
              'free 'free 'free 'free))
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:alloc-flat 0)
                 (gc:cons 2 2)
                 test-heap1)
      (vector 12 'right 'free 'free
              'free 'free 'free 'flat
              0 'pair 7 7))
(test/exn (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:alloc-flat 1)
                 (gc:cons 2 4))
          "no space")
(let ([v (make-vector 14 'f)])
  (test (with-heap v
                   (init-allocator)
                   (gc:alloc-flat 0)
                   (gc:alloc-flat 0)
                   (gc:alloc-flat 0)
                   (gc:cons 6 6)
                   v)
        (vector 13 'right 'free 'free 
                'free 'free 'free 'free 
                'flat 0 'pair 8 
                8 'free)))
;; gc:closure
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:closure 'f (vector))
                 test-heap1)
      (vector 7 'left 'flat 0
              'proc 'f 0 'free
              'free 'free 'free 'free))
(test/exn (with-heap test-heap1
                     (init-allocator)
                     (gc:alloc-flat 0)
                     (gc:closure 'f (vector 2)))
          "no space")
(let ([v (make-vector 14 'f)])
  (test (with-heap v
                   (init-allocator)
                   (gc:alloc-flat 0)
                   (gc:alloc-flat 0)
                   (gc:closure 'f (vector 2))
                   v)
        (vector 14 'right 'free 'free 
                'free 'free 'free 'free 
                'flat 0 'proc 'f 
                1 8)))
(define test-heap2 (make-vector 20 'f))
;; change-active-semi-space
(test (with-heap test-heap2
                 (init-allocator)
                 (change-active-semi-space)
                 test-heap2)
      (vector 11 'right 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free
              'free 'free 'free 'free))
;; forward/loc
(test (with-heap test-heap2
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (change-active-semi-space)
                 (forward/loc 2)
                 test-heap2)
      (vector 13 'right 'frwd 11 
              'free 'free 'free 'free 
              'free 'free 'free 'flat 
              0 'free 'free 'free
              'free 'free 'free 'free))
(test (with-heap test-heap2
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:cons 2 2)
                 (change-active-semi-space)
                 (forward/loc 4)
                 test-heap2)
      (vector 14 'right 'flat 0 
              'frwd 11 2 'free 
              'free 'free 'free 'pair 
              2 2 'free 'free
              'free 'free 'free 'free))
(test (with-heap test-heap2
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:closure 'f (vector 2))
                 (change-active-semi-space)
                 (forward/loc 4)
                 test-heap2)
      (vector 15 'right 'flat 0 
              'frwd '11 1 2 
              'free 'free 'free 'proc 
              'f 1 2 'free
              'free 'free 'free 'free))
;; forward/ref
(test (with-heap test-heap2
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (change-active-semi-space)
                 (forward/loc 2)
                 (forward/ref 11)
                 test-heap2)
      (vector 13 'right 'frwd 11 
              'free 'free 'free 'free 
              'free 'free 'free 'flat 
              0 'free 'free 'free
              'free 'free 'free 'free))
(test (with-heap test-heap2
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:cons 2 2)
                 (change-active-semi-space)
                 (forward/loc 4)
                 (forward/ref 11)
                 test-heap2)
      (vector 16 'right 'frwd 14 
              'frwd 11 2 'free 
              'free 'free 'free 'pair 
              14 14 'flat '0 
              'free 'free 'free 'free))
(test (with-heap test-heap2
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:closure 'f (vector 2))
                 (change-active-semi-space)
                 (forward/loc 4)
                 (forward/ref 11)
                 test-heap2)
      (vector 17 'right 'frwd 15
              'frwd '11 1 2 
              'free 'free 'free 'proc 
              'f 1 15 'flat
              0 'free 'free 'free))
;; free-from-space
(let ([test-heap (vector 11 'right 'flat 0
                         'flat 0 'free 'free
                         'free 'free 'free 'frwd
                         2 'frwd '4 'free
                         'free 'free 'free 'free)])
  (test (with-heap test-heap
                   (free-from-space)
                   test-heap)
        (vector 11 'right 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'frwd
                2 'frwd '4 'free
                'free 'free 'free 'free)))
(let ([test-heap (vector 2 'left 'frwd 11
                         'frwd 13 'free 'free
                         'free 'free 'free 'flat
                         0 'flat 0 'free
                         'free 'free 'free 'free)])
  (test (with-heap test-heap
                   (free-from-space)
                   test-heap)
        (vector 2 'left 'frwd 11
                'frwd 13 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free
                'free 'free 'free 'free)))
;; collect-garbage
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:alloc-flat 0)
                 (collect-garbage 2 2)
                 test-heap1)
      (vector 9 'right 'frwd 7
              'flat 0 'free 'flat
              0 'free 'free 'free))
(test (with-heap test-heap2
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:cons 2 2)
                 (collect-garbage #f #f)
                 test-heap2)
      (vector 11 'right 'flat 0 
              'pair 2 2 'free 
              'free 'free 'free 'free 
              'free 'free 'free 'free 
              'free 'free 'free 'free))
(test (with-heap test-heap2
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:cons 2 2)
                 (collect-garbage 2 #f)
                 test-heap2)
      (vector 13 'right 'frwd 11 
              'pair 2 2 'free 
              'free 'free 'free 'flat 
              0 'free 'free 'free 
              'free 'free 'free 'free))
(test (with-heap test-heap2
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:cons 2 2)
                 (collect-garbage #f 4)
                 test-heap2)
      (vector 16 'right 'frwd 14 
              'frwd 11 2 'free 
              'free 'free 'free 'pair 
              14 14 'flat 0 
              'free 'free 'free 'free))
(test (with-heap test-heap2
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:closure 'f (vector 2))
                 (collect-garbage #f 4)
                 test-heap2)
      (vector 17 'right 'frwd 15 
              'frwd 11 1 2 
              'free 'free 'free 'proc 
              'f 1 15 'flat 
              0 'free 'free 'free))
;; combination
(define test-heap3 (make-vector 14 'f))
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
              0 'free))
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
              'free 'free))
(test (with-heap test-heap3
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:alloc-flat 0)
                 (gc:closure 'f (vector))
                 (gc:alloc-flat 0)
                 (gc:cons 8 8)
                 test-heap3)
      (vector 8 'left 'proc 'f
              0 'pair 2 2
              'free 'free 'free 'free
              'free 'free))
