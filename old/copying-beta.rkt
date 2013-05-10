#lang plai/gc2/collector

(define (init-allocator)
  (heap-set! 0 4) ;; allocation pointer
  (heap-set! 1 'left) ;; active-semi-space : left -> left side, right -> right side
  (heap-set! 2 4) ;; gc queue start
  (heap-set! 3 4) ;; gc queue stop
  (for ([i (in-range 4 (heap-size))])
    (heap-set! i 'free)))

(define (gc:deref fl-loc)
  (case (heap-ref fl-loc)
    [(flat) (heap-ref (+ fl-loc 1))]
    [(frwd-flat) (gc:deref (heap-ref (+ fl-loc 1)))]
    [else (error 'gc:deref
                 "non-flat @ ~s"
                 fl-loc)]))

(define (gc:flat? loc)
  (case (heap-ref loc)
    [(flat frwd-flat) #t]
    [else #f]))

(define (gc:alloc-flat fv)
  (define ptr (alloc 2 #f #f))
  (heap-set! ptr 'flat)
  (heap-set! (+ ptr 1) fv)
  ptr)

(define (gc:cons hd tl)
  (define ptr (alloc 3 hd tl))
  (heap-set! ptr 'pair)
  (heap-set! (+ ptr 1) (track/loc hd))
  (heap-set! (+ ptr 2) (track/loc tl))
  (when (or (at-from-space? hd)
            (at-from-space? tl))
    (free-from-space))
  ptr)

(define (gc:cons? loc)
  (case (heap-ref loc)
    [(pair frwd-pair) #t]
    [else #f]))

(define (gc:first pr-loc)
  (if (equal? (heap-ref pr-loc) 'pair)
      (heap-ref (+ pr-loc 1))
      (error 'first "non cons @ ~s" pr-loc)))

(define (gc:rest pr-loc)
  (if (equal? (heap-ref pr-loc) 'pair)
      (heap-ref (+ pr-loc 2))
      (error 'first "non cons @ ~s" pr-loc)))

(define (gc:set-first! pr-loc new)
  (if (equal? (heap-ref pr-loc) 'pair)
      (heap-set! (+ pr-loc 1) new)
      (error 'set-first! "non cons")))

(define (gc:set-rest! pr-ptr new) 
  (if (equal? (heap-ref pr-ptr) 'pair)
      (heap-set! (+ pr-ptr 2) new)
      (error 'set-rest! "non cons")))

(define (gc:closure code-ptr free-vars)
  (define fv-count (vector-length free-vars))
  (define fv-vars (vector->roots free-vars))
  (define next (alloc (+ fv-count 3) fv-vars '()))
  (heap-set! next 'proc)
  (heap-set! (+ next 1) code-ptr)
  (heap-set! (+ next 2) fv-count)
  (for ([x (in-range 0 fv-count)])
    (heap-set! (+ next 3 x) 
               (track/loc (vector-ref free-vars x))))
  (when (ormap at-from-space? fv-vars)
    (free-from-space))
  next)

(define (gc:closure? loc)
  (case (heap-ref loc)
    [(proc frwd-proc) #t]
    [else #f]))

(define (gc:closure-code-ptr loc)
  (if (gc:closure? loc)
      (heap-ref (+ (track/loc loc) 1))
      (error 'gc:closure-code-ptr "non closure at ~a" loc)))

(define (gc:closure-env-ref loc i)
  (if (gc:closure? loc)
      (heap-ref (+ (track/loc loc) 3 i))
      (error 'gc:closure-env-ref "non closure at ~a" loc)))

(define (alloc n some-roots more-roots)
  (define next (heap-ref 0))
  (cond 
    [(<= (+ next n) (space-limit))
     (heap-set! 0 (+ next n))
     next]
    [else
     (collect-garbage some-roots more-roots)
     (define next (heap-ref 0))
     (unless (<= (+ next n) (space-limit))
       (error 'alloc "no space"))
     (heap-set! 0 (+ next n))
     (unless (or (at-from-space? some-roots)
                 (at-from-space? more-roots))
       (free-from-space))
     next]))

(define (collect-garbage some-roots more-roots)
  (switch-space)
  (forward/roots (get-root-set))
  (forward/roots some-roots)
  (forward/roots more-roots)
  (forward/queue))

;; switch from and to space
(define (switch-space)
  (case (heap-ref 1)
    [(left) (heap-set! 1 'right)]
    [(right) (heap-set! 1 'left)])
  (define start (space-start))
  (heap-set! 0 start)
  (heap-set! 2 start)
  (heap-set! 3 start))

;; free the to space
(define (free-to-space)
  (for ([i (in-range (space-start) (space-limit))])
       (heap-set! i 'free)))

;; free the from space
(define (free-from-space)
  (case (heap-ref 1)
    [(left)
     (for ([i (in-range (+ 2 (round (* (heap-size) 1/2))) (heap-size))])
          (heap-set! i 'free))]
    [(right)
     (for ([i (in-range 4 (+ 2 (round (* (heap-size) 1/2))))])
          (heap-set! i 'free))]))

;; forward/roots : roots -> void
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
;; move object from from-space to to-space
(define (forward/loc loc)
  (cond
    [(at-to-space? loc) loc]
    [else
      (case (heap-ref loc)
        [(flat) (define next (gc/alloc 2))
                (heap-set! next 'flat)
                (heap-set! (+ next 1) (heap-ref (+ loc 1))) 
                (heap-set! loc 'frwd-flat)
                (heap-set! (+ loc 1) next)
                (heap-set! 3 (+ 2 (heap-ref 3)))
                next]
        [(pair) (define next (gc/alloc 3))
                (heap-set! next 'pair)
                (heap-set! (+ next 1) (track/loc (heap-ref (+ loc 1))))
                (heap-set! (+ next 2) (track/loc (heap-ref (+ loc 2))))
                (heap-set! loc 'frwd-pair)
                (heap-set! (+ loc 1) next)
                (heap-set! 3 (+ 3 (heap-ref 3)))
                next]
        [(proc) (define closure-size (heap-ref (+ loc 2)))
                (define next (gc/alloc (+ 3 closure-size)))
                (for ([i (in-range 0 3)])
                     (heap-set! (+ next i) (heap-ref (+ loc i))))
                (for ([i (in-range 0 closure-size)])
                     (heap-set! (+ next 3 i) (track/loc (heap-ref (+ loc 3 i)))))
                (heap-set! loc 'frwd-proc)
                (heap-set! (+ loc 1) next)
                (heap-set! 3 (+ 3 closure-size (heap-ref 3)))
                next]
        [(frwd-flat frwd-pair frwd-proc) (heap-ref (+ loc 1))]
        [else (error 'forward/loc "wrong tag at ~a" loc)])]))

(define (forward/queue)
  (define loc (heap-ref 2))
  (cond
    [(= loc (heap-ref 3)) (void)]
    [else
      (case (heap-ref loc)
        [(flat) (heap-set! 2 (+ 2 (heap-ref 2)))
                (forward/queue)]
        [(pair) (heap-set! (+ loc 1) (forward/loc (heap-ref (+ loc 1))))
                (heap-set! (+ loc 2) (forward/loc (heap-ref (+ loc 2))))
                (heap-set! 2 (+ 3 (heap-ref 2)))
                (forward/queue)]
        [(proc) (define closure-size (heap-ref (+ loc 2)))
                (for ([i (in-range 0 closure-size)])
                     (heap-set! (+ loc 3 i) (forward/loc (heap-ref (+ loc 3 i)))))
                (heap-set! 2 (+ 3 closure-size (heap-ref 2)))
                (forward/queue)]
        [else (error 'forward/queue "wrong tag at ~a" loc)])]))

;; gc/alloc : num[size] -> loc
(define (gc/alloc n)
  (define next (heap-ref 0))
  (unless (<= (+ next n) (space-limit))
    (error 'gc/alloc "no space"))
  (heap-set! 0 (+ next n))
  next)

(define (track/loc loc)
  (case (heap-ref loc)
    [(flat pair proc) loc]
    [(frwd-flat frwd-pair frwd-proc) (heap-ref (+ loc 1))]
    [else (error 'track/loc "wrong tag at ~a" loc)]))

(define (at-to-space? loc)
  (and (>= loc (space-start))
       (< loc (space-limit))))

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

(define (space-limit) 
  (case (heap-ref 1)
    [(left) (+ 2 (round (* (heap-size) 1/2)))]
    [(right) (heap-size)]))

(define (space-start)
  (case (heap-ref 1)
    [(left) 4]
    [(right) (+ 2 (round (* (heap-size) 1/2)))]))

(print-only-errors #t)
(define test-heap1 (make-vector 12 'f))
;; init-allocator
(test (with-heap test-heap1
                 (init-allocator)
                 test-heap1)
      (vector 4 'left 4 4
              'free 'free 'free 'free
              'free 'free 'free 'free))
;; alloc
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (alloc 2 #f #f))
      6)
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (alloc 2 #f #f)
                 test-heap1)
      (vector 8 'left 4 4
              'flat 0 'free 'free
              'free 'free 'free 'free))
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:alloc-flat 0)
                 (alloc 2 #f #f))
      8)
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:alloc-flat 0)
                 (alloc 2 #f #f)
                 test-heap1)
      (vector 10 'right 8 8
              'free 'free 'free 'free
              'free 'free 'free 'free))
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (alloc 3 #f #f)
                 test-heap1)
      (vector 11 'right 8 8
              'free 'free 'free 'free
              'free 'free 'free 'free))
(test/exn (with-heap test-heap1
                     (init-allocator)
                     (gc:alloc-flat 0)
                     (alloc 3 4 4))
          "no space")
;; gc:alloc-flat
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 test-heap1)
      (vector 6 'left 4 4
              'flat 0 'free 'free
              'free 'free 'free 'free))
(let ([v (make-vector 16 'f)])
  (test (with-heap v
                   (init-allocator)
                   (gc:alloc-flat 0)
                   (gc:alloc-flat 0)
                   (gc:alloc-flat 0)
                   (gc:alloc-flat 0)
                   v)
        (vector 12 'right 10 10
                'free 'free 'free 'free
                'free 'free 'flat 0 
                'free 'free 'free 'free)))
;; gc:cons
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:cons 4 4)
                 test-heap1)
      (vector 7 'left 4 4
              'pair 4 4 'free
              'free 'free 'free 'free))
(test/exn (with-heap test-heap1
                     (init-allocator)
                     (gc:alloc-flat 0)
                     (gc:cons 4 4)
                     test-heap1)
          "no space")
(let ([v (make-vector 16 'f)])
  (test (with-heap v
                   (init-allocator)
                   (gc:alloc-flat 0)
                   (gc:alloc-flat 0)
                   (gc:alloc-flat 0)
                   (gc:cons 6 6)
                   v)
        (vector 15 'right 12 12
                'free 'free 'free 'free 
                'free 'free 'flat 0 
                'pair 10 10 'free)))
;; gc:closure
(test (with-heap test-heap1
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:closure 'f (vector))
                 test-heap1)
      (vector 11 'right 8 8
              'free 'free 'free 'free
              'proc 'f 0 'free))
(test/exn (with-heap test-heap1
                     (init-allocator)
                     (gc:alloc-flat 0)
                     (gc:closure 'f (vector 4)))
          "no space")
(let ([v (make-vector 16 'f)])
  (test (with-heap v
                   (init-allocator)
                   (gc:alloc-flat 0)
                   (gc:alloc-flat 0)
                   (gc:closure 'f (vector 4))
                   v)
        (vector 16 'right 12 12
                'free 'free 'free 'free
                'free 'free 'flat 0
                'proc 'f 1 10)))
;; combination
(define test-heap2 (make-vector 16 'f))
(test (with-heap test-heap2
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:alloc-flat 0)
                 (gc:closure 'f (vector))
                 (gc:alloc-flat 0)
                 test-heap2)
      (vector 15 'right 10 10
              'free 'free 'free 'free
              'free 'free 'proc 'f
              0 'flat 0 'free))
(test (with-heap test-heap2
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:alloc-flat 0)
                 (gc:closure 'f (vector))
                 (switch-space)
                 (forward/loc 10)
                 test-heap2)
      (vector 7 'left 4 7
              'proc 'f 0 'free
              'free 'free 'frwd-proc 4
              0 'free 'free 'free))
(test (with-heap test-heap2
                 (init-allocator)
                 (gc:alloc-flat 0)
                 (gc:alloc-flat 0)
                 (gc:closure 'f (vector))
                 (gc:alloc-flat 0)
                 (gc:cons 10 10)
                 test-heap2)
      (vector 10 'left 7 7
              'proc 'f 0 'pair
              4 4 'free 'free
              'free 'free 'free 'free))
