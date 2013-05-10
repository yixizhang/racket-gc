#lang plai/gc2/collector

(define (init-allocator)
  (for ([i (in-range 0 (heap-size))])
    (heap-set! i 'free)))

(define (gc:flat? loc)
  (equal? (heap-ref loc) 'flat))

(define (gc:deref loc)
  (cond
    [(equal? (heap-ref loc) 'flat)
     (heap-ref (+ loc 1))]
    [else
     (error 'gc:deref
            "non-flat @ ~s"
            loc)]))

(define (gc:cons? loc)
  (equal? (heap-ref loc) 'pair))

(define (gc:first pr-ptr)
  (if (equal? (heap-ref pr-ptr) 'pair)
      (heap-ref (+ pr-ptr 1))
      (error 'first "non pair @ ~s" pr-ptr)))

(define (gc:rest pr-ptr)
  (if (equal? (heap-ref pr-ptr) 'pair)
      (heap-ref (+ pr-ptr 2))
      (error 'rest "non pair @ ~s" pr-ptr)))

(define (gc:set-first! pr-ptr new)
  (if (equal? (heap-ref pr-ptr) 'pair)
      (heap-set! (+ pr-ptr 1) new)
      (error 'set-first! "non pair @ ~s" pr-ptr)))

(define (gc:set-rest! pr-ptr new)
  (if (equal? (heap-ref pr-ptr) 'pair)
      (heap-set! (+ pr-ptr 2) new)
      (error 'set-rest! "non pair @ ~s" pr-ptr)))

(define (gc:closure-code-ptr loc)
  (if (gc:closure? loc)
      (heap-ref (+ loc 1))
      (error 'gc:closure-code "non closure")))

(define (gc:closure-env-ref loc i)
  (if (gc:closure? loc)
      (heap-ref (+ loc 3 i))
      (error 'closure-env-ref "non closure")))

(define (gc:closure? loc)
  (equal? (heap-ref loc) 'proc))

(define (gc:alloc-flat fv)
  (define ptr (alloc 2 #f #f))
  (heap-set! ptr 'flat)
  (heap-set! (+ ptr 1) fv)
  ptr)

(define (gc:cons hd tl)
  (define ptr (alloc 3 hd tl))
  (heap-set! ptr 'pair)
  (heap-set! (+ ptr 1) hd)
  (heap-set! (+ ptr 2) tl)
  ptr)

(define (gc:closure code-ptr free-vars)
  (define fv-count (vector-length free-vars))
  (define next (alloc (+ fv-count 3)
                      (vector->roots free-vars)
                      '()))
  (heap-set! next 'proc)
  (heap-set! (+ next 1) code-ptr)
  (heap-set! (+ next 2) fv-count)
  (for ([x (in-range 0 fv-count)])
    (heap-set! (+ next 3 x)
               (vector-ref free-vars x)))
  next)

(define (alloc n some-roots more-roots)
  (define next (find-free-space 0 n))
  (cond
    [next next]
    [else 
     (collect-garbage some-roots more-roots)
     (define next (find-free-space 0 n))
     (unless next (error 'alloc "no space"))
     next]))

(define (find-free-space start size)
  (cond
    [(= start (heap-size)) #f]
    [else
     (case (heap-ref start)
       [(free) (if (n-free-blocks? start size)
                   start
                   (find-free-space (+ start 1) size))]
       [(flat) (find-free-space (+ start 2) size)]
       [(pair) (find-free-space (+ start 3) size)]
       [(proc)
        (find-free-space
         (+ start 3 (heap-ref (+ start 2)))
         size)]
       [else
        (error 'find-free-space "ack ~s" start)])]))

(define (n-free-blocks? start size)
  (cond
    [(= size 0) #t]
    [(= start (heap-size)) #f]
    [else 
     (and (eq? 'free (heap-ref start))
          (n-free-blocks? (+ start 1)
                          (- size 1)))]))

(define (collect-garbage some-roots more-roots)
  (mark-white! 0)
  (traverse/roots (get-root-set))
  (traverse/roots some-roots)
  (traverse/roots more-roots)
  (traverse/loc 0)
  (free-white! 0))

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
      [(free) (mark-white! (+ i 1))]
      [else
       (error 'mark-white! "unknown tag @ ~a" i)])))

(define (free-white! i)
  (when (< i (heap-size))
    (case (heap-ref i)
      [(pair) (free-white! (+ i 3))]
      [(flat) (free-white! (+ i 2))]
      [(proc) (free-white! (+ i 3 (heap-ref (+ i 2))))]
      [(white-pair) (heap-set! i 'free)
                    (heap-set! (+ i 1) 'free)
                    (heap-set! (+ i 2) 'free)
                    (free-white! (+ i 3))]
      [(white-flat) (heap-set! i 'free)
                    (heap-set! (+ i 1) 'free)
                    (free-white! (+ i 2))]
      [(white-proc) (define closure-size (heap-ref (+ i 2)))
                    (for ([dx (in-range 0 (+ closure-size 3))])
                      (heap-set! (+ i dx) 'free))
                    (free-white! (+ i 3 closure-size))]
      [(free) (free-white! (+ i 1))]
      [else (error 'free-white! "unknown tag ~s" (heap-ref i))])))

(define (traverse/roots thing)
  (cond
    [(list? thing)
     (for-each traverse/roots thing)]
    [(root? thing)
     (mark-grey (read-root thing))]
    [(number? thing)
     (mark-grey thing)]))

(define (traverse/loc loc)
  (cond
    [(= loc (heap-size)) (void)]
    [else
     (case (heap-ref loc)
       [(free) (traverse/loc (+ loc 1))]
       [(flat white-flat) (traverse/loc (+ loc 2))]
       [(pair white-pair) (traverse/loc (+ loc 3))]
       [(proc white-proc)
        (traverse/loc (+ loc 3 (heap-ref (+ loc 2))))]
       [(grey-flat grey-pair grey-proc) 
        (mark-black loc)
        (traverse/loc 0)]
       [else
        (error 'traverse/loc "ack ~s" loc)])]))

(define (mark-black loc)
  (case (heap-ref loc)
    [(grey-pair)
     (mark-grey (heap-ref (+ loc 1)))
     (mark-grey (heap-ref (+ loc 2)))
     (heap-set! loc 'pair)]
    [(grey-flat)
     (heap-set! loc 'flat)]
    [(grey-proc)
     (for ([i (in-range (heap-ref (+ loc 2)))])
       (mark-grey (heap-ref (+ loc 3 i))))
     (heap-set! loc 'proc)]
    [(pair flat proc) (void)]
    [else (error 'mark-grey "crash ~s" loc)]))

(define (mark-grey loc)
  (case (heap-ref loc)
    [(white-pair) (heap-set! loc 'grey-pair)]
    [(white-flat) (heap-set! loc 'grey-flat)]
    [(white-proc) (heap-set! loc 'grey-proc)]
    [(pair flat proc grey-pair grey-flat grey-proc) (void)]))
