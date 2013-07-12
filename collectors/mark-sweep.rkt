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
  (local [(define (->loc root/loc)
            (cond
              [(root? root/loc) (read-root root/loc)]
              [(number? root/loc) root/loc]
              [else (error 'gc:cons "non root or location arg ~s" root/loc)]))]
    (define ptr (alloc 3 hd tl))
    (define hd/loc (->loc hd))
    (define tl/loc (->loc tl))
    (heap-set! ptr 'pair)
    (heap-set! (+ ptr 1) hd/loc)
    (heap-set! (+ ptr 2) tl/loc)
    ptr))

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

;; vector related
(define (gc:vector length loc)
  (define next (alloc (+ length 2) loc #f))
  (heap-set! next 'vector)
  (heap-set! (+ next 1) length)
  (for ([i (in-range length)])
    (heap-set! (+ next 2 i) loc))
  next)

(define (gc:vector? loc)
  (equal? 'vector (heap-ref loc)))

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
                 "vector @ ~s index ~s out of range"
                 loc
                 number)]))

(define (gc:vector-set! loc number thing)
  (unless (gc:vector? loc)
    (error 'gc:vector-set! "non vector @ ~s" loc))
  (cond 
    [(< number (gc:vector-length loc)) 
     (heap-set! (+ loc 2 number) thing)]
    [else (error 'gc:vector-set! 
                 "vector @ ~s index ~s out of range"
                 loc
                 number)]))

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
  (define fv-roots (vector->roots fields-value))
  (define next (alloc (+ fv-count 2)
                      s
                      fv-roots))
  (heap-set! next 'struct-instance)
  (heap-set! (+ next 1) s)
  (for ([x (in-range 0 fv-count)])
    (heap-set! (+ next 2 x)
               (vector-ref fields-value x)))
  next)

(define (gc:struct-pred s instance)
  (local [(define (helper target s)
            (and s
                 (or (= target s)
                     (helper target (heap-ref (+ s 2))))))]
    (and (equal? 'struct (heap-ref s))
         (helper s (heap-ref (+ instance 1))))))

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
       [(vector)
        (find-free-space
         (+ start 2 (heap-ref (+ start 1)))
         size)]
       [(struct)
        (find-free-space (+ start 4) size)]
       [(struct-instance)
        (define fv-count (heap-ref (+ 3 (heap-ref (+ start 1)))))
        (find-free-space
         (+ start 2 fv-count)
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
      [(vector) (heap-set! i 'white-vector)
                (mark-white!
                 (+ i 2 (heap-ref (+ i 1))))]
      [(struct) (heap-set! i 'white-struct)
                (mark-white! (+ i 4))]
      [(struct-instance) (heap-set! i 'white-struct-instance)
                         (define fv-count (heap-ref (+ 3 (heap-ref (+ i 1)))))
                         (mark-white! (+ i 2 fv-count))]
      [(free) (mark-white! (+ i 1))]
      [else
       (error 'mark-white! "unknown tag @ ~a" i)])))

(define (free-white! i)
  (when (< i (heap-size))
    (case (heap-ref i)
      [(pair) (free-white! (+ i 3))]
      [(flat) (free-white! (+ i 2))]
      [(proc) (free-white! (+ i 3 (heap-ref (+ i 2))))]
      [(vector) (free-white! (+ i 2 (heap-ref (+ i 1))))]
      [(struct) (free-white! (+ i 4))]
      [(struct-instance) (define fv-count (heap-ref (+ 3 (heap-ref (+ i 1)))))
                         (free-white! (+ i 2 fv-count))]
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
      [(white-vector) (define length (heap-ref (+ i 1)))
                      (for ([dx (in-range (+ 2 length))])
                        (heap-set! (+ i dx) 'free))
                      (free-white! (+ i 2 length))]
      [(white-strut) (for ([dx (in-range 4)])
                       (heap-set! (+ i dx) 'free))
                     (free-white! (+ i 4))]
      [(white-struct-instance) (define fv-count (heap-ref (+ 3 (heap-ref (+ i 1)))))
                               (for ([dx (in-range (+ 2 fv-count))])
                                 (heap-set! (+ i dx) 'free))
                               (free-white! (+ i 2 fv-count))]
      [(free) (free-white! (+ i 1))]
      [else (error 'free-white! "unknown tag ~s" (heap-ref i))])))

(define (traverse/roots thing)
  (cond
    [(list? thing)
     (for-each traverse/roots thing)]
    [(root? thing)
     (traverse/loc (read-root thing))]
    [(number? thing)
     (traverse/loc thing)]))

(define (traverse/loc loc)
  (case (heap-ref loc)
    [(white-pair)
     (heap-set! loc 'pair)
     (traverse/loc (heap-ref (+ loc 1)))
     (traverse/loc (heap-ref (+ loc 2)))]
    [(white-flat)
     (heap-set! loc 'flat)]
    [(white-proc)
     (heap-set! loc 'proc)
     (for ([i (in-range (heap-ref (+ loc 2)))])
       (traverse/loc (heap-ref (+ loc 3 i))))]
    [(white-vector)
     (heap-set! loc 'vector)
     (for ([i (in-range (heap-ref (+ loc 1)))])
       (traverse/loc (heap-ref (+ loc 2 i))))]
    [(white-struct)
     (heap-set! loc 'struct)
     (define parent (heap-ref (+ loc 2)))
     (when parent
       (traverse/loc parent))]
    [(white-struct-instance)
     (heap-set! loc 'struct-instance)
     (define s (heap-ref (+ loc 1)))
     (traverse/loc s)
     (define fv-count (heap-ref (+ 3 s)))
     (for ([i (in-range fv-count)])
       (traverse/loc (heap-ref (+ loc 2 i))))]
    [(pair flat proc vector struct struct-instance) (void)]
    [else (error 'traverse/loc "crash ~s" loc)]))
