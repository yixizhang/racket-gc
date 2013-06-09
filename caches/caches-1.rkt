#lang plai

(require plai/gc2/private/gc-core
         plai/gc2/collector)

;; according to https://www.cs.washington.edu/education/courses/351/10sp/lectures/13-memory.pdf
;; assume heap size is power of 256
;; block size 8

;; direct-mapped cache (E=1)
(define l1/cache (make-vector 16)) ;; 8*16=64
(define l2/cache (make-vector 64)) ;; 8*64=256
(define (l1/offset loc) (modulo (block/number loc) 16))
(define (l2/offset loc) (modulo (block/number loc) 64))
(define (block/number loc) (quotient loc 8))
(define (block/offset loc) (modulo loc 8))
(define-struct cache-block (offset slots))

;; read/mem : location? -> (any/c number?)
(define (read/mem loc)
  (define b/n (block/number loc))
  (define b/o (block/offset loc))
  ;; check l1
  (define l1-offset (l1/offset loc))
  (define cb/l1 (vector-ref l1/cache l1-offset))
  (cond
    ;; hit
    [(and (cache-block? cb/l1)
          (= b/n (cache-block-offset cb/l1)))
     (values (vector-ref (cache-block-slots cb/l1) 
                         b/o)
             1)]
    ;; miss -> check l2
    [else 
      (define l2-offset (l2/offset loc))
      (define cb/l2 (vector-ref l2/cache l2-offset))
      (cond
        ;; hit
        [(and (cache-block? cb/l2)
              (= b/n (cache-block-offset cb/l2)))
         (cache-overwrite l1/cache l1-offset cb/l2)
         (values (vector-ref (cache-block-slots cb/l2) 
                             b/o)
                 10)]
        ;; miss -> overwrite l1 & l2
        [else
         (define slots
           (for/vector ([i (in-range 0 8)])
             (heap-ref (+ (* b/n 8) i))))
         (define cb (make-cache-block b/n slots))
         (cache-overwrite l1/cache l1-offset cb)
         (cache-overwrite l2/cache l2-offset cb)
         (values (heap-ref loc) 100)])]))

;; write/mem : location? any/c -> number?
(define (write/mem loc thing)
  (define b/n (block/number loc))
  (define b/o (block/offset loc))
  ;; check l1
  (define l1-offset (l1/offset loc))
  (define cb/l1 (vector-ref l1/cache l1-offset))
  (cond
    ;; hit
    [(and (cache-block? cb/l1)
          (= b/n (cache-block-offset cb/l1)))
     (vector-set! (cache-block-slots cb/l1)
                  b/o
                  thing)
     1]
    ;; miss -> check l2
    [else
      (define l2-offset (l2/offset loc))
      (define cb/l2 (vector-ref l2/cache l2-offset))
      ;; hit
      (cond
        [(and (cache-block? cb/l2)
              (= b/n (cache-block-offset cb/l2)))
         (vector-set! (cache-block-slots cb/l2)
                      b/o
                      thing)
         (cache-overwrite l1/cache l1-offset cb/l2)
         10]
        ;; miss -> overwrite l1 & l2
        [else
         (define slots
           (for/vector ([i (in-range 0 8)])
             (if (= i b/o)
                 thing
                 (heap-ref (+ (* b/n 8) i)))))
         (define cb (make-cache-block b/n slots))
         (cache-overwrite l1/cache l1-offset cb)
         (cache-overwrite l2/cache l2-offset cb)
         100])]))

;; cache-overwrite : vector? number? cache-block? -> void?
(define (cache-overwrite cache offset block)
  (define b/n (cache-block-offset block))
  (define current-block (vector-ref cache offset))
  (when (and (cache-block? current-block)
             (not (= b/n (cache-block-offset current-block))))
    (define current-b/n (cache-block-offset current-block))
    (define current-slots (cache-block-slots current-block))
    (for ([i (in-range 0 8)]
          [v (in-vector current-slots)])
      (heap-set! (+ (* current-b/n 8) i) v)))
  (vector-set! cache offset block))

#|
;; testing
(print-only-errors #t)
(define test-heap (make-vector 512 'free))
(test (with-heap test-heap
                 (let-values ([(val cycles) (read/mem 0)])
                   (cons val cycles)))
      '(free . 100))
(test (with-heap test-heap
                 (write/mem 0 #f)
                 (let-values ([(val cycles) (read/mem 0)])
                   (cons val cycles)))
      '(#f . 1))
(test (with-heap test-heap
                 (write/mem 0 #f)
                 (heap-ref 0))
      'free)
(test (with-heap test-heap
                 (write/mem 0 #f)
                 (write/mem 256 #f)
                 (cons (heap-ref 0) (heap-ref 256)))
      '(#f . free))
(test (with-heap test-heap
                 (write/mem 0 #f)
                 (write/mem 256 #f)
                 (let-values ([(val0 c0) (read/mem 0)]
                              [(val256 c256) (read/mem 256)])
                   (cons val0 val256)))
      '(#f . #f))
|#