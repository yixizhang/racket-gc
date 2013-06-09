#lang racket

(require plai/gc2/private/gc-core)
(provide (all-defined-out))

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

;; cal-cycles : location? -> number?
(define (cal-cycles loc)
  ;; check l1
  (define l1-offset (l1/offset loc))
  (cond
    ;; hit
    [(= loc (vector-ref l1/cache l1-offset))
     1]
    ;; miss :-> check l2
    [else
      (define l2-offset (l2/offset loc))
      (cond
        ;; hit :-> overwrite l1/cache
        [(= loc (vector-ref l2/cache l2-offset))
         (vector-set! l1/cache l1-offset loc)
         10]
        ;; miss :-> overwrite l1/cache and l2/cache
        [else
          (vector-set! l1/cache l1-offset loc)
          (vector-set! l2/cache l2-offset loc)
          100])]))

(define (read/mem loc)
  (values (heap-ref loc) (cal-cycles loc)))

(define (write/mem loc thing)
  (heap-set! loc thing)
  (cal-cycles loc))
