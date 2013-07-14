#lang plai/gc2/mutator
(allocator-setup "../collector.rkt" 1024)
;; helper functions
(define (null? thing) (empty? thing))
(define (not pred) (if pred #f #t))
(define-struct hash-table (vec))
(define-struct kv (key value))
(define (make-hash) (make-hash-table (make-vector 10 empty)))
;; set/append-it! : (listof kv) vector number key kv -> void
(define (set/append-it! lst vec index key kv)
  (cond
    [(null? lst)
     (vector-set! vec 
                  index 
                  (append! (vector-ref vec index) (cons kv empty)))]
    [else
     (let ([a-kv (first lst)])
       (if (equal? (kv-key a-kv) key)
           (set-first! lst kv)
           (set/append-it! (rest lst) vec index key kv)))]))
(define (hash-set! hash key value)
  (let* ([i (equal-hash-code key)]
         [vec (hash-table-vec hash)]
         [index (modulo i (vector-length vec))]
         [kv (make-kv key value)]
         [lst (vector-ref vec index)])
    (set/append-it! lst vec index key kv)))
(define (get-it lst key fail)
  (cond
    [(null? lst) (if (and (procedure? fail)
                          (procedure-arity-includes? fail 0))
                     (fail)
                     fail)]
    [else (let ([kv (first lst)])
            (if (equal? (kv-key kv) key)
                (kv-value kv)
                (get-it (rest lst))))]))
(define (hash-ref hash key fail)
  (let* ([i (equal-hash-code key)]
         [v (hash-table-vec hash)]
         [lst (vector-ref v (modulo i (vector-length v)))])
    (get-it lst key fail)))
(define (append some-list more-list)
  (cond
    [(null? some-list) more-list]
    [else
     (cons (first some-list)
           (append (rest some-list) more-list))]))
(define (list? l)
  (or (empty? l) (and (cons? l) (list? (rest l)))))
(define (last l)
  (cond
    [(cons? l)
     (cond
       [(cons? (rest l)) (last (rest l))]
       [else (rest l)])]
    [else (error 'last "expected a cons")]))
;; append! : list list -> list
(define (append! some-list more-list)
  (cond
    [(and (null? some-list) (list? more-list))
     more-list]
    [(and (list? some-list) (list? more-list))
     (append!-helper some-list some-list more-list)]
    [else
      (error 'append! "require list for both arguments")]))
(define (append!-helper orig-list some-list more-list)
  (cond
    [(null? (rest some-list))
     (begin
       (set-rest! some-list more-list)
       orig-list)]
    [else
     (append!-helper orig-list (rest some-list) more-list)]))

;; main
(define stats (make-hash))
(define (count ip)
  (let* ([c (read-char ip)]
         [n (hash-ref stats c (lambda ()
                                (begin
                                  (hash-set! stats c 0)
                                  (hash-ref stats c (lambda () (void))))))])
    (hash-set! stats c (+ 1 n))))
(count (open-input-string "abcdaadebbbbbcabccbdceaab"))