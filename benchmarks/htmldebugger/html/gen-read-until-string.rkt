#lang plai/gc2/mutator
(allocator-setup "../../collector.rkt" 2048)
(import-primitives
 not random integer->char open-input-string
 string string-ref string-length list->string string-append
 read-char peek-char eof-object?)

;; gen-read-until-string : String -> Input-port -> String
;; uses Knuth-Morris-Pratt from
;; Introduction to Algorithms, Cormen, Leiserson, and Rivest, pages 869-876
;; discards stop from input
(define (fall-back/loop kv stop prefix c)
  (cond
    [(and (> kv 0) (not (eq? (string-ref stop kv) c)))
     (fall-back/loop (vector-ref prefix (sub1 kv)) stop prefix c)]
    [else kv]))
(define (fall-back k c stop prefix)
  (let ([k (fall-back/loop k stop prefix c)])
    (if (eq? (string-ref stop k) c)
        (add1 k)
        k)))
(define (init/helper k q stop prefix len)
  (when (< q len)
    (let ([k (fall-back k (string-ref stop q) stop prefix)])
      (vector-set! prefix q k)
      (init/helper k (add1 q) stop prefix len))))
(define (loop/helper matched in stop prefix len)
  (let ([c (read-char in)])
    (let ([matched (fall-back matched c stop prefix)])
      (cond
        [(or (eof-object? c) (= matched len)) empty]
        [(zero? matched) (cons c (loop/helper matched in stop prefix len))]
        [else (let ([rest (loop/helper matched in stop prefix len)])
                (if (empty? rest)
                    empty
                    (cons c rest)))]))))
(define (gen-read-until-string stop)
  (let* ([len (string-length stop)]
         [prefix (make-vector len 0)])
    (init/helper 0 1 stop prefix len)
    ;; (vector-ref prefix x) = the longest suffix that matches a prefix of stop
    (lambda (in)
      (list->string
       (loop/helper 0 in stop prefix len)))))

;; read-functions
(define lex-comment-contents (gen-read-until-string "-->"))

;; test
(define (reverse l)
  (reverse1 l empty))
(define (reverse1 a l)
  (if (null? l)
      a
      (reverse1 (cons (first l) a) (rest l))))
(define (for-each f l)
  (if (empty? l)
      (void)
      (begin (f (first l))
             (for-each f (rest l)))))
(define (for/list n f result)
  (if (zero? n)
      (reverse result)
      (for/list (sub1 n) f (cons (f) result))))
(define (map f l)
  (if (null? l)
      empty
      (cons (f (first l)) (map f (rest l)))))
(define (foldr f init l)
  (if (empty? l)
      init
      (f (first l) (foldr f init (rest l)))))
;(for/list 40
;  (lambda ()
;    (case (random 4)
;      [(0) #\-]
;      [(1) #\>]
;      [(2) #\]]
;      [(3) (integer->char (+ (random 26) 96))]))
;  empty)
(define (random-string)
  (foldr string-append
         ""
         (map string
              (for/list 20
                (lambda ()
                  (case (random 4)
                    [(0) #\-]
                    [(1) #\>]
                    [(2) #\]]
                    [(3) (integer->char (+ (random 26) 96))]))
                empty))))
(let ([s (random-string)])
  (printf "~s\n" s)
  (lex-comment-contents (open-input-string s)))
