#lang plai

;; gen-read-until-string : String -> Input-port -> String
;; uses Knuth-Morris-Pratt from
;; Introduction to Algorithms, Cormen, Leiserson, and Rivest, pages 869-876
;; discards stop from input
(define (gen-read-until-string stop)
  (let* ([len (string-length stop)]
         [prefix (make-vector len 0)]
         [fall-back
          (lambda (k c)
            (let ([k (let loop ([k k])
                       (cond
                         [(and (> k 0) (not (eq? (string-ref stop k) c)))
                          (loop (vector-ref prefix (sub1 k)))]
                         [else k]))])
              (if (eq? (string-ref stop k) c)
                  (add1 k)
                  k)))])
    (let init ([k 0] [q 1])
      (when (< q len)
        (let ([k (fall-back k (string-ref stop q))])
          (vector-set! prefix q k)
          (init k (add1 q)))))
    ;; (vector-ref prefix x) = the longest suffix that matches a prefix of stop
    (lambda (in)
      (list->string
       (let/ec out
         (let loop ([matched 0] [out out])
           (let* ([c (read-char in)]
                  [matched (fall-back matched c)])
             (cond
               [(or (eof-object? c) (= matched len)) (out null)]
               [(zero? matched) (cons c (let/ec out (loop matched out)))]
               [else (cons c (loop matched out))]))))))))

(define (gen-read-until-string1 stop)
  (let* ([len (string-length stop)]
         [prefix (make-vector len 0)]
         [fall-back
          (lambda (k c)
            (let ([k (let ([loop 47])
                       (begin
                         (set! loop
                               (lambda (kv)
                                 (cond
                                   [(and (> kv 0) (not (eq? (string-ref stop kv) c)))
                                    (loop (vector-ref prefix (sub1 kv)))]
                                   [else kv])))
                         (loop k)))])
              (if (eq? (string-ref stop k) c)
                  (add1 k)
                  k)))])
    (let ([init 47])
      (begin
        (set! init
              (lambda (k q)
                (when (< q len)
                  (let ([k (fall-back k (string-ref stop q))])
                    (vector-set! prefix q k)
                    (init k (add1 q))))))
        (init 0 1)))
    ;; (vector-ref prefix x) = the longest suffix that matches a prefix of stop
    (lambda (in)
      (list->string
       (let ([loop 47])
         (begin
           (set! loop
                 (lambda (matched)
                   (let ([c (read-char in)])
                     (let ([matched (fall-back matched c)])
                       (cond
                         [(or (eof-object? c) (= matched len)) null]
                         [(zero? matched) (cons c (loop matched))]
                         [else (let ([rest (loop matched)])
                                 (if (null? rest) null (cons c rest)))])))))
           (loop 0)))))))

;; read-functions
(define lex-comment-contents (gen-read-until-string "-->"))
(define lex-comment-contents1 (gen-read-until-string1 "-->"))

(define lex-pi-data (gen-read-until-string "?>"))
(define lex-pi-data1 (gen-read-until-string1 "?>"))

(define lex-cdata-contents (gen-read-until-string "]]>"))
(define lex-cdata-contents1 (gen-read-until-string1 "]]>"))

;; test
(print-only-errors #t)

(define (random-string)
  (apply string
         (for/list ([i (in-range 100)])
           (case (random 4)
             [(0) #\-]
             [(1) #\>]
             [(2) #\]]
             [(3) (integer->char (+ (random 26) 96))]))))

(for ([i (in-range 100)])
  (define s (random-string))
  (test (lex-comment-contents (open-input-string s))
        (lex-comment-contents1 (open-input-string s))))