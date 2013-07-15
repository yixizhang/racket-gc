#lang plai/gc2/mutator

(allocator-setup "../../collector.rkt" 10240)
(require "xml-structures.rkt"
         "util.rkt")

(provide read-html-comments
         trim-whitespace
         gen-may-contain
         gen-read-sgml)

(define (local-file-position in)
  (make-location 0 0 (file-position in)))

;; Start-tag ::= (make-start-tag Location Location Symbol (listof Attribute))
(define-struct (start-tag source) (name attrs))
;; End-tag ::= (make-end-tag Location Location Symbol)
(define-struct (end-tag source) (name))

;; Token ::= Contents | Start-tag | End-tag | Eof

;;(define read-html-comments (make-parameter #f))
(define read-html-comments #f)
;;(define trim-whitespace (make-parameter #f))
(define trim-whitespace #f)

;; gen-may-contain : Spec -> Kid-lister
(define (gen-may-contain spec)
  (let ([table (make-hash)]) ;; use assoc-table (listof pairs)
    (for-each (lambda (def)
                (let ([rhs (rest def)])
                  (for-each (lambda (name) (hash-set! table name rhs))
                            (first def))))
              spec)
    (lambda (name)
      (hash-ref table name (lambda () #f)))))

;; gen-read-sgml : Kid-lister (Symbol Symbol -> (U #f Symbol)) -> [Input-port] -> (listof Content)
(define (gen-read-sgml may-contain auto-insert in)
  (read-from-port may-contain auto-insert in))

;; read-from-port : Kid-lister (Symbol Symbol -> (U #f Symbol)) Input-port -> (listof Content)
(define (read-tokens in)
  (let ([tok (lex in)])
    (cond
      [(eof-object? tok) empty]
      [else (cons tok (read-tokens in))])))
(define (read-from-port-loop tokens may-contain auto-insert)
  (cond
    [(empty? tokens) empty]
    [else
     (let ([tok (first tokens)] [rest-tokens (rest tokens)])
       (cond
         [(start-tag? tok)
          (let-values ([(el more-tokens) (read-element tok empty may-contain auto-insert rest-tokens)])
            (cons el (read-from-port-loop more-tokens may-contain auto-insert)))]
         [(end-tag? tok) (read-from-port-loop rest-tokens may-contain auto-insert)]
         [else (let ([rest-contents (read-from-port-loop rest-tokens may-contain auto-insert)])
                 (expand-content tok rest-contents))]))]))
(define (read-from-port may-contain auto-insert in)
  (let ([tokens (read-tokens in)])
    (read-from-port-loop tokens may-contain auto-insert)))
;(define (read-from-port may-contain auto-insert in)
;  (let loop ([tokens (let read-tokens ()
;                       (let ([tok (lex in)])
;                         (cond
;                           [(eof-object? tok) null]
;                           [else (cons tok (read-tokens))])))])
;    (cond
;      [(null? tokens) null]
;      [else
;       (let ([tok (car tokens)] [rest-tokens (cdr tokens)])
;         (cond
;           [(start-tag? tok)
;            (let-values ([(el more-tokens) (read-element tok null may-contain auto-insert rest-tokens)])
;              (cons el (loop more-tokens)))]
;           [(end-tag? tok) (loop rest-tokens)]
;           [else (let ([rest-contents (loop rest-tokens)])
;                   (expand-content tok rest-contents))]))])))

;; read-element : Start-tag (listof Symbol) Kid-lister (Symbol Symbol -> (U #f Symbol)) (listof Token) -> Element (listof Token)
;; Note: How elements nest depends on their content model.
;;   If a kind of element can't contain anything, then its start tags are implicitly ended, and
;;   end tags are implicitly started.
;;   Unknown elements can contain anything and can go inside anything.
;;   Otherwise, only the subelements listed in the content model can go inside an element.
;; more here - may-contain shouldn't be used to decide if an element is known or not.
;;             The edgar dtd puts tags in may-contain's range that aren't in its domain.
;; more here (or not) - the (memq name context) test leaks for a worst case of O(n^2) in the
;;                      tag nesting depth.  However, this only should be a problem when the tag is there,
;;                      but far back.  That shouldn't happen often.  I'm guessing n will be about 3.
(define (read-content/helper tokens context may-contain auto-insert start-name ok-kids)
  (cond
    [(null? tokens) (values empty tokens)]
    [else
     (let ([tok (first tokens)] [next-tokens (rest tokens)])
       (cond
         [(start-tag? tok)
          (let* ([name (start-tag-name tok)]
                 [auto-start (auto-insert start-name name)])
            (if auto-start
                (read-content/helper (cons (make-start-tag (source-start tok) (source-stop tok) auto-start empty) tokens)
                                     context may-contain auto-insert start-name ok-kids)
                (if (and ok-kids
                         (not (memq name ok-kids))
                         (may-contain name))
                    (values empty tokens)
                    (let-values ([(element post-element)
                                  (read-el tok (cons name context) next-tokens)])
                      (let-values ([(more-contents left-overs) (read-content/helper post-element
                                                                                    context may-contain auto-insert start-name ok-kids)])
                        (values (cons element more-contents) left-overs))))))]
         [(end-tag? tok)
          (let ([name (end-tag-name tok)])
            (if (eq? name start-name)
                (values empty next-tokens)
                (if (memq name context)
                    (values empty tokens)
                    (read-content/helper next-tokens
                                         context may-contain auto-insert start-name ok-kids))))]
         [else 
          (let-values ([(more-contents left-overs) (read-content/helper next-tokens
                                                                        context may-contain auto-insert start-name ok-kids)])
            (values
             (expand-content tok more-contents)
             left-overs))]))]))
(define (read-el start-tag context may-contain auto-insert tokens)
  (let ([start-name (start-tag-name start-tag)])
    (let ([ok-kids (may-contain start-name)])
      (let-values ([(content remaining)
                    (cond
                      [(empty? ok-kids) (values empty tokens)]
                      [else (read-content/helper tokens context may-contain auto-insert start-name ok-kids)])])
        (values (make-element (source-start start-tag)
                              (source-stop start-tag)
                              start-name
                              (start-tag-attrs start-tag)
                              content)
                remaining)))))
(define (read-element start-tag context may-contain auto-insert tokens)
  (read-el start-tag context may-contain auto-insert tokens))

;; expand-content : Content (listof Content) -> (listof Content)
(define (expand-content x lst)
  (cond
    [(entity? x) (cons (expand-entity x) lst)]
    [(comment? x) (if (read-html-comments)
                      (cons x lst)
                      lst)]
    [else (cons x lst)]))

;; expand-entity : Entity -> (U Entity Pcdata)
;; more here - allow expansion of user defined entities
(define (expand-entity x)
  (let ([expanded (default-entity-table (entity-text x))])
    (if expanded
        (make-pcdata (source-start x) (source-stop x) expanded)
        x)))

;; default-entity-table : Symbol -> (U #f String)
(define (default-entity-table name)
  (case name
    [(amp) "&"]
    [(lt) "<"]
    [(gt) ">"]
    [(quot) "\""]
    [(apos) "'"]
    [else #f]))

;; lex : Input-port -> Token
(define (lex in)
  (when (trim-whitespace)
    (skip-space in))
  (let ([c (peek-char in)])
    (cond
      [(eof-object? c) c]
      [(eq? c #\&) (lex-entity in)]
      [(eq? c #\<) (lex-tag-cdata-pi-comment in)]
      [else (lex-pcdata in)])))

;; lex-entity : Input-port -> Token
;; This might not return an entity if it doesn't look like one afterall.
(define (lex-entity in)
  (let ([start (local-file-position in)])
    (read-char in)
    (case (peek-char in)
      ;; more here - read while it's numeric (or hex) not until #\;
      [(#\#)
       (begin
         (read-char in)
         (let* ([hex? (if (equal? #\x (peek-char in))
                          (and (read-char in) #t)
                          #f)]
                [str (read-until #\; in)]
                [n (cond
                     [hex?
                      (string->number str 16)]
                     [else (string->number str)])])
           (if (number? n)
               (make-entity start (local-file-position in) n)
               (make-pcdata start (local-file-position in) (string-append "&#" str)))))]
      [else
       (let ([name (lex-name/case-sensitive in)]
             [c (peek-char in)])
         (if (eq? c #\;)
             (begin (read-char in) (make-entity start (local-file-position in) name))
             (make-pcdata start (local-file-position in) (format "&~a" name))))])))

;; lex-tag-cdata-pi-comment : Input-port -> Start-tag | Element | End-tag | Pcdata | Pi | Comment
(define (lex-tag-cdata-pi-comment in)
  (let ([start (local-file-position in)])
    (read-char in)
    (case (peek-char in)
      [(#\!)
       (begin
         (read-char in)
         (case (peek-char in)
           [(#\-) (begin
                    (read-char in)
                    (let ([c (read-char in)])
                      (cond
                        [(eq? c #\-)
                         (let ([data (lex-comment-contents in)])
                           (make-comment data))]
                        [else (make-pcdata start (local-file-position in) (format "<!-~a" c))])))]
           [(#\[) (begin
                    (read-char in)
                    (let ([s (read-string 6 in)])
                      (if (string=? s "CDATA[")
                          (let ([data (lex-cdata-contents in)])
                            (make-pcdata start (local-file-position in) data))
                          (make-pcdata start (local-file-position in) (format "<[~a" s)))))]
           [else (skip-dtd in) (lex in)]))]
      [(#\?) (begin
               (read-char in)
               (let ([name (lex-name in)])
                 (skip-space in)
                 (let ([data (lex-pi-data in)])
                   (make-p-i start (local-file-position in) name data))))]
      [(#\/) (begin
               (read-char in)
               (let ([name (lex-name in)])
                 (skip-space in)
                 (read-char in) ;; skip #\> or whatever else is there
                 (make-end-tag start (local-file-position in) name)))]
      [else
       (let ([name (lex-name in)]
             [attrs (lex-attributes in)])
         (begin
           (skip-space in)
           (case (read-char in)
             [(#\/)
              (begin
                (read-char in) ;; skip #\> or something
                (make-element start (local-file-position in) name attrs empty))]
             [else (make-start-tag start (local-file-position in) name attrs)])))])))


;; lex-attributes : Input-port -> (listof Attribute)
(define (lex-attributes-loop in)
  (skip-space in)
  (cond [(name-start? (peek-char in))
         (cons (lex-attribute in) (lex-attributes-loop in))]
        [else empty]))
(define (lex-attributes in)
  (sort (lex-attributes-loop in)
        (lambda (a b)
          (string<? (symbol->string (attribute-name a))
                    (symbol->string (attribute-name b))))))

;; lex-attribute : Input-port -> Attribute
;; Note: entities in attributes are ignored, since defacto html uses & in them for URL syntax
(define (char-whitespace? c)
  (equal? c #\ ))
(define (read-more in delimiter)
  (let ([c (read-char in)])
    (cond
      [(or (eq? c delimiter) (eof-object? c)) empty]
      [else (cons c (read-more in delimiter))])))
(define (lex-attribute in)
  (let ([start (local-file-position in)]
        [name (lex-name in)])
    (skip-space in)
    (cond
      [(eq? (peek-char in) #\=)
       (begin
         (read-char in)
         (skip-space in)
         (let* ([delimiter (read-char in)]
                [value (list->string
                        (case delimiter
                          [(#\' #\") (read-more in delimiter)]
                          [else (cons delimiter (read-up-to (lambda (c) (or (char-whitespace? c) (eq? c #\>))) in))]))])
           (make-attribute start (local-file-position in) name value)))]
      [else (make-attribute start (local-file-position in) name (symbol->string name))])))

;; skip-space : Input-port -> Void
;; deviation - should sometimes insist on at least one space
(define (skip-space/helper in)
  (let ([c (peek-char in)])
    (when (and (not (eof-object? c)) (char-whitespace? c))
      (begin
        (read-char in)
        (skip-space/helper in)))))
(define (skip-space in)
  (skip-space/helper in))

;; lex-pcdata : Input-port -> Pcdata
;; deviation - disallow ]]> "for compatability" with SGML, sec 2.4 XML spec 
(define (lex-pcdata in)
  (let ([start (local-file-position in)])
    ;; The following regexp match must use bytes, not chars, because
    ;; `in' might not be a well-formed UTF-8 sequence. If it isn't,
    ;; and it goes wrong with the first byte sequence, then a char-based
    ;; pattern would match 0 characters. Meanwhile, the caller of this function
    ;; expects characters to be read.
    (let ([s (regexp-match #rx#"^[^&<]*" in)])
      (make-pcdata start
                   (local-file-position in)
                   (bytes->string/utf-8
                    (if (trim-whitespace)
                        (regexp-replace* #rx#"[ \t\v\r\n]+" (first s) #"")
                        (first s))
                    #\?)))))
#|
      ;; Original slow version:
      (define (lex-pcdata in)
	(let ([start (local-file-position in)]
	      [data (let loop ([c (read-char in)])
		      (let ([next (peek-char in)])
			(cond
			 [(or (eof-object? next) (eq? next #\&) (eq? next #\<))
			  (list c)]
			 [(and (char-whitespace? next) (trim-whitespace))
			  (skip-space in)
			  (let ([lst (loop #\space)])
			    (cond
			     [(null? (cdr lst)) (list c)]
			     [else (cons c lst)]))]
			 [else (cons c (loop (read-char in)))])))])
	  (make-pcdata start
		       (local-file-position in)
		       (list->string data))))
  |#


;; lex-name : Input-port -> Symbol
(define (lex-name in)
  (let ([s (bytes->string/utf-8 (first (regexp-match #rx"^[a-zA-Z_:0-9&.-]*" in)))])
    (string->symbol
     ;; Common case: string is already lowercased
     (if (regexp-match-positions #rx"[A-Z]" s)
         (string-downcase s)
         s))))
;; lex-name/case-sensitive : Input-port -> Symbol
(define (lex-name/case-sensitive in)
  (let ([s (bytes->string/utf-8 (first (regexp-match #rx"^[a-zA-Z_:0-9&.-]*" in)))])
    (string->symbol s)))
#|
      (define (lex-name in)
        (string->symbol
         (list->string
          (let lex-rest ()
            (cond
             [(name-char? (peek-char in))
              (cons (char-downcase (read-char in)) (lex-rest))]
             [else null])))))
|#


;; skip-dtd : Input-port -> Void
(define (skip-dtd/helper in)
  (let ([c (read-char in)])
    (if (eof-object? c)
        (void)
        (case c
          [(#\') (begin (read-until #\' in) (skip-dtd/helper in))]
          [(#\") (begin (read-until #\" in) (skip-dtd/helper in))]
          [(#\<)
           (case (read-char in)
             [(#\!) (case (read-char in)
                      [(#\-) (begin (read-char in) (lex-comment-contents in) (skip-dtd/helper in))]
                      [else (begin (skip-dtd/helper in) (skip-dtd/helper in))])]
             [(#\?) (begin (lex-pi-data in) (skip-dtd/helper in))]
             [else (begin (skip-dtd/helper in) (skip-dtd/helper in))])]
          [(#\>) (void)]
          [else (skip-dtd/helper in)]))))
(define (skip-dtd in)
  (skip-dtd/helper in))

;; name-start? : TST -> Bool
(define (name-start? ch)
  (and (char? ch) (char-name-start? ch)))

;; char-name-start? : Char -> Bool
(define (char-name-start? ch)
  (or (char-alphabetic? ch) 
      (eq? ch #\_)
      (eq? ch #\:)))

;; name-char? : TST -> Bool
(define (name-char? ch)
  (and (char? ch)
       (or (char-name-start? ch)
           (char-numeric? ch)
           (eq? ch #\&) ; ugly illegal junk for SEC's EDGAR database
           (eq? ch #\.)
           (eq? ch #\-))))

;; read-up-to : (Char -> Bool) Input-port -> (listof Char)
;; abstract this with read-until
(define (read-up-to/helper p? in)
  (let ([c (peek-char in)])
    (cond
      [(or (eof-object? c) (p? c)) empty]
      [else (cons (read-char in) (read-up-to/helper p? in))])))
(define (read-up-to p? in)
  (read-up-to/helper p? in))

;; read-until : Char Input-port -> String
;; discards the stop character, too
(define (read-until/helper char in)
  (let ([c (read-char in)])
    (cond
      [(or (eof-object? c) (eq? c char)) empty]
      [else (cons c (read-until/helper char in))])))
(define (read-until char in)
  (read-until/helper char in))

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
#|
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
|#

;; "-->" makes more sense, but "--" follows the spec, but this isn't XML anymore.
(define lex-comment-contents (gen-read-until-string "-->"))
(define lex-pi-data (gen-read-until-string "?>"))
(define lex-cdata-contents (gen-read-until-string "]]>"))
