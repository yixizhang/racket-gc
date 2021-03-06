#lang scheme
(require (prefix-in scheme: scheme)
         plai/private/command-line
         (for-syntax plai/private/command-line)
         plai/gc2/private/collector-exports
         plai/gc2/private/gc-core
         scheme/gui/dynamic
         (only-in plai/test-harness
                  exn:plai? equal~?
                  plai-error generic-test test halt-on-errors print-only-errors)
         (only-in racket/base
                  struct)
         (for-syntax scheme)
         (for-syntax plai/gc2/private/gc-transformer)
         scheme/stxparam
         (for-syntax scheme/stxparam-exptime))

(provide else require provide #%top all-defined-out all-from-out
         values
         test/location=? 
         test/value=?
         (rename-out
          [plai-error error]
          
          [mutator-void void]
          [mutator-and and]
          [mutator-or or]
          [mutator-cond cond]
          [mutator-case case]
          [mutator-define define]
          [mutator-define-values define-values]
          (mutator-let let)
          [mutator-let* let*]
          [mutator-begin begin]
          
          [mutator-if if]
          [mutator-when when]
          [mutator-let-values let-values]
          [mutator-set! set!]
          [mutator-lambda lambda]
          [mutator-lambda λ]
          (mutator-app #%app)
          (mutator-datum #%datum)
          (collector:cons cons)
          (collector:first car)
          (collector:first first)
          (collector:rest cdr)
          (collector:rest rest)
          
          (mutator-quote quote)
          (mutator-top-interaction #%top-interaction)
          (mutator-module-begin #%module-begin)
          (mutator-define-struct define-struct)))

(define-syntax-parameter mutator-name #f)
(define-syntax-parameter mutator-tail-call? #t)
(define-syntax-parameter mutator-env-roots empty)

; Sugar Macros
(define-syntax-rule (->address e) e)
(define (mutator-void)
  (collector:alloc-flat (void)))
(define-syntax mutator-and
  (syntax-rules ()
    [(_) (mutator-quote #t)]
    [(_ fe) fe]
    [(_ fe e ...) (mutator-if fe (mutator-and e ...) (mutator-quote #f))]))
(define-syntax mutator-or
  (syntax-rules ()
    [(_) (mutator-quote #f)]
    [(_ fe) fe]
    [(_ fe e ...) (mutator-let ([tmp fe]) (mutator-if tmp tmp (mutator-or e ...)))]))
(define-syntax mutator-cond
  (syntax-rules (else)
    [(_) (mutator-begin)]
    [(_ [else e ...]) (mutator-begin e ...)]
    [(_ [q ans] e ...) (mutator-if q ans (mutator-cond e ...))]))
(define-syntax mutator-case
  (syntax-rules (else)
    [(_ value
        [(v ...) e ...]
        ...
        [else ee ...])
     (mutator-let ([tmp value])
                  (mutator-cond [(mutator-app mutator-member? tmp (mutator-quote (v ...)))
                                 e ...]
                                ...
                                [else ee ...]))]
    [(_ value
        [(v ...) e ...]
        ...)
     (mutator-case value
                   [(v ...) e ...]
                   ...
                   [else (mutator-begin)])]))
(define-syntax mutator-define
  (syntax-rules ()
    [(_ (f a ...) e ...)
     (mutator-define-values (f) 
                            (syntax-parameterize ([mutator-name #'f])
                                                 (mutator-lambda (a ...) e ...)))]
    [(_ id e)
     (mutator-define-values (id) 
                            (syntax-parameterize ([mutator-name #'id])
                                                 e))]))
(define-syntax-rule (mutator-let ([id e] ...) be ...)
  (mutator-let-values ([(id) (syntax-parameterize ([mutator-name #'id])
                                                  e)]
                       ...)
                      be ...))
(define-syntax mutator-let*
  (syntax-rules ()
    [(_ () be ...)
     (mutator-begin be ...)]
    [(_ ([fid fe] [rid re] ...) be ...)
     (mutator-let ([fid fe])
                  (mutator-let* ([rid re] ...)
                                be ...))]))
(define-syntax mutator-begin
  (syntax-rules ()
    [(_) (mutator-app void)]
    [(_ e) e]
    [(_ fe e ...)
     (let ([tmp 
            (syntax-parameterize ([mutator-tail-call? #f])
                                 fe)])
       (mutator-begin e ...))]))

; Real Macros
(define-syntax-rule (mutator-define-values (id ...) e)
  (begin (define-values (id ...) 
           (syntax-parameterize ([mutator-tail-call? #f])
                                (->address e)))
         (add-global-root! (make-env-root id))
         ...))
(define-syntax-rule (mutator-if test true false)
  (if (syntax-parameterize ([mutator-tail-call? #f])
                           (gc->scheme (->address test)))
      (->address true)
      (->address false)))
(define-syntax-rule (mutator-when test true)
  (if (syntax-parameterize ([mutator-tail-call? #f])
                           (gc->scheme (->address test)))
      (->address true)
      (mutator-app void)))
(define-syntax-rule (mutator-set! id e)
  (begin
    (set! id (->address e))
    (mutator-app void)))
(define-syntax (mutator-let-values stx)
  (syntax-case stx ()
    [(_ ([(id ...) expr]
         ...)
        body-expr)
     (with-syntax ([((tmp ...) ...)
                    (map generate-temporaries (syntax->list #'((id ...) ...)))])
       (let ([binding-list (syntax->list #'((tmp ...) ...))])
         (with-syntax ([((previous-tmp ...) ...)
                        (build-list (length binding-list) 
                                    (λ (n) (append-map syntax->list (take binding-list n))))])
           (syntax/loc stx
             (let*-values ([(tmp ...) 
                            (syntax-parameterize ([mutator-env-roots 
                                                   (append
                                                    (find-referenced-locals
                                                     (list #'previous-tmp ...)
                                                     #'expr)
                                                    (syntax-parameter-value #'mutator-env-roots))]
                                                  [mutator-tail-call? #f])
                                                 expr)]
                           ...)
               (let-values ([(id ...) (values tmp ...)]
                            ...)
                 (syntax-parameterize ([mutator-env-roots 
                                        (append (find-referenced-locals
                                                 (list #'id ... ...)
                                                 #'body-expr)
                                                (syntax-parameter-value #'mutator-env-roots))])
                                      (->address body-expr))))))))]
    [(_ ([(id ...) expr]
         ...)
        body-expr ...)
     (syntax/loc stx
       (mutator-let-values
        ([(id ...) expr]
         ...)
        (mutator-begin body-expr ...)))]))
(define-syntax (mutator-lambda stx)
  (syntax-case stx ()
    [(_ (id ...) body)
     (let ([env-roots (syntax-parameter-value #'mutator-env-roots)])
       (with-syntax ([(free-id ...) (map syntax-local-introduce (find-referenced-locals env-roots stx))]
                     [(env-id ...) env-roots]
                     [closure (or (syntax-parameter-value #'mutator-name)
                                  (syntax-local-name)
                                  (let ([prop (syntax-property stx 'inferred-name)])
                                    (if (or (identifier? prop)
                                            (symbol? prop))
                                        prop
                                        #f))
                                  (string->symbol "#<proc>"))])
         (quasisyntax/loc stx
           (let ([closure 
                  (closure-code
                   #,(length (syntax->list #'(free-id ...)))
                   (let ([closure
                          (lambda (free-id ... id ...) 
                            (syntax-parameterize ([mutator-env-roots 
                                                   (append
                                                    (find-referenced-locals
                                                     (list #'id ...)
                                                     #'body)
                                                    (list #'free-id ...))]
                                                  [mutator-tail-call? #t])
                                                 (->address body)))])
                     closure))])
             #,(if (syntax-parameter-value #'mutator-tail-call?)
                   (syntax/loc stx
                     (#%app collector:closure closure (vector free-id ...)))
                   (syntax/loc stx
                     (with-continuation-mark 
                         gc-roots-key 
                       (list (make-env-root env-id) ...)
                       (#%app collector:closure closure (vector free-id ...)))))))))]
    [(_ (id ...) body ...)
     (syntax/loc stx
       (mutator-lambda (id ...) (mutator-begin body ...)))]))

(define-syntax (mutator-app stx)
  (syntax-case stx ()
    [(_ e ...)
     (local [(define (do-not-expand? exp)
               (and (identifier? exp)
                    (or (free-identifier=? exp #'empty)
                        (ormap (λ (x) (free-identifier=? x exp))
                               prim-ids))))
             (define exps (syntax->list #'(e ...)))
             (define tmps
               (generate-temporaries #'(e ...)))]
       (with-syntax ([(ne ...)
                      (map (lambda (exp tmp) (if (do-not-expand? exp) exp tmp))
                           exps tmps)])
         (for/fold ([acc (syntax/loc stx (mutator-anf-app ne ...))])
           ([exp (in-list (reverse exps))]
            [tmp (in-list (reverse tmps))])
           (if (do-not-expand? exp)
               acc
               (quasisyntax/loc stx
                 (mutator-let ([#,tmp #,exp])
                              #,acc))))))]))
(define-syntax (mutator-anf-app stx)
  (syntax-case stx ()
    [(_ fe ae ...)
     (let ()
       (define prim-app? (ormap (λ (x) (free-identifier=? x #'fe))
                                prim-ids))
       (with-syntax ([(env-id ...) (syntax-parameter-value #'mutator-env-roots)]
                     [app-exp (if prim-app?
                                  (syntax/loc stx (collector:alloc-flat (fe (collector:deref ae) ...)))
                                  (syntax/loc stx ((deref-proc fe) ae ...)))])
         (if (syntax-parameter-value #'mutator-tail-call?)
             ; If this call is in tail position, we will not need access
             ; to its environment when it returns.
             #'app-exp
             ; If this call is not in tail position, we make the
             ; environment at the call site reachable.
             #`(with-continuation-mark gc-roots-key 
                 (list (make-env-root env-id) ...)
                 app-exp))))]))
(define-syntax mutator-quote
  (syntax-rules ()
    [(_ (a . d))
     (mutator-app collector:cons (mutator-quote a) (mutator-quote d))]
    [(_ s) 
     (mutator-datum . s)]))
(define-syntax (mutator-datum stx)
  (syntax-case stx ()
    [(_ . e) 
     (quasisyntax/loc stx (mutator-anf-app collector:alloc-flat (#%datum . e)))]))

(define-syntax (mutator-top-interaction stx)
  (syntax-case stx (require provide mutator-define mutator-define-values test/value=? import-primitives mutator-define-struct)
    [(_ . (require . e))
     (syntax/loc stx
       (require . e))]
    [(_ . (provide . e))
     (syntax/loc stx
       (provide . e))]
    [(_ . (mutator-define . e))
     (syntax/loc stx
       (mutator-define . e))]
    [(_ . (mutator-define-values . e))
     (syntax/loc stx
       (mutator-define-values . e))]
    [(_ . (mutator-define-struct . e))
     (syntax/loc stx
       (mutator-define-struct . e))]
    [(_ . (test/value=? . e))
     (syntax/loc stx
       (test/value=? . e))]
    [(_ . (import-primitives . e))
     (syntax/loc stx
       (import-primitives . e))]
    [(_ . expr)
     (syntax/loc stx
       (call-with-values
        (lambda ()
          (syntax-parameterize ([mutator-tail-call? #f])
                               (->address expr)))
        (case-lambda
          [() (void)]
          [(result-addr)
           (show-one-result result-addr)]
          [result-addrs
           (show-multiple-results result-addrs)])))]))
(define (show-one-result result-addr)
  (cond
    [(procedure? result-addr)
     (printf "Imported procedure:\n")
     result-addr]
    [(location? result-addr)
     (printf "Value at location ~a:\n" result-addr)
     (gc->scheme result-addr)]))
(define (show-multiple-results results)
  (apply values
         (for/list ([result (in-list results)])
           (cond
             [(procedure? result)
              result]
             [(location? result)
              (gc->scheme result)]))))

;; define-struct : number symbol (listof symbol) -> mutator-define functions ...
(define-syntax (define-struct/offset stx)
  (syntax-case stx ()
    [(_ parent-fields parent-struct s (x ...))
     (let* ([local-fields (syntax->list #'(x ...))]
            [fields (append (syntax-e #'parent-fields) local-fields)]
            [fields-num (length fields)]
            [index-list (let* ([local-num (length local-fields)] [base (- fields-num local-num)])
                          (build-list local-num (λ (x) (+ x base))))])
       (with-syntax ([struct:s (datum->syntax #'s (string->symbol
                                                   (format "struct:~a" (syntax-e #'s))))]
                     [make-s (datum->syntax #'s (string->symbol (format "make-~a" (syntax-e #'s))))]
                     [s? (datum->syntax #'s (string->symbol (format "~a?" (syntax-e #'s))))]
                     [(s-f ...) (map (lambda (x)
                                       (datum->syntax #'s
                                                      (string->symbol (format "~a-~a"
                                                                              (syntax-e #'s)
                                                                              (syntax-e x)))))
                                     local-fields)]
                     [(set-s-f! ...) (map (lambda (x)
                                            (datum->syntax #'s
                                                           (string->symbol (format "set-~a-~s!"
                                                                                   (syntax-e #'s)
                                                                                   (syntax-e x)))))
                                          local-fields)])
         #`(begin
             (mutator-define struct:s 
                             (collector:alloc-struct 's parent-struct #,fields-num))
             (define (make-s #,@fields)
               (collector:alloc-struct-instance struct:s (vector #,@fields)))
             (define (s? a)
               (collector:alloc-flat (collector:struct-pred struct:s a)))
             #,@(for/list ([i index-list]
                           [f (in-list (syntax->list #'(s-f ...)))])
                  #`(define (#,f a)
                      (collector:struct-select struct:s a #,i)))
             #,@(for/list ([i index-list]
                           [f (in-list (syntax->list #'(set-s-f! ...)))])
                  #`(define (#,f a value)
                      (collector:struct-set! struct:s a #,i value))))))]))
(begin-for-syntax
  (define-struct define-struct-info (fields) #:prefab))
;; struct constructors
(define sc (make-hash))
(define-syntax (mk-s-macro stx)
  (syntax-case stx ()
    [(_ s (x ...) sc)
     #`(begin
         (struct s (x ...))
         (hash-set! sc (format "~a" s) s))]
    [(_ s t (x ...) sc)
     #`(begin
         (struct s t (x ...))
         (hash-set! sc (format "~a" s) s))]))
(define-syntax (mutator-define-struct stx)
  (syntax-case stx ()
    [(_ s (f ...))
     (identifier? #'s)
     (begin
       (for ([x (in-list (syntax->list #'(f ...)))])
         (unless (identifier? x)
           (raise-syntax-error 'define-struct "expected an identifier" stx x)))
       (with-syntax ([local-s (datum->syntax #'s (string->symbol (format "mutator:~a" (syntax-e #'s))))])
         #`(begin
             (mk-s-macro local-s (f ...) sc)
             (define-syntax s #,(make-define-struct-info (syntax->list #'(f ...))))
             (define-struct/offset #,(datum->syntax #f '()) #f s (f ...)))))]
    [(_ (s t) (f ...))
     (begin
       (unless (identifier? #'s)
         (raise-syntax-error 'define-struct "expected an identifier" stx #'s))
       (unless (and (identifier? #'t)
                    (define-struct-info? (syntax-local-value #'t (λ () #f))))
         (raise-syntax-error 'define-struct "expected an identifier from an earlier define-struct" stx #'t))
       (for ([x (in-list (syntax->list #'(f ...)))])
         (unless (identifier? x)
           (raise-syntax-error 'define-struct "expected an identifier" stx x)))
       (let ([parent-fields (define-struct-info-fields (syntax-local-value #'t))]
             [parent-struct (datum->syntax #'t (string->symbol
                                                (format "struct:~a" (syntax-e #'t))))])
         (with-syntax ([local-s (datum->syntax #'s (string->symbol (format "mutator:~a" (syntax-e #'s))))]
                       [local-t (datum->syntax #'t (string->symbol (format "mutator:~a" (syntax-e #'t))))])
           #`(begin
               (mk-s-macro local-s local-t (f ...) sc)
               (define-syntax s #,(make-define-struct-info (append parent-fields (syntax->list #'(f ...)))))
               (define-struct/offset #,parent-fields #,parent-struct s (f ...))))))]))

; Module Begin
(define-for-syntax (allocator-setup-internal stx)
  (syntax-case stx ()
    [(collector-module heap-size)
     (with-syntax ([(init-allocator gc:deref gc:alloc-flat gc:cons 
                                    gc:closure gc:closure? gc:closure-code-ptr gc:closure-env-ref
                                    gc:first gc:rest 
                                    gc:flat? gc:cons?
                                    gc:set-first! gc:set-rest!
                                    gc:vector gc:vector? 
                                    gc:vector-length gc:vector-ref gc:vector-set!
                                    gc:alloc-struct gc:struct? gc:alloc-struct-instance gc:struct-instance?
                                    gc:struct-pred gc:struct-select gc:struct-set!)
                    (map (λ (s) (datum->syntax stx s))
                         '(init-allocator gc:deref gc:alloc-flat gc:cons 
                                          gc:closure gc:closure? gc:closure-code-ptr gc:closure-env-ref
                                          gc:first gc:rest 
                                          gc:flat? gc:cons?
                                          gc:set-first! gc:set-rest!
                                          gc:vector gc:vector? 
                                          gc:vector-length gc:vector-ref gc:vector-set!
                                          gc:alloc-struct gc:struct? gc:alloc-struct-instance gc:struct-instance?
                                          gc:struct-pred gc:struct-select gc:struct-set!))]) 
       (begin
         #`(begin
             #,(if (alternate-collector)
                   #`(require #,(datum->syntax #'collector-module (alternate-collector)))
                   #`(require collector-module))
             
             (set-collector:deref! 'collector-module gc:deref)
             (set-collector:alloc-flat! 'collector-module gc:alloc-flat)
             (set-collector:cons! 'collector-module gc:cons)
             (set-collector:first! 'collector-module gc:first)
             (set-collector:rest! 'collector-module gc:rest)
             (set-collector:flat?! 'collector-module gc:flat?)
             (set-collector:cons?! 'collector-module gc:cons?)
             (set-collector:set-first!! 'collector-module gc:set-first!)
             (set-collector:set-rest!! 'collector-module gc:set-rest!)
             (set-collector:closure! 'collector-module gc:closure)
             (set-collector:closure?! 'collector-module gc:closure?)
             (set-collector:closure-code-ptr! 'collector-module gc:closure-code-ptr)
             (set-collector:closure-env-ref! 'collector-module gc:closure-env-ref)
             (set-collector:vector! 'collector-module gc:vector)
             (set-collector:vector?! 'collector-module gc:vector?)
             (set-collector:vector-length! 'collector-module gc:vector-length)
             (set-collector:vector-ref! 'collector-module gc:vector-ref)
             (set-collector:vector-set!! 'collector-module gc:vector-set!)
             (set-collector:alloc-struct! 'collector-module gc:alloc-struct)
             (set-collector:struct?! 'collector-module gc:struct?)
             (set-collector:alloc-struct-instance! 'collector-module gc:alloc-struct-instance)
             (set-collector:struct-instance?! 'collector-module gc:struct-instance?)
             (set-collector:struct-pred! 'collector-module gc:struct-pred)
             (set-collector:struct-select! 'collector-module gc:struct-select)
             (set-collector:struct-set!! 'collector-module gc:struct-set!)
             
             (init-heap! (#%datum . heap-size) init-allocator)
             (when (gui-available?) 
               (if (<= (#%datum . heap-size) 1024)
                   (set-ui! (dynamic-require `plai/gc2/private/gc-gui 'heap-viz%))
                   (printf "Large heap; the heap visualizer will not be displayed.\n"))))))]
    [_ (raise-syntax-error 'mutator 
                           "Mutator must start with an 'allocator-setup' expression, such as: (allocator-setup <module-path> <literal-number>)"
                           stx)]))

(define-for-syntax allocator-setup-error-msg
  "Mutator must start with an 'allocator-setup' expression, such as: (allocator-setup <module-path> <literal-number>)")

(define-syntax (mutator-module-begin stx)
  (syntax-case stx (allocator-setup)
    [(_ (allocator-setup . setup) module-expr ...)
     (begin
       (syntax-case #'setup ()
         [(collector heap-size)
          (begin
            (unless (module-path? (syntax->datum #'collector))
              (raise-syntax-error 'allocator-setup "expected a module path" #'collector))
            (unless (number? (syntax->datum #'heap-size))
              (raise-syntax-error 'allocator-setup "expected a literal number" #'heap-size)))]
         [_
          (raise-syntax-error 'mutator allocator-setup-error-msg (syntax/loc #'setup (allocator-setup . setup)))])
       (quasisyntax/loc stx
         (#%module-begin
          #,(allocator-setup-internal #'setup)
          #,@(for/list ([me (in-list (syntax->list #'(module-expr ...)))])
               (quasisyntax/loc me
                 (mutator-top-interaction . #,me))))))]
    [(_ first-expr module-expr ...)
     (raise-syntax-error 'mutator allocator-setup-error-msg #'first-expr)]
    [(_)
     (raise-syntax-error 'mutator allocator-setup-error-msg)]))

; User Macros
(provide import-primitives)
(define-syntax (import-primitives stx)
  (syntax-case stx ()
    [(_ id ...) 
     (andmap identifier? (syntax->list #'(id ...)))
     (with-syntax ([(renamed-id ...) (generate-temporaries #'(id ...))]
                   [source (syntax-local-get-shadower
                            (syntax-local-introduce #'scheme))])
       #`(begin
           (require (only-in source [id renamed-id] ...))
           ;; XXX make a macro to unify this and provide/lift
           (define id
             (lambda args
               (for ([arg (in-list args)])
                 (unless ((lambda (v) 
                            (or (and (location? v) 
                                     (or (collector:flat? v)
                                         (collector:cons? v)
                                         (collector:closure? v)
                                         (collector:vector? v)
                                         (collector:struct-instance? v)))
                                (procedure? v)))
                          arg)
                   (error 'id (string-append "all arguments must be <heap-value?>s, "
                                             "or structured data of <heap-value?>s, "
                                             "but got ~s\n") (heap-ref arg))))
               (let ([result (apply renamed-id 
                                    (map (λ (x) 
                                           (cond [(or (procedure? x) (collector:closure? x)) (deref-proc x)]
                                                 [(or (collector:flat? x) (collector:cons? x) (collector:vector? x) (collector:struct-instance? x))
                                                  (gc->scheme x)]))
                                         args))])
                 (cond
                   [(void? result) (void)]
                   ;; do-alloc or this section is buggy
                   [(or (heap-value? result)
                        (pair? result)
                        (vector? result))
                    (let ([locals (map (λ (x)
                                         (make-env-root x))
                                       (filter location? args))])
                      (add-extra-roots! locals)
                      (define loc (do-alloc result))
                      (remove-extra-roots! locals)
                      loc)]
                   [else 
                    (error 'id (string-append "imported primitive must return <heap-value?>, "
                                              "or structured data of <heap-valus?>s, "
                                              "received ~a" result))]))))
           ...))]
    [(_ maybe-id ...) 
     (ormap (λ (v) (and (not (identifier? v)) v)) (syntax->list #'(maybe-id ...)))
     (let ([offending-stx (findf (λ (v) (not (identifier? v))) (syntax->list #'(maybe-id ...)))])
       (raise-syntax-error 
        #f "expected identifier to import" offending-stx))]
    [(_ . __)
     (raise-syntax-error #f "expected list of identifiers to import" stx)]
    [_ (raise-syntax-error #f "expected open parenthesis before import-primitive")]))

(define-for-syntax ((mk-id-macro p-id) stx)
  (syntax-case stx ()
    [id
     (identifier? #'id)
     (raise-syntax-error (syntax-e stx)
                         "primitive must appear in the function position of an application"
                         stx)]
    [(id exp ...)
     #`(mutator-app #,p-id exp ...)]))

(define-syntax (provide-flat-prims/lift stx)
  (syntax-case stx ()
    [(_ prim-ids id ...)
     (andmap identifier? (syntax->list #'(id ...)))
     (with-syntax ([(id2 ...) (generate-temporaries #'(id ...))]
                   [(p ...) (generate-temporaries #'(id ...))])
       #'(begin
           (define-for-syntax prim-ids (syntax->list #'(id ...)))
           (provide (rename-out [id2 id] ...))
           (define-syntax id2 (mk-id-macro #'id)) ...))]))

(provide-flat-prims/lift
 prim-ids
 symbol? boolean? number? symbol=?
 add1 sub1 zero? exact? + - * / even? odd? = < > <= >=)

(define (member? v l)
  (and (member v l) #t))
(provide (rename-out (mutator-member? member?)))
(define (mutator-member? v l)
  (collector:alloc-flat
   (member? (gc->scheme v)
            (gc->scheme l))))

(provide (rename-out [mutator-sort sort]))
(define (mutator-sort seq less?)
  (define local-roots empty)
  (local [(define (mk-root loc)
            (define r (make-env-root loc))
            (add-extra-root! r)
            (set! local-roots (cons r local-roots))
            r)
          (define (alloc l)
            (cond
              [(null? l) (collector:alloc-flat empty)]
              [else (collector:cons (car l) (alloc (cdr l)))]))]
    (let ([result
           (alloc
            (sort (for/list ([i (copy-gc-pair seq)])
                    (mk-root i))
                  (lambda (a b)
                    (collector:deref 
                     ((deref-proc less?) (read-root a) 
                                         (read-root b))))))])
      (remove-extra-roots! local-roots)
      result)))

(provide (rename-out (mutator-make-vector make-vector)))
(define (mutator-make-vector length loc)
  (collector:vector (collector:deref length) 
                    loc))

(provide (rename-out (mutator-vector-length vector-length)))
(define (mutator-vector-length loc)
  (collector:alloc-flat (collector:vector-length loc)))

(provide (rename-out (mutator-vector-ref vector-ref)))
(define (mutator-vector-ref loc number)
  (collector:vector-ref loc 
                        (collector:deref number)))

(provide (rename-out (mutator-vector-set! vector-set!)))
(define (mutator-vector-set! loc number a-loc)
  (collector:vector-set! loc 
                         (collector:deref number)
                         a-loc)
  (void))

(provide (rename-out (mutator-set-first! set-first!)))
(define (mutator-set-first! x y)
  (collector:set-first! x y)
  (void))

(provide (rename-out (mutator-set-rest! set-rest!)))
(define (mutator-set-rest! x y)
  (collector:set-rest! x y)
  (void))

(provide (rename-out [mutator-empty empty]))
(define-syntax mutator-empty
  (syntax-id-rules (mutator-empty)
    [_ (mutator-quote ())]))

(provide (rename-out (mutator-empty? empty?)))
(provide (rename-out (mutator-empty? null?)))
(define (mutator-empty? loc)
  (cond
    [(collector:flat? loc) 
     (collector:alloc-flat (empty? (collector:deref loc)))]
    [else 
     (collector:alloc-flat false)]))

(provide (rename-out [mutator-cons? cons?]))
(define (mutator-cons? loc)
  (collector:alloc-flat (collector:cons? loc)))

(provide (rename-out [mutator-eq? eq?]))
(define (mutator-eq? l1 l2)
  (cond
    [(and (collector:flat? l1)
          (collector:flat? l2))
     (collector:alloc-flat
      (equal? (collector:deref l1)
              (collector:deref l2)))]
    [else (collector:alloc-flat (= l1 l2))]))

(provide (rename-out [mutator-equal? equal?]))
(define (mutator-equal? l1 l2)
  (collector:alloc-flat
   (or (= l1 l2)
       (equal? (gc->scheme l1) (gc->scheme l2)))))

(provide (rename-out [mutator-printf printf]))
(define-syntax (mutator-printf stx)
  (syntax-case stx ()
    [(_ fmt arg ...)
     ; We must invoke mutator-app to A-normalize the arguments.
     (syntax/loc stx 
       (begin
         (mutator-app printf (#%datum . fmt)
                      (mutator-app gc->scheme arg) ...)
         (void)))]))

(provide (rename-out
          (mutator-halt-on-errors halt-on-errors)
          (mutator-print-only-errors print-only-errors)))
(define-syntax (mutator-halt-on-errors stx)
  (syntax-case stx ()
    [(_) #'(halt-on-errors)]
    [(_ arg) #'(#%app halt-on-errors (#%datum . arg))]))

(define-syntax (mutator-print-only-errors stx)
  (syntax-case stx ()
    [(_) #'(print-only-errors)]
    [(_ arg) #'(#%app print-only-errors (#%datum . arg))]))

; Implementation Functions
(define (deref-proc proc/loc)
  (define v
    (cond
      [(procedure? proc/loc) proc/loc]
      [(location? proc/loc) (collector:closure-code-ptr proc/loc)]
      [else 
       (error 'procedure-application "expected procedure, given something else")]))
  (cond
    [(procedure? v)
     v]
    [(closure-code? v)
     (lambda args
       (apply (closure-code-proc v) 
              (append 
               (for/list ([i (in-range (closure-code-env-count v))])
                 (collector:closure-env-ref proc/loc i))
               args)))]
    [else
     (error 'procedure-application "expected procedure, given ~e" v)]))

(define (copy-gc-pair loc)
  (cond [(and (collector:flat? loc)
              (null? (collector:deref loc)))
         '()]
        [else (cons (collector:first loc)
                    (copy-gc-pair (collector:rest loc)))]))

(define (do-alloc value)
  ;; value supports flat, cons, and vector
  ;; any/c -> loc
  (define local-roots empty)
  (local [(define (mk-root loc)
            (define r (make-env-root loc))
            (add-extra-root! r)
            (set! local-roots (cons r local-roots))
            r)
          (define (alloc val)
            (cond [(heap-value? val) (mk-root (collector:alloc-flat val))]
                  [(pair? val) (mk-root (collector:cons (alloc (car val)) (alloc (cdr val))))]
                  [(vector? val) 
                   (let* ([vs (for/vector ([v (in-vector val)]) (alloc v))]
                          [vec (mk-root (collector:vector (vector-length val) (collector:alloc-flat #f)))])
                     (for ([v (in-vector vs)] [i (in-naturals)])
                       (collector:vector-set! (read-root vec)
                                              i 
                                              (if (location? v) v (read-root v))))
                     vec)]
                  [else (error 'do-alloc "expected flat, pair or vector values, but get ~s" val)]))]
    (let ([result (alloc value)])
                  (remove-extra-roots! local-roots)
                  (read-root result))))

(define (gc->scheme loc)
  (define-struct an-unset ())
  (define unset (make-an-unset))
  (define phs (make-hash))
  (define (mk-s s fields)
    (local [(define const (hash-ref sc
                                    (format "#<procedure:~a>" s)
                                    (λ () #f)))]
      (apply const fields)))
  (define (unwrap loc)
    (if (hash-has-key? phs loc)
        (hash-ref phs loc)
        (begin
          (local [(define ph (make-placeholder unset))]
            (hash-set! phs loc ph)
            (cond
              [(collector:flat? loc)
               (placeholder-set! ph (collector:deref loc))]
              [(collector:cons? loc)
               (local [(define car-ph (make-placeholder unset))
                       (define cdr-ph (make-placeholder unset))]
                 (placeholder-set! ph (cons car-ph cdr-ph))
                 (placeholder-set! car-ph (unwrap (collector:first loc)))
                 (placeholder-set! cdr-ph (unwrap (collector:rest loc))))]
              [(collector:closure? loc)
               ;; XXX get env?
               (placeholder-set! ph (closure-code-proc (collector:closure-code-ptr loc)))]
              [(collector:vector? loc)
               (local [(define vlen (collector:vector-length loc))
                       (define vec (make-vector vlen 0))]
                 (for ([i (in-range vlen)])
                   (local [(define vp (make-placeholder unset))]
                     (placeholder-set! vp (unwrap (collector:vector-ref loc i)))
                     (vector-set! vec i vp)))
                 (placeholder-set! ph vec))]
              [(collector:struct-instance? loc)
               (local [(define (struct-symbol loc)
                         (heap-ref (+ 1 (heap-ref (+ loc 1)))))
                       (define (struct-type loc)
                         (heap-ref (+ loc 1)))
                       (define (struct-fcount loc)
                         (heap-ref (+ 3 (heap-ref (+ loc 1)))))
                       (define (struct-fvals loc)
                         (let ([s (struct-type loc)])
                           (for/list ([i (in-range (struct-fcount loc))])
                             (begin
                               (define fp (make-placeholder unset))
                               (placeholder-set! fp (unwrap (collector:struct-select s loc i)))
                               fp))))]
                 (placeholder-set! ph (mk-s (format "mutator:~a" (struct-symbol loc)) (struct-fvals loc))))]
              [else 
               (error (format (string-append "gc:flat?, gc:cons?, gc:closure?, gc:vector? gc:struct-instance? "
                                             "all returned false for value ~s @ ~s")
                              (heap-ref loc) loc))])
            (placeholder-get ph)))))
  (make-reader-graph (unwrap loc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing support

(define-syntax (test/location=? stx)
  (syntax-case stx ()
    [(_ e1 e2)
     (quasisyntax/loc stx
       (generic-test 
        (λ () e1) 
        (λ (result-value)
          (define expected-val e2)
          (values
           (cond
             [(exn:plai? result-value) result-value]
             [(equal~? result-value expected-val) true]
             [else false])
           expected-val))
        (quote (heap-loc #,(syntax->datum #'e1)))
        (format "at line ~a" #,(syntax-line stx))))]))

(define-for-syntax (flat-heap-value? v)
  (or (number? v) (boolean? v)))

(define-syntax (expand-scheme stx)
  (syntax-case stx (mutator-quote mutator-datum)
    [(_ val) (flat-heap-value? (syntax->datum #'val)) #'(#%datum . val)]
    [(_ (mutator-datum . val))
     #'(#%datum . val)]
    [(_ (mutator-quote e))
     #'(quote e)]
    [_ 
     (raise-syntax-error 'test/value=? "must be a number, boolean or a quoted value" stx)]))

(define-syntax (test/value=? stx)
  (syntax-case stx (mutator-quote)
    [(_ mutator-expr scheme-datum)
     (quasisyntax/loc stx
       (generic-test 
        (λ () 
          (mutator-let ([v1 mutator-expr])
                       (gc->scheme v1))) 
        (λ (result-value)
          (define expected-val (expand-scheme scheme-datum))
          (values
           (cond
             [(exn:plai? result-value) result-value]
             [(equal~? result-value expected-val) true]
             [else false])
           expected-val))
        (quote #,(syntax->datum #'mutator-expr))
        (format "at line ~a" #,(syntax-line stx))))]))
