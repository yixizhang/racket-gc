#lang plai/gc2/mutator
(allocator-setup "../../collector.rkt" 2048)
(require "html-structs.rkt"
         "html-spec.rkt"
         "sgml-reader.rkt"
         "xml-structures.rkt")
(import-primitives
 current-input-port)
(provide (all-from-out "html-structs.rkt")
         (all-defined-out))

;; compose : proc proc -> proc
(define (compose f g)
  (lambda (x)
    (f (g x))))

;; clean-up-pcdata : (listof Content) -> (listof Content)
;; Each pcdata inside a tag that isn't supposed to contain pcdata is either
;; a) appended to the end of the previous subelement, if that subelement may contain pcdata
;; b) prepended to the front of the next subelement, if that subelement may contain pcdata
;; c) discarded
;; unknown tags may contain pcdata
;; the top level may contain pcdata

(define local-clean-up-pcdata 'undefined)
(define local-eliminate-pcdata 'undefined)

(define clean-up-pcdata
  ;; clean-up-pcdata : (listof Content) -> (listof Content)
  (begin
    (set! local-clean-up-pcdata
      (lambda (content)
        (map (lambda (to-fix)
               (cond
                 [(element? to-fix)
                  (recontent-xml to-fix
                                 (let ([possible (may-contain (element-name to-fix))]
                                       [content (element-content to-fix)])
                                   (if (or (not possible) (memq 'pcdata possible))
                                     (local-clean-up-pcdata content)
                                     (local-eliminate-pcdata content))))]
                 [else to-fix]))
             content)))
    (set! local-eliminate-pcdata
      ;: (listof Content) -> (listof Content)
      (lambda (content)
        (let ([non-elements (first-non-elements content)]
              [more (memf element? content)])
          (if more
            (let ([el (first more)])
              (let ([possible (may-contain (element-name el))])
                (if (or (not possible) (memq 'pcdata possible))
                  (cons (recontent-xml el (append non-elements (local-clean-up-pcdata (element-content el)) (local-eliminate-pcdata (first-non-elements (rest more)))))
                        (or (memf element? (rest more)) empty))
                  (cons (recontent-xml el (local-eliminate-pcdata (element-content el)))
                        (local-eliminate-pcdata (rest more))))))
            empty))))
    local-clean-up-pcdata))

;; first-non-elements : (listof Content) -> (listof Content)
(define (first-non-elements content)
  (cond
    [(empty? content) empty]
    [else (if (element? (first content))
              empty
              (cons (first content) (first-non-elements (rest content))))]))

;; recontent-xml : Element (listof Content) -> Element
(define (recontent-xml e c)
  (make-element (source-start e) (source-stop e) (element-name e) (element-attributes e) c))

;; implicit-starts : Symbol Symbol -> (U #f Symbol)
(define (implicit-starts parent child)
  (or (and (eq? child 'tr) (eq? parent 'table) 'tbody)
      (and (eq? child 'td) (memq parent '(table tbody tfoot thead)) 'tr)))

;; may-contain : Kid-lister
(define may-contain
  (gen-may-contain html-spec))

(define may-contain-anything
  (gen-may-contain empty))

(define use-html-spec #t)

;; read-html-as-xml : [Input-port] -> (listof Content)
(define (read-html-as-xml port)
  ((if use-html-spec clean-up-pcdata values)
   (gen-read-sgml (if use-html-spec
                      may-contain 
                      may-contain-anything)
                  implicit-starts
                  (if port
                      port
                      (current-input-port)))))
