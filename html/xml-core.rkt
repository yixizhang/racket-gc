#lang plai/gc2/mutator

(allocator-setup "../incr-struct.rkt" 200)

;; Core structures needed for `xml/xexpr'

;;(provide (all-defined-out))
(provide permissive-xexprs
         source
         comment
         p-i
         pcdata
         cdata
         valid-char?)

; permissive-xexprs : parameter bool
;;(define permissive-xexprs (make-parameter #f))
(define permissive-xexprs #f)

; Source = (make-source Location Location)
;;(define-struct source (start stop) #:transparent)
(define-struct source (start stop))

; Comment = (make-comment String)
(define-struct comment (text))

; Processing-instruction = (make-p-i Location Location String String)
; also represents XMLDecl
(define-struct (p-i source) (target-name instruction))

; Pcdata = (make-pcdata Location Location String)
(define-struct (pcdata source) (string))

; Cdata = (make-cdata Location Location String)
(define-struct (cdata source) (string))

; Section 2.2 of XML 1.1
; (XML 1.0 is slightly different and looks less restrictive)
(define (valid-char? i)
  (and (exact-nonnegative-integer? i)
       (or (= i #x9)
           (= i #xA)
           (= i #xD)
           (<= #x20 i #xD7FF)
           (<= #xE000 i #xFFFD)
           (<= #x10000 i #x10FFFF))))
