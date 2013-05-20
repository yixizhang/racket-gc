#lang racket
(require html)
(define in (open-input-file "basic.html"))
(read-html-as-xml in)