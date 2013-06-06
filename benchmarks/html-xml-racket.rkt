#lang racket
(require html)
(with-handlers ([exn:fail?
                  (lambda (exn)
                    (printf "error: ~s" exn))])
               (call-with-input-file (vector-ref (current-command-line-arguments) 0)
                                     (lambda (port)
                                       (read-html-as-xml port))))
