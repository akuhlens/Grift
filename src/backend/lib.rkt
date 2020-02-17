#lang racket/base
(require
 racket/file
 racket/path)
(provide (all-defined-out))

; string -> path
(define (find-unused-path suffix)
  (normalize-path (make-temporary-file (format "tmp~~a~a" suffix))))
