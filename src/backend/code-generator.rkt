#lang racket/base

(require 
 "../configuration.rkt"
 "./c/code-generator.rkt")

(provide backend-generate-code)

;; Basic driver for the entire backend
(define (backend-generate-code uil) 
  (c-backend-generate-code uil))
