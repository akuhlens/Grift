#lang typed/racket/no-check

(require
 ffi/unsafe
 sham/ast
 sham/ir
 sham/env
 sham/llvm/ffi/all
 sham/llvm/llvm-config
 syntax/location
 "../../configuration.rkt"
 "./generate-sham.rkt"
 (for-syntax racket/system)
 (for-syntax "../runtime-location.rkt")
 "../runtime-location.rkt")

(provide
 generate-code)

(llvm-initialize-all)
(define bin-path (llvm-config "--bindir"))

;; Basic driver for the entire backend
(: generate-code (Data5-Lang . -> . Path))
(define (generate-code uil) 
  (let* ([keep-ll-code-file? (c-path)] ;;  TODO rename il-code-path
         [o-path (normalize-path (or (output-path) (build-path "a.out")))]
         [bc-path (path-replace-extension o-path ".bc")]
         [keep-s? (s-path)]
         [s-path (or keep-s? (path-replace-extension o-path ".s"))])

    ;; Empty LLVM State
    (define context (LLVMContextCreate))
    ;; Create The Sham IR
    (define sham-module (generate-sham uil))
    ;; Write LLVM bitcode to a file
    (unless (check-equal?
             0
             (LLVMWriteBitcodeToFile module (path->string bc-path)))
      (error 'grift/backend/sham/generate-code
             "failed to generate bitcode or write file"))

    (llc "-O3" bc-path "-o" s-path)

    ;; Link the executable
    (define rt? (or (runtime-path) runtime.o-path))
    (define flags c-flags)

    (parameterize ([s-path #f])
      (invoke-c-compiler s-path o-path))
    
    (unless keep-s?
      (delete-file s-path))
    
    o-path))



(module+ test
  (parameterize))
