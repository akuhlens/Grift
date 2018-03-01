#lang racket/base
#|------------------------------------------------------------------------------+
|pass: src/generate-sham
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu), Rajan Walia
+-------------------------------------------------------------------------------+
| Description:
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require "../helpers.rkt"
         "../errors.rkt"
         "../configuration.rkt"
         "../language/data1.rkt"
         "../macros.rkt"
         #;"runtime-location.rkt")

;; Only the pass is provided by this module
(provide generate-sham)

(define (generate-sham prog)
  (match-define (Prog (list n c t) (GlobDecs d* (Labels b* b)))
    prog)
  (define uc (make-unique-counter c))
  (define-values (main bnd*)
    (parameterize ([current-unique-counter uc])
      (let* ([body (gen-body b)]
             [bnd* (map gen-bnd-code b*)])
        (values body bnd*))))
  (sham:module (hash) (cons main bnd*)))


(define i64 (error 'todo))

;; Turn code bindings into module function defs
(define (gen-bnd-code bnd)
  (match-let ([(cons i (Code i* b)) bnd])
    ;;needs to produce defs
    (sham:def:function i*
                       (map (const i64) i*)
                       i64
                       (gen-body b))))

;; 
(define (gen-body body)  
  ;; body of gen-body
  (match-define (Locals i* t) body)
  (define t* (map (const i64) i*))
  (define v* (map (const v*) void))
  (sham:stmt:let i* t* v* (gen-tail t)))

  ;; tail -> sham:stmt
  (define (gen-tail tail)
    (define ret sham:stmt:return)
    (match tail
      [(If t c a)
       (sham:stmt:if (gen-pred t)
                     (gen-tail c)
                     (gen-tail a))]
      [(Switch e c* d)
       (error 'todo)
       (define-values (s* t) (trivialize-value e))
       (make-begin s*
                   (Switch t
                        (map-switch-case* gen-tail c*)
                        (gen-tail d)))]
      [(Begin e* v)
       (sham:stmt:block (gen-effect* e* (gen-tail v)))]
      [(App-Code e e*)
       (error 'todo "what is the correct forms for rator")
       (ret (sham:expr:app (gen-expr e) (gen-expr* e*)))]
      [(Op p (app gen-expr* e*))
       (ret (op->sham:expr p e*))]
      [(and e (or (Code-Label i)
                  (Var i)
                  (Quote d)
                  (Halt)
                  (Break-Repeat)
                  (Success)))
       (ret (gen-expr e))]
      [other (error 'remove-complex-opera "unmatched ~a" other)]))
  (: gen-pred (D2-Pred -> D3-Pred))
  (define (gen-pred pred)
    (match pred
      [(If t c a) (If (gen-pred t) (gen-pred c) (gen-pred a))]
      [(Switch e c* d)
       (define-values (s* t) (trivialize-value e))
       (make-begin s* (Switch t (map-switch-case* gen-pred c*) (gen-pred d)))]
      [(Begin e* v) (make-begin (gen-effect* e*) (gen-pred v))]
      [(Relop p v1 v2)
       (define-values (s*1 t1) (trivialize-value v1))
       (define-values (s*2 t2) (trivialize-value v2))
       (make-begin s*1 (make-begin s*2 (Relop p t1 t2)))]))
  (: gen-effect (D2-Effect -> D3-Effect))
  (define (gen-effect effect)
    (match effect
      ;; Is this line correct?
      [(Assign i v) (Assign i (gen-value v))]
      [(and h (Halt)) h]
      [(and h (Break-Repeat)) h]
      [(If t c a) (If (gen-pred t) (gen-effect c) (gen-effect a))]
      [(Switch e c* d)
       (define-values (s* t) (trivialize-value e))
       (define s (list (Switch t (map-switch-case* gen-effect c*) (gen-effect d))))
       (make-begin (append s* s) NO-OP)]
      [(Begin e* _) (make-begin (gen-effect* e*) NO-OP)]
      [(Repeat i v1 v2 #f #f e)
       (define-values (s*1 t1) (trivialize-value v1))
       (define-values (s*2 t2) (trivialize-value v2))
       (define u1 (local-next-uid! "tmp_gen"))
       (define u2 (local-next-uid! "tmp_gen"))
       (make-begin
        (append s*1
                s*2
                (list (Assign u1 t1)
                      (Assign u2 t2)
                      (Repeat i (Var u1) (Var u2) #f #f (gen-effect e))))
        NO-OP)]
      [(App-Code v v*)
       (define-values (s*1 t)  (trivialize-value v))
       (define-values (s*2 t*) (trivialize-value* v*))
       (make-begin (append s*1 s*2 (list (App-Code t t*))) NO-OP)]
      [(Op p v*)
       (define-values (s* t*) (trivialize-value* v*))
       (make-begin (append s* (list (Op p t*))) NO-OP)]
      [(No-Op) NO-OP]
      [other (error 'remove-complex-opera/effect "~a" other)]))

  ;; effect* sham:stmt* -> sham:stmt*
  (define (gen-effect* effect* [stmt* '()])
    (foldr (lambda (e s)
             (cons (gen-effect e) s)) stmt* effect*))

(define (gen-expr* e* [se* '()]) (foldr gen-expr se* e*))

(define (gen-expr val)
  (match val
    [(Begin e* v) (make-begin (gen-effect* e*) (gen-value v))]
    [(Quote (? char? c))
     (sham:expr:ui-value (char->integer c) i64)]
    [(Var u)
     (sham:expr:var (uid->symbol i))]
    ;; Help me does this work?
    [(Code-Label i)
     (sham:expr:var (uid->symbol i))]
    [(App-Code v v*) ;; Non-Tail Call
     ;; Purposely duplicate with Tail so that tail calls
     ;; can be marked
     (define r (gen-rator v))
     (sham:expr:app r (gen-expr* v*))
     (make-begin s*1 (make-begin s*2 (App-Code t t*)))]
    [(Op p v*)
     (define-values (s* t*) (trivialize-value* v*))
     (make-begin s* (Op p t*))]
    ;; Todo cases
    ;; let void and set! in each branch for if and switch
    [(Switch e c* d)
     (define i (next-uid!))
     (sham:expr:let '() '() '()
                    (error 'todo)
                    (sham:expr:var (uid->symbol i)))]
    [(If t c a) (If (gen-pred t) (gen-value c) (gen-value a))]
    [(Quote (? string? s))
     (error 'todo "figure out how to handle")]
    [(Halt)
     (error 'todo "figure out how to exit with error")]
    [(Break-Repeat)
     (sham:expr:stmt (error 'todo))]))

(define (gen-rator e)
  (error 'todo))

;; Return the sham operation that corresponds to an expr
(define (noop e) e)

(define (grift-type->sham-type s)
   (error 'todo))

(define ((runtime s type) e*)
  (unless (= (length e*) a)
    (error 'generate/sham "expected arg: ~a, recieved: ~a" e*))
  (sham:expr:app (sham:rator:external s)))
(define-syntax-rule (runtime* [op fn] ...)
  `((op . ,(runtime 'fn
                    (grift-type->sham-type
                     (grift-primitive->type 'op))))
    ...))

;; Define in sham/private/internal
(define (grift-type-arity t)
  (match t
    [(Fn a (and args (list _)) r)
     (unless (= a (length args))
       (error 'generate-sham "type arity corruption ~a" t))
     a]))

(define ((internal s a) e*)
  (unless (= (length e*) a)
    (error 'generate/sham "expected arg: ~a, recieved: ~a" e*))
  (sham:expr:app (sham:rator:symbol s)))

(define-syntax-rule (internal* [op inst] ...)
  `((op . ,(internal inst (grift-type-arity
                           (grift-primitive->type 'op))))
    ...))

;; LLVM intrinsics that are not provided by sham API
(define ((intrinsic s return-type a) e*)
  (unless (= (length e*) a)
    (error 'generate/sham "expected arg: ~a, recieved: ~a" e*))
  (sham:expr:app (sham:rator:intrinsic s return-type) e*))

(define-syntax-rule (bitcast* op ...)
  (error 'todo "lookup type to figure out cast operation"))

(define grift->sham-primitive-table
  (make-immutable-hash
   (append
    (bit-cast*
     float->int
     int->float
     read-float
     char->int
     int->char)
    (runtime*
     (print-char   print_ascii_char)
     (display-char display_ascii_char)
     (read-char    read_ascii_char))
    (internal*
     (*   mul)
     (+   add)
     (-   sub)
     (%/  sdiv)
     (quotient sdiv)
     (%%  srem))
     ;; TODO Check to see if this should be arithmetic or logical
     (%>> ashr)
     (%<< shl)
     (binary-and and)
     (binary-or  or)
     (binary-xor xor)
     (binary-not not)
     ;; Fixnum operations runtime
     (runtime*
      (read-int read_int)
      (print-int print_int)) 
     (internal*
      (<  icmp-lt)
      (<= icmp-sge)
      (=  icmp-eq)
      (>  icmp-slt)
      (>= icmp-sle))
     (internal*
      (fl+   . ,FLOATxFLOAT->FLOAT-TYPE)
      (fl-   . ,FLOATxFLOAT->FLOAT-TYPE)
      (fl*   . ,FLOATxFLOAT->FLOAT-TYPE)
      (fl/   . ,FLOATxFLOAT->FLOAT-TYPE)
      (flmodulo . ,FLOATxFLOAT->FLOAT-TYPE)
      (flmin . ,FLOATxFLOAT->FLOAT-TYPE)
      (flmax . ,FLOATxFLOAT->FLOAT-TYPE)
      (flabs . ,FLOAT->FLOAT-TYPE)
      (fl<   . ,FLOATxFLOAT->BOOL-TYPE)
      (fl<=  . ,FLOATxFLOAT->BOOL-TYPE)
      (fl=   . ,FLOATxFLOAT->BOOL-TYPE)
      (fl>=  . ,FLOATxFLOAT->BOOL-TYPE)
      (fl>   . ,FLOATxFLOAT->BOOL-TYPE)
      (flmin . ,FLOATxFLOAT->FLOAT-TYPE)
      (flmax . ,FLOATxFLOAT->FLOAT-TYPE)
      (flnegate . ,FLOAT->FLOAT-TYPE)
      (flround . ,FLOAT->FLOAT-TYPE)
      (flfloor . ,FLOAT->FLOAT-TYPE)
      (flceiling . ,FLOAT->FLOAT-TYPE)
      (fltruncate . ,FLOAT->FLOAT-TYPE)
      (flquotient . ,FLOATxFLOAT->INT-TYPE)
      ;; Float operations (trig)
      (flsin . ,FLOAT->FLOAT-TYPE)
      (flcos .  ,FLOAT->FLOAT-TYPE)
      (fltan .  ,FLOAT->FLOAT-TYPE)
      (flasin . ,FLOAT->FLOAT-TYPE)
      (flacos . ,FLOAT->FLOAT-TYPE)
      (flatan . ,FLOAT->FLOAT-TYPE)
      ;; Float operations (math)
      (fllog  . ,FLOAT->FLOAT-TYPE)
      (flexp  . ,FLOAT->FLOAT-TYPE)
      (flsqrt . ,FLOAT->FLOAT-TYPE)
      (flexpt . ,FLOATxFLOAT->FLOAT-TYPE))
     (runtime*
      (print-float . ,FLOATxINT->UNIT-TYPE) 
      (timer-start . ,->UNIT-TYPE)
      (timer-stop . ,->UNIT-TYPE)
      (timer-report . ,->UNIT-TYPE))
     ;;


     
     
     
     

     ;; Float operations
     
     )))

(define op-map
  (make-immutable-hash
   '()))



(define/match (gen-op op e*)
  ;; Needs to be turned into lets and ifs 
  (and . ,BOOLxBOOL->BOOL-TYPE)
  (or  . ,BOOLxBOOL->BOOL-TYPE)

  )
