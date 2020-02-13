#lang typed/racket/no-check
#|------------------------------------------------------------------------------+
|pass: src/generate-sham
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: This pass generates two components one is the custom c-code that
| initializes the runtime correctly and contains the main function for the
| the program. The second is a sham IR which represents the user program.
| The main function calls into the sham IR.
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require
 "../../unique-identifiers.rkt"
 "../../language/primitives.rkt"
 "../../errors.rkt"
 "../../casts/cast-profiler.rkt"
 "../../configuration.rkt"
 "../../language/forms.rkt"
 "../../macros.rkt"
 "../runtime-location.rkt"
 sham/ast)

(module+ test
  (require rackunit))

;; Only the pass is provided by this module
(provide generate-sham)

(define (generate-sham prgm out-path)
  (match-let ([(Prog (list name count type) (GlobDecs d* expr)) prgm])
    (define (dglob d) (dglobal #f (uid->symbol d) grift-obj-type))
    (define defs (box (map dglob d*)))
    (define stmt (generate-program defs stmt))
    (dmodule
     (hash)
     'grift_module
     (cons (dfunction #f 'grift_main '() '() tvoid stmt)
           (unbox defs)))))


;; TODO: don't assume 64bit target
(define grift-obj-type i64)

(define (generate-program lifted-code* stmt)  

  (define (lift-code! x)
    (set-box! lifted-code* (cons x (unbox lifted-code*))))
  
  (define (generate-bnd-code* bnd*)
    (for ([b bnd*])
      (match-define (cons u (Code u* t)) b)
      (lift-code!
       (dfunction
        #f
        (uid->symbol u)
        (map uid->symbol u*)
        (for/list ([_ u*]) grift-obj-type)
        grift-obj-type
        (generate-tail t)))))
  
  (define (generate-repeat index start stop acc init body
                           #:return? [return? #f]
                           #:value?  [value? #f])
    (define index-id (uid->symbol index))
    (define index-var (var index-id))
    (define stop-id (gensym 'stop))
    (define stop-var (var stop-id))
    (define acc-id (uid->symbol acc))
    (define acc-var (var acc-id))
    (sham:ast:expr:let
     (list index-id stop-id acc-id)
     (list grift-obj-type grift-obj-type grift-obj-type)
     (list (generate-value start) (generate-value stop) (generate-value init))
     (block^
      (while^
       (icmp-slt index-var stop-var)
       (set!^ acc-var (generate-value body))
       (set!^ index-var (add index-var (si 1))))
      (if return? (return acc-var) (svoid)))
     (if value? acc-var (evoid))))

  (define (generate-constant k)
    (cond
      [(inexact-real? k) (fp->ui (cfl k))]
      [(exact-integer? k) (intcast (csi k))]
      [(char? k)
       (define i (char->integer k))
       (if (<= 0 i 255)
           (intcast (cui i))
           (error 'generate-c/quote-char "currently only supports ASCII"))]
      [(string? k) (cstring k)]))
  
  ;;; Make sure the expression is in tail position
  (define (generate-tail exp)
    (match exp
      [(Labels bndc* exp)
       (generate-bnd-code* bndc*)
       (generate-tail exp)]
      [(Let (list (cons id* rhs*) ...) tail)
       (sham:ast:stmt:expr
        (sham:ast:expr:let
         (map uid->symbol id*)
         (for/list ((_ id*)) grift-obj-type)
         (map generate-value rhs*)
         (generate-tail tail)
         (evoid)))]
      [(If test consequence alternative)
       (sham:ast:stmt:if
        (generate-value test)
        (generate-tail consequence)
        (generate-tail alternative))]
      [(Begin eff* exp)
       (block^ (block (map generate-effect eff*)) (generate-tail exp))]
      [(Repeat i e1 e2 a e3 e4)
       (sham:ast:stmt:expr
        (generate-repeat #:return? #t i e1 e2 a e3 e4))]
      [(App-Code exp exp*)
       (return (app (generate-value exp) (map generate-value exp*)))]
      [(Op p exp*)
       (return (generate-value-op p (map generate-value exp*)))]
      [(Var u) (return (var (uid->symbol u)))]
      [(Global u) (return (global (uid->symbol u)))]
      [(Stack-Alloc _) (error 'todo)]
      [(Quote k) (return (generate-constant k))]
      [(Halt) (error 'todo)]
      [(Switch _ _ _) (error 'todo)]
      [(and other
            (or
             (While _ _)
             (Assign _ _)
             (No-Op)))
       (error 'generate-tail "invalid return value")]))

  (define (generate-value exp)
    (match exp
      [(Labels bndc* exp)
       (generate-bnd-code* bndc*)
       (generate-value exp)]
      [(Let (list (cons id* rhs*) ...) exp)
       (sham:ast:expr:let
        (map uid->symbol id*)
        (for/list ((_ id*)) grift-obj-type)
         (map generate-value rhs*)
         '()
         (generate-value exp))]
      [(If t c a)
       (define result (gensym 'if-result))
       (sham:ast:expr:let
        (list result)
        (list grift-obj-type)
        (list (si 0))
        (list (if^ (generate-value t)
                   (set!^ (var result) (generate-value c))
                   (set!^ (var result) (generate-value a))))
        (var result))]
      [(Begin eff* exp)
       (sham:ast:expr:let
        '()
        '()
        '()
        (map generate-effect eff*)
        (generate-value exp))]
      [(Repeat i e1 e2 a e3 e4)
       (generate-repeat #:value? #t i e1 e2 a e3 e4)]
      [(Break-Repeat)
       (sham:ast:expr:let '() '() '() (break) (evoid))]
      [(App-Code exp exp*)
       (app (generate-value exp) (map generate-value exp*))]
      [(Op p e*)
       (generate-value-op p (map generate-value e*))]
      [(Stack-Alloc n)
       (arr-alloca (mul (cui n) (sizeof grift-obj-type)))]
      [(Var i)
       (var i)]
      [(Global s)
       (global s)]
      [(Code-Label i)
       (ri (uid->symbol i))]
      [(Quote k) (generate-constant k)]
      [(Halt) (error 'todo)]
      [(or
        (While _ _)
        (Assign _ _)
        (Stack-Alloc _)
        (No-Op))
       (error 'generate-value "Invalid Value")]
      [(Switch _ _ _) (error 'todo "switch value")]))

  (define (generate-effect exp)
    (match exp
      [(Assign u rhs)
       (set!^ (var u) (generate-value rhs))]
      [(Labels bndc* exp)
       (generate-bnd-code* bndc*)
       (generate-effect exp)]
      [(Let (list (cons id* rhs*) ...) body)
       (sham:ast:stmt:expr
        (sham:ast:expr:let
         (map uid->symbol id*)
         (for/list ((_ id*)) grift-obj-type)
         (map generate-value rhs*)
         (generate-effect body)
         (evoid)))]
      [(If test consequence alternative)
       (if^ (generate-value test)
            (generate-effect consequence)
            (generate-effect alternative))]
      
      [(Begin eff* exp)
       (block^ (map map generate-effect eff*) (generate-effect exp))]
      [(Repeat i e1 e2 a e3 e4)
       (sham:ast:stmt:expr
        (generate-repeat i e1 e2 a e3 e4))]
      [(While e1 e2)
       (while (generate-value e1)
         (generate-effect e2))]
      [(Break-Repeat) (break)]
      [(App-Code exp exp*)
       (app (generate-value exp) (map generate-value exp*))]
      [(Op p exp*)
       (if (grift-primitive-effect? p)
           (Op p (map generate-value exp*))
           ;; evaluate values for their effects
           (block (map generate-effect exp*)))]
      [(or
        (Stack-Alloc _)
        (Var _)
        (Global _)
        (Code-Label _)
        (Quote _)
        (No-Op))
       (svoid)]
      [(Switch e c* d) (error 'todo)]))

  (generate-effect stmt))

(define float-type (sham:ast:expr:etype f64))
(define imdt-type (sham:ast:expr:etype i64))
(define int-type (sham:ast:expr:etype i64))
(define bool-type (sham:ast:expr:etype i1))
(define assoc-stack-type
  (sham:ast:expr:etype
   (sham:ast:type:pointer i64)))

(define (float->imdt v)
  (bitcast v imdt))
(define (imdt->float v)
  (bitcast v float))
(define (imdt->int v) v)
(define (int->imdt v) v)

;; truncate to integer
(define (imdt->float->int v)
  (fp->si (imdt->float v) v))

;; extend to floating point
(define (imdt->int->float v)
  (si->fp (imdt->int v) float))

(define (imdt->grift-obj v) v)
(define (grift-obj->imdt v) v)
(define (imdt->assoc-stack v)
  (bitcast v assoc-stack-type))
(define (assoc-stack->imdt th)
  (bitcast v assoc-stack-type))

(define (bool->imdt v)
  (intcast v imdt-type))
(define (imdt->bool v)
  (intcast v bool-type))
(define (char->imdt x) x)
(define (imdt->char x) x)



(define (build-and x y) (if^ x y (ui 0)))
(define (build-or x y) (if^ x (ui 1) y))
(define (build-negate n) (fsub (fl 0.0) n))
(define ((call s) . args)
  (app^ ()))


(define prim-impl : (HashTable Symbol IMPL)
  (make-immutable-hash
   `((+          ,add    (,imdt->int ,imdt->int) (,int->imdt))
     (-          ,sub    (,imdt->int ,imdt->int) (,int->imdt))
     (*          ,mul    (,imdt->int ,imdt->int) (,int->imdt))
     (%<<        ,shl    (,imdt->int ,imdt->int) (,int->imdt))
     (%>>        ,lshr   (,imdt->int ,imdt->int) (,int->imdt))
     (%/         ,sdiv   (,imdt->int ,imdt->int) (,int->imdt))
     (%%         ,srem   (,imdt->int ,imdt->int) (,int->imdt))
     (binary-and ,and^   (,imdt->int ,imdt->int) (,int->imdt))
     (binary-or  ,or^    (,imdt->int ,imdt->int) (,int->imdt))
     (binary-xor ,xor^   (,imdt->int ,imdt->int) (,int->imdt))
     (binary-not ,not^   (,imdt->int) (,int->imdt))
     (<          ,icmp-slt (,imdt->int ,imdt->int) (,bool->imdt))
     (<=         ,icmp-sle (,imdt->int ,imdt->int) (,bool->imdt))
     (=          ,icmp-eq  (,imdt->int ,imdt->int) (,bool->imdt))
     (>          ,icmp-sgt (,imdt->int ,imdt->int) (,bool->imdt))
     (>=         ,icmp-sge (,imdt->int ,imdt->int) (,bool->imdt))
     (and        ,build-and (,imdt->bool ,imdt->bool) (,bool->imdt))
     (or         ,build-or (,imdt->bool ,imdt->bool) (,bool->imdt))
     (not        ,not^     (,imdt->bool) (,bool->imdt))
     (quotient   ,sdiv     (,imdt->int ,imdt->int) (,int->imdt))
     (fl+        ,fadd     (,imdt->float ,imdt->float) (,float->imdt))
     (fl-        ,fsub     (,imdt->float ,imdt->float) (,float->imdt))
     (fl*        ,fmul     (,imdt->float ,imdt->float) (,float->imdt))
     (fl/        ,fdiv     (,imdt->float ,imdt->float) (,float->imdt))
     (flmodulo   ,frem  (,imdt->float ,imdt->float) (,float->imdt))
     (flmin      ,ri-minnum  (,imdt->float ,imdt->float) (,float->imdt))
     (flmax      ,ri-maxnum  (,imdt->float ,imdt->float) (,float->imdt))
     (fl<        ,fcmp-olt     (,imdt->float ,imdt->float) (,bool->imdt))
     (fl<=       ,fcmp-ole   (,imdt->float ,imdt->float) (,bool->imdt))
     (fl=        ,fcmp-oeq    (,imdt->float ,imdt->float) (,bool->imdt))
     (fl>        ,fcmp-ogt     (,imdt->float ,imdt->float) (,bool->imdt))
     (fl>=       ,fcmp-oge    (,imdt->float ,imdt->float) (,bool->imdt))
     (flquotient ,fdiv     (,imdt->float ,imdt->float) (,int->imdt))
     (flround    ,ri-round (,imdt->float) (,float->imdt))
     (flnegate   ,build-negate    (,imdt->float) (,float->imdt))
     (flabs      ,ri-fabs  (,imdt->float) (,float->imdt))
     (flfloor    ,ri-floor (,imdt->float) (,float->imdt))
     (flceiling  ,ri-ceil  (,imdt->float) (,float->imdt))
     (flcos      ,ri-cos   (,imdt->float) (,float->imdt))
     (fltan      ,ri-tan   (,imdt->float) (,float->imdt))
     (flsin      ,ri-sin   (,imdt->float) (,float->imdt))
     (flasin     ,call-asin  (,imdt->float) (,float->imdt))
     (flacos     ,call-acos  (,imdt->float) (,float->imdt))
     (flatan     ,call-atan  (,imdt->float) (,float->imdt))
     (flatan2    ,call-atan2 (,imdt->float ,imdt->float) (,float->imdt)) 
     (fllog      ,ri-log   (,imdt->float) (,float->imdt))
     (flexp      ,ri-exp   (,imdt->float) (,float->imdt))
     (flsqrt     ,ri-sqrt  (,imdt->float) (,float->imdt))
     (Print      ,call-printf  (,imdt->string) ())
     (Exit         "exit"  (,no-cast) ())
     (int->float   "none"  (,imdt->int->float) ())
     (float->int   "none"  (,imdt->float->int) ())
     (read-bool    "read_bool"  () (,int->imdt))
     (read-int   "read_int"   () (,int->imdt))
     (read-float "read_float" () (,float->imdt))
     (read-char  "read_ascii_char" () (,char->imdt))
     (print-char "print_ascii_char" (,imdt->char) ())
     (display-char "display_ascii_char" (,imdt->char) ())
     (int->char   "none"  (,imdt->char) ())
     (char->int   "none"  (,char->imdt) ())
     (make-assoc-stack
      "grift_make_assoc_stack"
      () (,assoc-stack->imdt))
     (assoc-stack-push!
      "grift_assoc_stack_push"
      (,imdt->assoc-stack ,imdt->grift-obj ,imdt->grift-obj ,imdt->grift-obj)
      ())
     (assoc-stack-pop!
      "grift_assoc_stack_pop"
      (,imdt->assoc-stack) (,grift-obj->imdt))
     (assoc-stack-find
      "grift_assoc_stack_find"
      (,imdt->assoc-stack ,imdt->grift-obj ,imdt->grift-obj)
      (,int->imdt))
     (assoc-stack-set!
      "grift_assoc_stack_set"
      (,imdt->assoc-stack ,imdt->int ,imdt->grift-obj) ())
     (assoc-stack-ref
      "grift_assoc_stack_ref"
      (,imdt->assoc-stack ,imdt->int)
      (,grift-obj->imdt)))))

#;
(define (easy-p? [s : Symbol]) : Boolean
  (if (hash-ref prim-impl s #f) #t #f))
#;
(define (easy-p->impl [s : Symbol]) : IMPL
  (define (err) (error 'easy-p->impl))
  (hash-ref prim-impl s err))

#;
(define (emit-op p exp*)
  (match* (p exp*)
    [('Array-set! (list a o v))
     (begin
       (emit-wrap (display "(int64_t*)")
                  (emit-value a))
       (display "[")
       (emit-value o)
       (display "] = ")
       (emit-value v))]
    [('Array-ref (list a o))
     (begin
       (emit-wrap (display "(int64_t*)")
                  (emit-value a))
       (display "[")
       (emit-value o)
       (display "]"))]
    [('print-float (list f p))
     (display "printf")
     (emit-wrap (display "\"%.*lf\",")
                (emit-value p)
                (display ", ")
                (display "imdt_to_float")
                (emit-wrap (emit-value f)))]
    [('print-int (list d))
     (display "printf")
     (emit-wrap (display "\"%ld\",") (emit-value d))]
    [('print-bool (list b))
     (display "puts")
     (emit-wrap (emit-value b) (display " ? \"#t\" : \"#f\""))]
    [('Printf (cons fmt exp*))
     (begin (display "printf")
            (emit-wrap
             (emit-value fmt)
             (unless (null? exp*)
               (display ", ")
               (sequence emit-value exp* display "" "(void *)" "," "" ""))))]
    [('Alloc (list exp))
     ;; TODO this could be implemented in the C Code as a layer of macros
     (match (garbage-collector)
       ['Boehm (display "GC_MALLOC(8 * ")
               (emit-value exp)
               (display ")")]
       ['None (display "nonegc_malloc(8 * ")
              (emit-value exp)
              (display ")")])]
    [('Types-hashcons! (list e hcode))
     (display "hashcons(")
     (display "types_ht,")
     (emit-value e)
     (display ",")
     (emit-value hcode)
     (display ")")]
    [('Types-gen-index! (list))
     (display "types_unique_index_counter++")]
    [('mref-cast-queue-enqueue (list addr ty))
     (display "cast_queue_enqueue(mref_cast_q,")
     (emit-value addr)
     (display ",")
     (emit-value ty)
     (display ")")]
    [('mref-cast-queue-dequeue (list))
     (display "cast_queue_dequeue(mref_cast_q)")]
    [('mref-cast-queue-not-empty? (list))
     (bool->imdt (lambda () (display "cast_queue_is_not_empty(mref_cast_q)")))]
    [('mref-cast-queue-peek-address (list))
     (display "cast_queue_peek_address(mref_cast_q)")]
    [('mref-cast-queue-peek-type (list))
     (display "cast_queue_peek_type(mref_cast_q)")]
    [('mvect-cast-queue-enqueue (list addr ty))
     (display "cast_queue_enqueue(mvect_cast_q,")
     (emit-value addr)
     (display ",")
     (emit-value ty)
     (display ")")]
    [('mvect-cast-queue-dequeue (list))
     (display "cast_queue_dequeue(mvect_cast_q)")]
    [('mvect-cast-queue-not-empty? (list))
     (bool->imdt (lambda () (display "cast_queue_is_not_empty(mvect_cast_q)")))]
    [('mvect-cast-queue-peek-address (list))
     (display "cast_queue_peek_address(mvect_cast_q)")]
    [('mvect-cast-queue-peek-type (list))
     (display "cast_queue_peek_type(mvect_cast_q)")]
    [('timer-start (list)) (display timer-start)]
    [('timer-stop  (list)) (display timer-stop)]
    [('timer-report (list)) (display timer-report)]
    [((? easy-p? (app easy-p->impl (list t s a* r))) e*)
     (emit-easy-op e* t s a* r)]
    [(other wise)
     (error 'backend/c/generate-c/emit-op "unmatched value ~a" other)]))

(define (generate-value-op p exp*)
  (cond
    [(primitive-effect/unit-return? p)
     (let^ '() '() '() (generate-op p v*) (ui UNIT-IMDT))]
    [else (generate-op p v*)]))
