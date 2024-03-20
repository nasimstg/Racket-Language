#lang racket

;; cs450f23, section 2
;; CS450js Lang, for hw9
(provide (rename-out [parse parse450js]
                     [run run450js])
         fn-result?
         exn:fail:syntax:cs450js?
         NaN
         undefined-err?
         arity-err?
         apply-err?
         circular-err?)

(require rackunit)

;; 450jsExpr ------------------------------------------------------------
;; 450jsLang surface syntax, i.e., what the programmer writes, data defs.
;; The syntax of the language is a simplified s-expression

;; A 450jsVariable (Var) is a Symbol

;; A 450jsExpr (Expr) is one of
;; - 450jsAtom
;; - 450jsVariable
;; - (list 'bind [Var 450jsExpr] . List<450jsExpr>)
;; - (list 'bind/rec [Var 450jsExpr] . List<450jsExpr>)
;; - (list 'iffy 450jsExpr 450jsExpr 450jsExpr)
;; - (list 'fn List<Var> 450jsExpr)
;; - TestExpr
;; - 450jsFnApp
;; Interp: simplified s-expression used as surface syntax of "450js Lang"
;; - any non-atom or Var that is not 'bind 'iffy or 'fn is parsed as 450jsFnApp
;; - bind and bind/rec support multiple bodies

;; A 450jsFnApp is a (cons 450jsExpr List<450jsExpr>)
;; Inter: a cs450js function application expression

;; An 450jsAtom (Atom) is one of:
;; - Number
;; - String
;; - SymBool (hah)

;; a SymBool
;; - 'realtrue
;; - 'realfalse

;; A TestExpr is a:
;; - (list 'chk=? 450jsExpr 450jsExpr)
;;   interp: test passes if the two results are (Racket) equal?
;;           This is equiv to rackunit's check-equal?

;; - (list 'chkerr 450jsExpr 450jsExpr)
;;   interp: special case for testing error-producing programs
;;           - first Expr should be a predicate
;;           - second Expr should be error-producing program
;;           test passes if applying predicate to second arg produces true
;;           This is equiv to rackunit (check-true (expr1 expr2)) 
;; interp: These are special-case testing forms built into the language
;; They must be primitives and not functions because they need to
;; bypass the automatic error-propagation of 450jsLang

;; 450jsAST ------------------------------------------------------------
;; 450jsLang abstract syntax tree (AST)
;; These data defs capture the tree structure inherent in a program code.
;; Note that this represents "code", i.e., a program that has not been run.

;; An 450jsAST (AST) is one of:
;; - (num Number)
;; - (str String)
;; - (boo Boolean)
;; - (vari Symbol)
;; - (bind Symbol AST AST)
;; - (bind/rec Symbol AST AST)
;; - (ite AST AST AST)
;; - (app AST List<AST>)
;; - (chk-err AST AST)
;; - (chk=? AST AST)
;; - (fn-ast List<Symbol> AST)
(struct app [fn args] #:transparent)
(struct num [val] #:transparent)
(struct str [val] #:transparent)
(struct boo [val] #:transparent)
(struct vari [name] #:transparent)
(struct bind [var e body] #:transparent)
(struct bind/rec [var e body] #:transparent)
(struct ite [test then else] #:transparent)
(struct fn-ast [params code] #:transparent)

;; special case testing forms built into the language
(struct test-form [] #:transparent)
(struct chk=? test-form (expected actual))
(struct chkerr test-form (p? err-expr))

;; 450jsResult ------------------------------------------------------------
;; A Result is the "answer" produced by a running a 450jsLang program
;; (specifically, the AST representation is run)
;; In 450jsLang, errors are treated as just another "result"

;; A 450jsResult (Result) is one of
;; - NonErrorResult
;; - 450jsErrorResult
;; Interp: most functions need to handle errs specially,
;; so split "Result" data into errors and non-errors
;; - Note: "Falsy" values are a NonErrorResult

;; A NonErrorResult is one of
;; - (Racket) Number
;; - (Racket) String
;; - (Racket) Boolean
;; - NaN
;; - 450jsFunctionResult
(struct nan [])
(define NaN (nan))

;; A 450jsFunctionResult
;; - Racket Function 
;; - (fn-result List<Symbol> AST env)
;; interp:
;; - A Racket Function f is one where (procedure? f) = true
(define racket-fn? procedure?)
;; - A an fn-result is the result of evaluating a `fn` "lambda" function
(struct fn-result [params code env] #:transparent)

;; A 450jsErrorResult is one of
;; - (undefined-err Symbol)
;; - (arity-err 450jsFunctionResult List<450jsResult>)
;; - (apply-err 450jsResult)
;; - (circular-err Symbol)
;; interp:
;; - undefined-err : undefined variable, field is var name
;; - arity-err     : wrong # args, fields are misapplied fn and args,
;;                   only work with "fn", racket funcs get racket arityerr
;; - apply-err     : tried to apply non-function, field is non-fn res
;; - circular-err  : reference a bind/rec var whose Result
;;                   is not fully determined, field is var name

(struct 450js-err [] #:transparent)
(struct undefined-err 450js-err [var] #:transparent)
(struct arity-err 450js-err [fn args] #:transparent)
(struct apply-err 450js-err [fn] #:transparent)
(struct circular-err 450js-err [var] #:transparent)

;; conversion functions
;; ->bool : SymBool -> Boolean
;; converts 450js lang bool to racket bool
(define (->bool b)
  (match b
    ['realtrue #true]
    ['realfalse #false]))

;; racket exception for 450js-lang parse errors, e.g., (bind x x)
(struct exn:fail:syntax:cs450js exn:fail:syntax [])

;; parse : Expr -> AST
;; converts 450js-lang s-expression to its AST
;; raises 450js exception for some invalid uses of bind, bind/rec, iffy, fn,
;; and unsupported racket literals
(define (parse s)
  (match s
    [(? number?) (num s)]
    [(? string?) (str s)]
    ['realtrue (boo (->bool s))]
    ['realfalse (boo (->bool s))]
    [(? symbol?) (vari s)]
    [`(bind [,x ,e] . ,bodys) (bind x (parse e) (map parse bodys))]
    [`(bind/rec [,x ,e] . ,bodys) (bind/rec x (parse e) (map parse bodys))]
    [`(iffy ,test ,then ,else) (ite (parse test) (parse then) (parse else))]
    [`(fn ,args ,body) (fn-ast args (parse body))]
    [`(,(or 'bind 'bind/rec 'iffy 'fn) . ,_)
     (raise-syntax-error ; this must be before fn app case
      'parse450js "invalid use of bind, bind/rec, iffy, or fn" s
      #:exn exn:fail:syntax:cs450js)]
    [`(chk=? ,e1 ,e2) (chk=? (parse e1) (parse e2))]
    [`(chkerr ,e1 ,e2) (chkerr (parse e1) (parse e2))]
    [`(,(or 'chk=? 'chkerr) . ,_)
     (raise-syntax-error ; this must be before fn app case
      'parse450js "invalid use of test form chk=? or chkerr" s
      #:exn exn:fail:syntax:cs450js)]
    [`(,f . ,args) (app (parse f) (map parse args))] ; fn app
    [_ (raise-syntax-error
        'parse450js "not a valid CS450js program" s
        #:exn exn:fail:syntax:cs450js)]
  ))

(check-equal? (parse 1) (num 1))
(check-equal? (parse '(+ 1 2))
              (app (vari '+) (list (num 1) (num 2))))
(check-equal? (parse '(+ (- 1 2) (- 3 4)))
              (app (vari '+)
                   (list (app (vari '-) (list (num 1) (num 2)))
                         (app (vari '-) (list (num 3) (num 4))))))

(check-equal? (parse '(fn (x y) (+ x y)))
              (fn-ast '(x y) (app (vari '+) (list (vari 'x) (vari 'y)))))

(check-equal? (parse '(bind [x 1] (bind [y 2] (+ x y))))
              (bind
               'x (num 1)
               (list
                (bind
                 'y (num 2)
                 (list (app (vari '+) (list (vari 'x) (vari 'y))))))))

;; app and fn
(check-equal? (parse '((fn (x y) (+ x y)) 1 2))
              (app (fn-ast '(x y) (app (vari '+) (list (vari 'x) (vari 'y))))
                   (list (num 1) (num 2))))

;; check stx errs
(check-exn
 exn:fail:syntax:cs450js?
 (lambda () (parse '(fn (x) x x))))
(check-exn
 exn:fail:syntax:cs450js?
 (lambda () (parse '(iffy 1 2 3 4))))
(check-exn
 exn:fail:syntax:cs450js?
 (lambda () (parse '(bind x x))))
(check-exn
 exn:fail:syntax:cs450js?
 (lambda () (parse '(bind/rec x x))))

;; bool->str : Boolean -> String
(define (bool->str b)
  (if b "true" "false"))

;; bool->num : Boolean -> Number
(define (bool->num b)
  (if b 1 0))

;; res->string : Result -> String
(define (res->str x)
  (cond
    [(string? x) x]
    [(boolean? x) (bool->str x)]
    [(list? x) (string-join (map res->str x) ",")]
    [else (~a x)]))

;; res->string : Result -> Number
(define (res->num x)
  (cond
    [(number? x) x]
    [(boolean? x) (bool->num x)]
    [else NaN]))

;; res->bool : Result -> Boolean
;; used to compute "truthiness" of non-bool results
(define (res->bool x)
  (cond
    [(nan? x) false]
    [(false? x) false]
    [(and (number? x) (zero? x)) false]
    [(and (string? x) (string=? x "")) false]
    [(empty? x) false]
    [else true]))

;; 450+ : NonErrResult NonErrResult ... -> Result
;; follows js coercion semantics
(define (450+ . args)
  (cond
    [(andmap list? args) (apply append args)]
    [(ormap string? args) (apply string-append (map res->str args))]
    [(ormap nan? (map res->num args)) NaN]
    [else (apply + (map res->num args))]))

;; 450- : NonErrResult NonErrResult ... -> Result
(define (450- . args)
  (cond
    [(ormap nan? (map res->num args)) NaN]
    [else (apply - (map res->num args))]))

;; 450== : NonErrResult NonErrResult -> Result
;; follows js "loose ==" semantics
(define (450== x y)
  (cond
    [(or (and (number? x) (number? y))
         (and (boolean? x) (boolean? y))
         (and (string? x) (string? y)))
     (equal? x y)]
    [(boolean? x) (450== (bool->num x) y)]
    [(boolean? y) (450== x (bool->num y))]
    [else
     (equal? (res->str x) (res->str y))]))

;; An Environment (Env) is a List<EnvVal>
;; - represents in-scope variables while running a 450jsLang AST
;; - ids earlier in the list shadow later ones with the same name

;; An EnvVal is one of:
;; - 450jsResult
;; - Box<450jsResult>

;; lookup : Var Env -> Result
;; looks up the given var in the given env
;; unbound vars result in (undefined-err x)
(define (lookup x env)
  (or (and (assoc x env)
           (envval->result (second (assoc x env))))
      (undefined-err x)))

;; envval->result : EnvVal -> Result
(define (envval->result ev)
  (cond
    [(box? ev) (unbox ev)]
    [else ev]))

;; env-add : Var EnvVal Env -> Env
(define (env-add x res env)
  (cons (list x res) env))

;; env-add/many : List<Var> List<EnvVal> Env -> Env
(define (env-add/many xs ress env)
  (append (map list xs ress) env))

;; run : 450jsAST Env-> 450jsResult
;; Computes the result of running the given CS450js program AST.
;; Primarily follows a structural "map" of the AST, except
;; errors in the following positions are automatically propagated "up":
;; - applied function:
;;     (unknown-fn 1 2) =>
;;     ((undefined-err 'unknown-fn) 1 2) =>
;;     (undefined-err 'unknown-fn)
;; - fn arg, for racket fns only
;;      (+ (unknown-fn 1 2) ...) => ... => (undefined-err 'unknown-fn)
;; - iffy test position:
;;      (iffy (unknown-fn 1 2) ...) => ... => (undefined-err 'unknown-fn)
;; examples:
(define (run/env p env)
  (match p
    [(num n) n]
    [(boo b) b]
    [(str s) s]
    [(vari x) (lookup x env)]
    [(app f args)
     (450apply
      (run/env f env)
      args ; dont run args unless f is proper fn
      env)]
    [(bind x e bodys)
     (define new-env (env-add x (run/env e env) env))
     (last
      (map (curryr run/env new-env) bodys))]
    [(bind/rec x e bodys)
     (define placeholder (box (circular-err x)))
     (define env/placeholder (env-add x placeholder env))
     (define x-result (run/env e env/placeholder))
     (set-box! placeholder x-result)
     (last
      (map (curryr run/env env/placeholder) bodys))]
    [(fn-ast args body) (fn-result args body env)] ; dont eval body     
    [(ite tst thn els)
     (define tst-result (run/env tst env))
     (if (450js-err? tst-result) ;; errors are not "falsy"
         tst-result ; propagate error immediately
         (if (res->bool tst-result) ; else evaluate the iffy
             (run/env thn env)
             (run/env els env)))]
    [(chk=? expected actual)
     (check-equal? (run/env expected env) (run/env actual env))]
    [(chkerr p? err)
     (check-true ((run/env p? env) (run/env err env)))]))

;; run-all : List<AST> Env -> List<Result>
(define (run-all args env)
  (map (curryr run/env env) args))

;; 450apply : 450jsResult List<450jsAST> Env -> 450jsResult
(define (450apply fn args env)
  (match fn
    [(? racket-fn?)
     (define args-res (run-all args env))
     ; racket fn cant handle 450js-err so just propagate err
     (if (ormap 450js-err? args-res)
         (findf 450js-err? args-res) 
         (apply fn args-res))]
    [(fn-result params body fnenv)
     (if (= (length params) (length args))
         (run/env
          body
          (env-add/many params (run-all args env) fnenv))
         (arity-err fn args))]
    [(? 450js-err?) fn] ; propagate err
    [_ (apply-err fn)])) ; anything else is apply-err (can only apply fns)


;; for non+ numeric fns, do a rough conversion of non-num results
(define (mk-450jsnumfn fn)
  (lambda args
    (apply fn (map res->num args))))

(require 2htdp/image)

;; initial environment
(define INIT-ENV `((+ ,450+)
                   (- ,450-)
                   (* ,*)
                   (/ ,/)
                   (= ,=)
                   (== ,450==)
                   (chk=? ,check-equal?)
                   (err? ,450js-err?)
                   (undef-err? ,undefined-err?)
                   (circ-err? ,circular-err?)
                   (app-err? ,apply-err?)
                   (arity-err? ,arity-err?)
                   (mk-lst ,list)
                   (cns ,cons)
                   (fst ,first)
                   (rst ,rest)
                   (emp? ,empty?)
                   (emp ,empty)
                   (above ,above)
                   (append, append)
                   (beside ,beside)
                   (triangle ,triangle)
                   .
                   ,(map
                     list
                     ; length must match 2nd list below
                     '(< > <= >= lrger smller mod add1 sub1) 
                     (map
                      mk-450jsnumfn
                      (list < > <= >= max min remainder add1 sub1)))))

                   

;; run : 450jsAST -> 450jsResult
(define (run p) (run/env p INIT-ENV))
                          
(define compute (compose run parse))

(check-equal? (compute 1) 1)
(check-equal? (compute '(+ 1 2)) 3)
(check-equal? (compute '(+ (- 1 2) (- 3 4))) -2)

;; with js-style coercions: string, number, and bools

;; "adding" bools
(check-equal? (compute '(+ realtrue realfalse)) 1)
(check-equal? (compute '(+ realfalse realfalse)) 0)
(check-equal? (compute '(+ realtrue realtrue)) 2)

;; "adding" strings
(check-equal? (compute '(+ "hello" " world")) "hello world")

;; "adding" bools and strings
(check-equal? (compute '(+ "true" realtrue)) "truetrue")
(check-equal? (compute '(+ realtrue "true")) "truetrue")
(check-equal? (compute '(+ realtrue "false")) "truefalse")
(check-equal? (compute '(+ "false" realtrue)) "falsetrue")

;; "adding" bools and numbers
(check-equal? (compute '(+ realtrue 10)) 11)
(check-equal? (compute '(+ realfalse 10)) 10)
(check-equal? (compute '(+ -1 realfalse)) -1)

;; "adding" strings and numbers
(check-equal? (compute '(+ 100 "grand")) "100grand")
(check-equal? (compute '(+ "cs" 450)) "cs450")

;; "minus" bools
(check-equal? (compute '(- realtrue realfalse)) 1)
(check-equal? (compute '(- 100 realfalse)) 100)
(check-equal? (compute '(- realfalse 100)) -100)

;; "minus" with any string is NaN
(check-equal? (compute '(- "true" "false")) NaN)
(check-equal? (compute '(- "true" realfalse)) NaN)
(check-equal? (compute '(- "true" 100)) NaN)
(check-equal? (compute '(- 100 "true")) NaN)
(check-equal? (compute '(- realtrue "true")) NaN)


;; sane ==
(check-true (compute '(== 10 10)))
(check-false (compute '(== 11 10)))
(check-true (compute '(== "hello" (+ "hel" "lo"))))
(check-false (compute '(== "hello" "world")))
(check-false (compute '(== realtrue realfalse)))
(check-true (compute '(== realfalse realfalse)))

;; different types not equal
(check-false (compute '(== "hello" 10)))
(check-false (compute '(== 100 "world")))
(check-false (compute '(== realtrue "world")))
(check-false (compute '(== "true" realtrue)))
(check-false (compute '(== (+ "tr" "ue") realtrue)))
(check-false (compute '(== 100 realtrue)))
(check-false (compute '(== realtrue 100)))


;; sort of sane ==
(check-true (compute '(== realtrue 1)))
(check-true (compute '(== 1 realtrue)))
(check-true (compute '(== 0 realfalse)))
(check-true (compute '(== realfalse 0)))
(check-false (compute '(== 0 realtrue)))

;; insane ==
(check-true (compute '(== "100" 100)))
(check-true (compute '(== (+ 10 90) "100")))
(check-true (compute '(== (+ 10 90) (+ "10" "0"))))


;; if
(check-equal? (compute '(iffy (== 10 10) 100 200)) 100)
(check-equal? (compute '(iffy (== 10 11) 100 200)) 200)

;; js truthy false
(check-equal? (compute '(iffy (- 100 100) 100 200)) 200)
(check-equal? (compute '(iffy (- "100" 3) 100 200)) 200)
(check-equal? (compute '(iffy (+ "" "") 100 200)) 200)

;; lists
(check-equal? (compute '(+ "hello" (mk-lst "world" "!")))
              "helloworld,!")
;; emp list is truthy false
(check-equal? (compute '(iffy emp 1 2)) 2)

;; var and bind
(check-true (undefined-err? (compute 'x)))
(check-equal? (undefined-err 'x) (compute 'x))
(check-equal? (undefined-err-var (compute 'x)) 'x)
(check-equal? (compute '(bind [x 1] x)) 1)
(check-equal? (compute '(bind [x 1] (bind [y 2] (+ x y)))) 3)
(check-equal? (compute '(bind [x (- 4 3)] (bind [y (+ 5 6)] (+ x y)))) 12)

;; check 'if is undefined (should be 'iffy)
;; undefined-err should be propagated before arg is run (errors)
(check-true
 (undefined-err?
  (compute '(if x 1 2))))
(check-equal? (compute '(if x 1 2)) (undefined-err 'if))

; "fn"s (lambda) and app
(check-equal? (compute '(fn (x) x)) (fn-result '(x) (vari 'x) INIT-ENV))

(check-true (arity-err?
             (compute '((fn (x y) (+ x y)) 1))))
(check-equal? (compute '((fn (x y) (+ x y)) 1 2)) 3)

;; check shadowing, proper variable capture

;; x in-scope should be captured with function def
(check-equal? (parse '((bind [y 10] (fn (x) (+ x y))) 100))
              (app (bind
                    'y (num 10)
                    (list
                     (fn-ast '(x) (app (vari '+) (list (vari 'x) (vari 'y))))))
                   (list (num 100))))
(check-equal? (compute '(bind [y 10] (fn (x) (+ x y))))
              (fn-result
               '(x)
               (app (vari '+) (list (vari 'x) (vari 'y)))
               (cons '(y 10) INIT-ENV)))
(check-equal? (compute '((bind [y 10] (fn (x) (+ x y)))
                         100))
              110)

;; different xs for fn and args should not get shadowed
(check-equal? (compute '((bind [x 10] (fn (y) (+ x y)))
                         (bind [x 11] x)))
              21)

;; multiple lambdas
(check-equal? (compute '(((fn (x) (fn (x) (+ 1 x))) 10) 11)) 12)

;; second x is shadowed in arg
(check-equal? (compute '(bind [x 10] ((fn (y) (+ x y)) (bind [x 11] x))))
              21)

;; dynamic scope not supported - should be error
(check-true (undefined-err?
             (compute '(bind [f (fn (x) (+ x y))] (bind [y 10] (f 11))))))
 

(check-equal? (compute '(bind [x 10] (bind [x (+ x 1)] (+ x 2))))
              13)

;; bind/rec

(check-true (circular-err?
             (compute '(bind/rec [f f] f))))

(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (sub1 n)))))
(for ([n 6])
  (check-equal?
   (compute
    `(bind/rec [fac
                (fn (n)
                    (iffy n (* n (fac (- n 1))) 1))]
               (fac ,n)))
   (factorial n)))
