#lang racket

;; Assignment 3: A CE (Control and Environment) interpreter for Scheme

(provide interp-ce)

; Your task is to write a CE interpreter for a substantial subset of Scheme/Racket. 
; A CE interpreter is meta-circular to a large degree (e.g., a conditional in the target
; language (scheme-ir?) can be implemented using a conditional in the host language (Racket),
; recursive evaluation of a sub-expression can be implemented as a recursive call to the
; interpreter, however, it's characterized by creating an explicit closure value for lambdas
; that saves its static environment (the environment when it's defined). For example, a CE
; interpreter for the lambda calculus may be defined:
(define (interp-ce-lambda exp [env (hash)])
  (match exp
         [`(lambda (,x) ,body)
          ; Return a closure that pairs the code and current (definition) environment
          `(closure (lambda (,x) ,body) ,env)]
         [`(,efun ,earg)
          ; Evaluate both sub-expressions
          (define vfun (interp-ce-lambda vfun env))  
          (define varg (interp-ce-lambda earg env))
          ; the applied function must be a closure
          (match-define `(closure (lambda (,x) ,body) ,env+) vfun)
          ; we extend the *closure's environment* and interp the body
          (interp-ce-lambda body (hash-set env+ x varg))]
         [(? symbol? x)
          ; Look up a variable in the current environment
          (hash-ref env x)]))

; Following is a predicate for the target language you must support. You must support any
; syntax allowed by scheme-ir that runs without error in Racket, returning a correct value..
(define (scheme-ir? exp)
  ; You should support a few built-in functions bound to the following variables at the top-level
  (define (prim? s) (member s '(+ - * = equal? list cons car cdr null?)))
  (match exp
         [`(lambda ,(? (listof symbol?)) ,(? scheme-ir?)) #t] ; fixed arguments lambda
         [`(lambda ,(? symbol?) ,(? scheme-ir?)) #t] ; variable argument lambda
         [`(if ,(? scheme-ir?) ,(? scheme-ir?) ,(? scheme-ir?)) #t]
         [`(let ([,(? symbol?) ,(? scheme-ir?)] ...) ,(? scheme-ir?)) #t]
         [`(let* ([,(? symbol?) ,(? scheme-ir?)] ...) ,(? scheme-ir?)) #t]
         [`(and ,(? scheme-ir?) ...) #t]
         [`(or ,(? scheme-ir?) ...) #t]
         [`(apply ,(? scheme-ir?) ,(? scheme-ir?)) #t]
         [(? (listof scheme-ir?)) #t]
         [(? prim?) #t]
         [(? symbol?) #t]
         [(? number?) #t]
         [(? boolean?) #t]
         [''() #t]
         [_ #f]))

; Interp-ce must correctly interpret any valid scheme-ir program and yield the same value
; as DrRacket, except for closures which must be represented as `(closure ,lambda ,environment).
; (+ 1 2) can return 3 and (cons 1 (cons 2 '())) can yield '(1 2). For programs that result in a 
; runtime error, you should return `(error ,message)---giving some reasonable string error message.
; Handling errors and some trickier cases will give bonus points. 
(define (interp-ce exp)
  (call/cc (lambda (ret)
  ;constuructor for the top enviroment with the primatives were supporting out the door.
  ;I feel like theres a more elegant way to do this but it works.
  (define (prim? s) (member s '(+ - * = equal? list cons car cdr null?)))
  (define (top-env hsh) 
    (hash-set* hsh '+ + '- - '* * '= =
    'equal? equal? 'list list 'cons cons
    'car car 'cdr cdr 'null? null?))

  (define (value-of-if ge te fe env)
    (let ([value_ge (interp ge env)])
      (match value_ge
            [#t (interp te env)]
            [#f (interp fe env)])))
  
  (define (value-of-apply fn ls env)
    (let ([ls2 (interp ls env)])
          (match fn
            [(? prim? prim) (apply (hash-ref env prim) ls2)]
            [_ (apply (interp fn env) ls2)])))

  (define (value-of-and es env)
    (foldl (λ (e acc) (and (interp e env) acc)) #t es))

  (define (value-of-and2 es env)
    (define (h es)
      (if (null? es) #t
      (let ([ve (interp (car es) env)])
        (if (equal? ve #f) #f
            (h (cdr es))))))
      (h es))

  (define (value-of-or es env)
    (foldl (λ (e acc) (or (interp e env) acc)) #f es))

  (define (value-of-or2 es env)
    (define (h es)
      (if (null? es) #f
      (let ([ve (interp (car es) env)])
        (if (equal? ve #t) #t
            (h (cdr es))))))
      (h es))

  (define (value-of-prim-exp prim es env)
    (define op (hash-ref env prim))
    (apply (hash-ref env prim)
           (foldr (λ (e acc) (cons (interp e env) acc)) '() es)))

  (define (value-of-symbol env s)
    (if (hash-has-key? env s)
        (hash-ref env s)
        (ret `(error ,s not found))))
  
  (define (interp exp env)
    (match exp
      [`(lambda ,args ,body)
        `(closure ,exp ,env)]
      [`(lambda (,x) ,bdy)
        `(closure (lambda (,x) ,bdy) ,env)]
      [`(if ,ge ,te ,fe) (value-of-if ge te fe env)]
      [`(apply ,fn ,ls) (value-of-apply fn ls env)]
      [`(and ,es ...) (value-of-and2 es env)]
      [`(or ,es ...) (value-of-or2 es env)]
      [`(let ([,xs ,es] ...) ,bdy)
       (interp bdy (foldl (λ (x e env+) (hash-set env+ x (interp e env))) env xs es))]
      [`(let* ([,xs ,es] ...) ,bdy)
        (interp bdy (foldl (λ (x e env+) (hash-set* env+ x (interp e env+))) env xs es))]
      [(? number? n) n]
      [`(,prim ,es ...) #:when (prim? prim) (value-of-prim-exp prim es env)]
      [(? boolean? bool) bool]
      [''() '()]
      [(? symbol? x) (value-of-symbol env x)]
      [`(,ef ,ea)
       (define vf (interp ef env))
       (define va (interp ea env))
       (match-define `(closure (lambda (,x) ,bdy) ,env+) vf)
       (interp bdy (hash-set env+ x va))]
      [`(,ef ,eargs ...)
       (define vf (interp ef env))
       (define vas (foldr (λ (earg acc) (cons (interp earg env) acc)) '() eargs))
       (match-define `(closure (lambda ,xs ,bdy) ,env+) vf)
       (if (list? xs)
           (interp bdy (foldl (λ (x va env+) (hash-set env+ x va)) env+ xs vas))
           (interp bdy (hash-set env+ xs vas)))]
      [_ (ret '(error, match error))]))
  (interp exp (top-env(hash))))))