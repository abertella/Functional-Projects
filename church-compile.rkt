#lang racket

;; Assignment 4: A church-compiler for Scheme, to Lambda-calculus

(provide church-compile
         churchify
         ; provided conversions:
         church->nat
         church->bool
         church->listof)


;; Input language:
;
; e ::= (letrec ([x (lambda (x ...) e)]) e)    
;     | (let ([x e] ...) e)  
;     | (let* ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (e e ...)    
;     | x  
;     | (and e ...) | (or e ...)
;     | (if e e e)
;     | (prim e) | (prim e e)
;     | datum
; datum ::= nat | (quote ()) | #t | #f 
; nat ::= 0 | 1 | 2 | ... 
; x is a symbol
; prim is a primitive operation in list prims
; The following are *extra credit*: -, =, sub1  
(define prims '(+ * - = add1 sub1 cons car cdr null? not zero?))

; This input language has semantics identical to Scheme / Racket, except:
;   + You will not be provided code that yields any kind of error in Racket
;   + You do not need to treat non-boolean values as #t at if, and, or forms
;   + primitive operations are either strictly unary (add1 sub1 null? zero? not car cdr), 
;                                           or binary (+ - * = cons)
;   + There will be no variadic functions or applications---but any fixed arity is allowed

;; Output language:

; e ::= (lambda (x) e)
;     | (e e)
;     | x
;
; also as interpreted by Racket


;; Using the following decoding functions:

; A church-encoded nat is a function taking an f, and x, returning (f^n x)
(define (church->nat c-nat)
  ((c-nat add1) 0))

; A church-encoded bool is a function taking a true-thunk and false-thunk,
;   returning (true-thunk) when true, and (false-thunk) when false
(define (church->bool c-bool)
  ((c-bool (lambda (_) #t)) (lambda (_) #f)))

; A church-encoded cons-cell is a function taking a when-cons callback, and a when-null callback (thunk),
;   returning when-cons applied on the car and cdr elements
; A church-encoded cons-cell is a function taking a when-cons callback, and a when-null callback (thunk),
;   returning the when-null thunk, applied on a dummy value (arbitrary value that will be thrown away)
(define ((church->listof T) c-lst)
  ; when it's a pair, convert the element with T, and the tail with (church->listof T)
  ((c-lst (lambda (a) (lambda (b) (cons (T a) ((church->listof T) b)))))
   ; when it's null, return Racket's null
   (lambda (_) '())))


;; Write your church-compiling code below:

; churchify recursively walks the AST and converts each expression in the input language (defined above)
;   to an equivalent (when converted back via each church->XYZ) expression in the output language (defined above)
(define (churchify e)
  (match e
    [`(let ([,x ,rhs]) ,body)
     (churchify `((lambda (,x) ,body) ,rhs))]
    [`(lambda () ,body)
     `(lambda (_) ,(churchify body))]
    [`(lambda (,x) ,body)
     `(lambda (,x) ,(churchify body))]
    [`(lambda (,x ,ys ...) ,body)
     `(lambda (,x) ,(churchify `(lambda ,ys ,body)))]
    [`(,ef)
     `(,(churchify ef) (lambda (x) x))]
    [`(,ef ,earg)
     `(,(churchify ef) ,(churchify earg))]
    [`(,ef ,earg0 ,eargs ...)
     (churchify `((,ef ,earg0) ,@eargs))]
    [(? integer? n)
     (define (wrap n)
       (if (zero? n)
           'x
           `(f ,(wrap (- n 1)))))
     (churchify `(lambda (f x) ,(wrap n)))]
    [_ e]))

; Takes a whole program in the input language, and converts it into an equivalent program in lambda-calc
(define (church-compile program)
  ; Define primitive operations and needed helpers using a top-level let form?
  (define (prim? s) (member s prims))
  (define id `(lambda (x) x))
  (define church+ `(lambda (m n) (lambda (f x) (n f (m f x)))))
  (define church* `(lambda (m n) (lambda (f x) ((m (n f)) x))))
  (define church- `())
  (define church-t `(lambda (t) (lambda (f) (t ,id))))
  (define church-f `(lambda (t) (lambda (f) (f ,id))))
  (define church-zero? `(lambda (x) ((x (lambda (_) ,church-f)) ,church-t)))
  (define church-cons `(lambda (a) (lambda (b) (lambda (when-cons) (lambda (when-null) ((when-cons a) b))))))
  (define church-car `(lambda (p) (p (λ(x y) x))))
  (define church-cdr `(lambda (p) (p (λ(x y) y))))
  (define church-term `(lambda (is-cons) (lambda(is-null) (is-null))))
  (define church-null? `(lambda (p) (p (lambda(x x) church-f)) (lambda(_) church-t)))

  (define (compile-h prog)
      (match prog
        [#t church-t]
        [#f church-f]
        [`(#t ,rest ...) `(,church-t ,(compile-h rest))]
        [`(#f ,rest ...) `(,church-f ,(compile-h rest))]
        [`(+ ,x ,y) `((,church+ ,(compile-h x)) ,(compile-h y))]
        [`(* ,x ,y) `((,church* ,(compile-h x)) ,(compile-h y))]
        [`(- ,x ,y) `((,church-  ,(compile-h x)) ,(church-compile y))]
        [`(add1 ,x) `((,church+ ,(compile-h x)) 1)]
        [`(sub1 ,x) `((,church- ,(compile-h x)) 1)]
        [`(zero? ,x) `((,church-zero?) ,(compile-h x))]
        [`(cons ,x ,y) `((,church-cons ,(compile-h x)) ,(compile-h y))]
        [`(car ,lst) `(,church-car (compile-h ,lst))]
        [`(cdr ,lst) `(,church-cdr (compile-h ,lst))]
        [`(null? ,x) `(,church-null? (compile-h ,x))]
        [`() church-term]
        [`(if ,c ,t ,f) (compile-h `((,(compile-h c) (lambda(_) ,(compile-h t))) (lambda(_) ,(compile-h f))))]
        [`(and ,x ,rest ...) (compile-h `(if ,x ,(compile-h rest) #f))]
        [`(or ,x ,rest ...) (compile-h `(if ,x #t ,(compile-h rest)))]
        [`(not ,b) (if (equal? b '#f) '#t '#f)]
        [`(let ([,xs ,es] ...) ,bdy) (compile-h `((lambda (,@xs) ,(compile-h bdy) ,@es)))]
        [(? integer? n) n]
        [(? symbol? x) x]
        [`(lambda () ,body)
         `(lambda () (compile-h body))]
        [`(lambda (,x) ,body)
         `(lambda (,x) ,(compile-h body))]
        [`(lambda (,x ,ys ...) ,body)
         `(lambda (,x ,ys) ,(compile body))]
        [`(,ef ,earg) `(,ef ,(compile-h earg))]
        [_ prog]))
  
  
  (churchify (compile-h program)))
