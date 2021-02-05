#lang racket

;; Coding Exam 1

(provide only-integers
         power-set
         list->polynomial
         interp-ce-ext)


;; Problem 0: Filter a list of numbers to contain only integers (in the same order as given)
;;
;; E.g., (only-integers '(0 5 7.2 6.0 3.14159 1/3)) => '(0 5 6.0)

(define (only-integers lst)
  (filter integer? lst))


;; Problem 1: Computing a power-set 
;;
;; Your task is to write a function taking a set and returning its power set
;;
;; E.g., (power-set (set 1 2)) => (set (set) (set 1) (set 2) (set 1 2))

(define (power-set st)
  (define (loop lst1 lst2)
    (if (null? lst1)
        lst2
        (loop (cdr lst1) (append lst2 (map (λ(x) (append x (list (car lst1)))) lst2)))))
  (list->set(map list->set (loop (set->list st) (cons '() (map list (set->list st)))))))


;; Problem 2: Turn a list of coefficients into a polynomial function
;;
;; Your task is to write a function list->polynomial that takes a list of
;; number?-satisfying coefficients (ordered from degree zero, one, two, on up) 
;; and returns a single-arity function encoding the polynomial.
;;
;; E.g., (list->polynomial '(2 3 4)) should return a value equivalent to
;;    => (lambda (x) (+ (* 4 x x) (* 3 x) 2))

(define (list->polynomial lst)
  (define (h lst1)
    `(lambda (x) (+ ,(loop lst '() 0))))
  (define (loop lst2 lst3 n)
    (if (null? (cdr lst2))
        (append lst3 `((* ,(car lst2) (expt x ,n))))
        (append `((* ,(car lst2) (expt x ,n))) (loop (cdr lst2) lst3 (add1 n)))))
  (eval (h lst)))


;; Problem 3: Debugging a CE interpreter [***]
;;
;; Your task is to write a CE interpreter for the language
;; lambda-ext?, which should implement the call-by-name (CBN) lambda
;; calculus with several extensions. To do this, you will fix the
;; provided starter code in interp-ce-ext. This function contains
;; three bugs. You will fix these bugs so that interp-ce-ext properly
;; interprets the language lambda-ext? using the CBN evaluation
;; strategy.

;; prims include +, -, *, /, >, and <
(define (prim? x) (member x '(+ - * / < >)))

(define (lambda-ext? expr)
  (match expr
    ; Lambdas
    [`(lambda (,(? symbol? x)) ,(? lambda-ext? e-body)) #t]
    ; Variable reference
    [(? symbol? x) #t]
    ; Boolean and integer constants
    [(? boolean? b) #t]
    [(? integer? i) #t]
    ; If
    [`(if ,(? lambda-ext? e-guard) ,(? lambda-ext? e-true) ,(? lambda-ext? e-false)) #t]
    ; Binary primitives
    [`(,(? prim? operator) ,(? lambda-ext? e0) ,(? lambda-ext? e1)) #t]
    ; Applications
    [`(,(? lambda-ext? e0) ,(? lambda-ext? e1)) #t]))

(define (interp-ce-ext exp [env (hash)])
  ; generate a racket function corresponding to the prim
  (define (prim->impl prim)
    (hash-ref (hash '+ + '- - '* * '/ / '< < '> >) prim))
  (match exp
    ; lambda
    [`(lambda (,x) ,body)
     ; return a closure that pairs the code and current (definition) environment
     `(closure (lambda (,x) ,body) ,env)]
    ; variables
    [(? symbol? x)
     ; look up variable in the current environment
     (hash-ref env x)]
    ; boolean and integer constants
    [(? boolean? b) b]
    [(? integer? i) i]
    ; if
    [`(if ,(? lambda-ext? e-guard) ,(? lambda-ext? e-true) ,(? lambda-ext? e-false))
     (define vguard (interp-ce-ext e-guard env))
     (if vguard (interp-ce-ext e-true env)  (interp-ce-ext e-false env))]
    ; binary primitives
    [`(,(? prim? prim) ,(? lambda-ext? e0) ,(? lambda-ext? e1))
     ((prim->impl prim) (interp-ce-ext e0 env) (interp-ce-ext e1 env))]
    ; applications
    [`(,efun ,earg)
     (define vfun (interp-ce-ext efun env))
     (match-define `(closure (lambda (,x) ,body) ,env+) vfun)
     (interp-ce-ext body (hash-set env+ x earg))]))
