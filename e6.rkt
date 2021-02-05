#lang racket

;; Exercises 6: Reducing (normalizing/simplifying) lambda calc terms by textual substitution
;;              using four different orders of evaluation (applicative, normal, CBV, CBN)
(provide collect-evaluation-trace
         applicative-order-reduce
         normal-order-reduce
         CBV-order-reduce
         CBN-order-reduce)

; A predicate for terms in the lambda calculus
(define (exp? e)
  (match e
         [(? symbol?) #t]
         [`(,(? exp?) ,(? exp?)) #t]
         [`(lambda (,(? symbol?)) ,(? exp?)) #t]
         [_ #f]))

; Free variables (may be useful for defining capt-avoid-subst)
(define (free-vars exp)
  (define (yield-free exp1 st)
    (match exp1
         [`,s #:when (symbol? s) (set-add st s)]
         [`(,x ,y) #:when (and (exp? x) (exp? y))
                  (set-union st (set-union (free-vars x) (free-vars y)))]
         [`(lambda (,s) ,e) (set-subtract (set-union st (free-vars e)) (set s))]))
  (yield-free exp (set)))

; Capture avoiding substitution
(define (capt-avoid-subst e0 x e1)
  (define (replace exp old new)
  (cond
    [(null? exp) '()]
    [(equal? (car exp) old)
     (cons new (replace (cdr exp) old new))]
    [else (cons (car exp) (cdr exp))]))
  (match e0
    [`,s #:when (symbol? s)
         (if (equal? x s) e1
             (s))]
    [`(,exp0 ,exp1) #:when (and (exp? exp0) (exp? exp1))
              `(,(capt-avoid-subst exp0 x e1) ,(capt-avoid-subst exp1 x e1))]
    [`(lambda (,s) ,e) #:when (and (symbol? s) `(exp? e))
                       (if (equal? x s) `(lambda (,s) ,e)
                           (if (not (set-member? (free-vars e) s))
                               `(lambda (,s) ,(replace e x e1)) `(lambda (,s) ,e)))]
    [_ 'failed]))

; Reduce the given expression by exactly one beta-reduction using
; applicative evaluation order. Return the simplified expression in a
; singleton set, or return (set) if there is no applicative order redex.
; Skip over any redexes that cannot be reduced using capture avoiding subst.
(define (applicative-order-reduce e)
  (match e
         [(? symbol? y) (set)]
         [`(lambda (,y) ,body)
          (define body-st (applicative-order-reduce body))
          (if (set-empty? body-st)
              ; No redex found
              (set)
              ; Derive a lambda with simplified body
              (set `(lambda (,y) ,(set-first body-st))))]
         [`((lambda (,y) ,body) ,ea)
          (define body-st (applicative-order-reduce body))
          (if (set-empty? body-st)
              ; No redex under this lambda:
              (let ([ea-st (applicative-order-reduce ea)])
                (if (set-empty? ea-st)
                    ; This is the innermost+leftmost redex
                    (let ([body+ (capt-avoid-subst body y ea)])
                      (if (eq? body+ 'failed)
                          (set) ; reducing redex failed
                          (set body+)))
                    ; A redex under the argument expression was found
                    (set `((lambda (,y) ,body) ,(set-first ea-st)))))
              ; A redex under the lambda was reduced already
              (set `((lambda (,y) ,(set-first body-st)) ,ea)))]
         [`(,ef ,ea)
          (define ef-st (applicative-order-reduce ef))
          (if (set-empty? ef-st)
              ; Call expression contains no redex
              (let ([ea-st (applicative-order-reduce ea)])
                (if (set-empty? ea-st)
                    ; No redex found
                    (set)
                    ; Argument expression contained a redex
                    (set `(,ef ,(set-first ea-st)))))
              ; Call expression contained a redex
              (set `(,(set-first ef-st) ,ea)))]))

; Reduce the given expression by exactly one beta-reduction using
; normal evaluation order. Return the simplified expression in a
; singleton set, or return (set) if there is no normal-order redex.
; Skip over any redexes that cannot be reduced using capture avoiding subst.
(define (normal-order-reduce e)
  (match e
    [(? symbol? sym) (set)]
    [`(lambda (,sym) ,body)
     (define body-st (normal-order-reduce body))
     (if (set-empty? body-st)
         (set)
         (set `(lambda (,sym) ,(set-first body-st))))]
    [`((lambda (,sym) ,body) ,exp) (set (capt-avoid-subst body sym exp))]
    [`(,ef ,ea)
     (define ef-st (normal-order-reduce ef))
     (if (set-empty? ef-st)
         (let ([ea-st (normal-order-reduce ea)])
           (if (set-empty? ea-st)
               (set)
               (set `(,ef ,(set-first ea-st)))))
         (set `(,(set-first ef-st) ,ea)))]))

; Reduce the given expression by exactly one beta-reduction using
; call-by-value evaluation order. Return the simplified expression in a
; singleton set, or return (set) if there is no CBV order redex.
(define (CBV-order-reduce e)
  (match e
    [(? symbol? sym) (set)]
    [`(lambda (,sym) ,body) (set)]
    [`((lambda (,sym) ,body) ,exp)
      (match exp
        [(? symbol? sym) (set (capt-avoid-subst body sym exp))]
        [`(lambda (,sym2) ,body2) (set (capt-avoid-subst body sym exp))]
        [`((lambda (,sym2) ,body2) ,exp2) (set `((lambda (,sym) ,body) ,(capt-avoid-subst body2 sym2 exp2)))])]
    [`(,ef ,ea)
     (define ef-st (CBV-order-reduce ef))
     (if (set-empty? ef-st)
         (let ([ea-st (CBV-order-reduce ea)])
           (if (set-empty? ea-st)
               (set)
               (set `(,ef (set ,(set-first ea-st))))))
         (set `(,(set-first ef-st) ,ea)))]))
 

; Reduce the given expression by exactly one beta-reduction using
; call-by-name evaluation order. Return the simplified expression in a
; singleton set, or return (set) if there is no applicative order redex.
(define (CBN-order-reduce e)
  (match e
    [(? symbol? sym) (set)]
    [`(lambda (,sym) ,body) (set)]
    [`((lambda (,sym) ,body) ,exp) (set (capt-avoid-subst body sym exp))]
    [`(,ef ,ea)
     (define ef-st (CBN-order-reduce ef))
     (if (set-empty? ef-st)
         (let ([ea-st (CBN-order-reduce ea)])
           (if (set-empty? ea-st)
               (set)
               (set `(,ef-st (set ,(set-first ea-st))))))
         (set `(,(set-first ef-st) ,ea)))]))

; Takes one of the four step/reduce functions and an expression in the lambda calculus (satisfying exp?)
; Yields a list representing the full evaluation trace from e to a value
; Note, this function will non-terminate on programs like Omega that cannot be reduced to a value.
(define (collect-evaluation-trace step-f e)
  (let loop ([latest (set e)]
             [trace '()])
    (if (set-empty? latest)
        (reverse trace)
        (loop (step-f (set-first latest))
              (cons (set-first latest) trace)))))

