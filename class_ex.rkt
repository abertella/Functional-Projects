#lang racket
(require 2htdp/image)

(define (rand-lines n m)
  (define (loop ls n)
    (if (zero? n)
        ls
    (loop (append ls (list(gen-cons m))) (sub1 n))))
  (loop '() n))

(define (gen-cons m)
  (let
      ([begin (random (car m) (cdr m))])
       (list begin (random begin  (cdr m)))))

(define (gilray-rand-lines N M L)
  (map (lambda (_)
              (let* ([len (random L)]
                  [start (random (- M L))])
                (list start (+ start (random L)))))
       (range N)))

(define (line-coverage lines)
  (define sorted-lines (sort lines <= #:key first))
    (define (accumulate-total-from lines total from-x)
      (if (null? lines)
          total
          (match (first lines)
            [`(,start ,end) #:when (<= end from-x)
             (accumulate-total-from (cdr lines) total from-x)]
            [`(,start ,end)
             (define real-start (max start from-x))
             (accumulate-total-from (cdr lines)
                                        (+ total (- end real-start) end))])))
    (accumulate-total-from sorted-lines 0 0))


(define (line-coverage1 lines)
  (define (lt-add line lt)
    (match line
      [`(,ls ,le) #:when (< ls le)
    (match lt
      ['covered 'covered]
      ['empty
       `(lt ,ls empty (lt ,le covered empty))]
      [`(lt ,px ,left ,right)
       `(lt ,px
            ,(lt-add `(,(min ls px) ,(min le px)) left)
            ,(lt-add `(,(max ls px) ,(max le px)) right))])]
      [_ lt]))
  (define (sum-lt lt [wstart -inf.0] [wend +inf.0])
    (match lt
      ['empty 0]
      ['covered (- wend wstart)]
      [`(lt ,px ,left ,right)
       (+ (sum-lt left wstart px)
          (sum-lt right px wend))]))
  (sum-lt(foldl lt-add 'empty lines)))

(define (interp e)
  (define (interp-ce e env)
    (match e
      [(? symbol? x)
       (hash-ref env x)]
      [`(lambda (,x) ,bdy)
        `(closure (lambda (,x) ,bdy) ,env)]
      [`(,ef ,ea)
       (define vf (interp-ce ef env))
       (define va (interp-ce ea env))
       (match-define `(closure (lambda (,x) ,bdy) ,env+) vf)
       (interp-ce bdy (hash-set env+ x va))]))
  (interp-ce e (hash)))

(define (interp2 e)
  ;More metacircular
  (define (interp-ce e env)
    (match e
      [`(let ([,xs ,es] ...) ,bdy)
       (interp-ce bdy (foldl (λ (x e env+) (hash-set env+ x (interp-ce e env))) env xs es))] 
      [(? symbol? x)
       (hash-ref env x)]
      [`(lambda (,x) ,bdy)
        (lambda (v) (interp-ce bdy (hash-set env x v)))]
      [`(,ef ,ea)
       ((interp-ce ef env) (interp-ce ea env))]
      [(? number? n) n]))
  (interp-ce e (hash)))

(define zero (λ (f) (λ (x) x)))
(define one (λ (f) (λ (x) (f x))))
(define two (λ (f) (λ (x) (f (f x)))))
(define three (λ (f) (λ (x) (f (f (f x))))))
(define church+ (λ (m) (λ (n)
                         (λ (f) (λ (x)
                                  ((n f) ((m f) x)))))))

(define church* (λ (m) (λ (n)
                         (λ (f) (λ (x)
                                  ((m (n f)) x))))))

(define add1c (λ (a) (λ (f) (λ (x) (f ((a f) x))))))

(define false (λ(x) (λ(y) y)))

(define true (λ(x) (λ (y) x)))

(define church-if (λ (ge te fe) ((ge te) fe)))

(define zero? (λ (a) (a (λ(x) false)) true))

(define (church->nat n) ((n add1) 0))

(define null (λ (is-cons) (λ(is-null) (is-null))))

(define cons (λ(a) (λ(b) (λ (is-cons) (λ (is-null) (is-cons a b))))))

(define null? (λ (p) (p (λ(x x) false)) (λ() true)))

(define car (λ (p) (p (λ(x y) x))))

(define cdr  (λ (p) (p (λ(x y) y))))

(let* ([U (λ (u) (u u ))]
       [Y (U (λ(y) λ(f) (f (λ (x) (((U y) f) x)))))]
       [fact (Y (λ (fact) (λ(n) (if (= n 0) 1 (* n (fact (-n 1)))))))])
  (fact 5))



(define p (delay (begin (pretty-print "hello") 99)))

