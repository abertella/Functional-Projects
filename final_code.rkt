#lang racket

(let* ([U (λ (u) (u u))]
      [Y (U (λ (y) (λ (f) (f (λ (x) (((U y) f) x))))))]
      [fact (Y (λ (fact) (λ (n) (if (= n 0) 1 (* n (fact (- n 1)))))))])
   (fact 5))

(let* ([U (λ (u) (u u))]
      [Y (U (λ (y) (λ (f) (f (λ (x) (((U y) f) x))))))]
      [fib (Y (λ (fib)
                (λ (n)
                  (case n
                    [(0) 0]
                    [(1) 1]
                    [else (+ (fib (- n 1)) (fib (- n 2)))]))))])
   (fib 7))

(define (my_map f lst)
  (foldr (λ (e l) (cons (f e) l)) '() lst))

(define (my_map2 f lst)
  (if (null? lst) lst
      (cons (f (car lst)) (my_map2 f (cdr lst)))))

(define (my_map3 f lst)
  (define (tailmap lst stack)
    (if (null? lst)
        (ret lst stack)
        (let ([n (f (car lst))])
        (tailmap (cdr lst) `((cons ,n) . ,stack)))))
  (define (ret v stack)
    (match stack
      [`((cons ,v1) ,stack ...)
       (ret (cons v1 v) stack)]
      ['() v]))
  (tailmap lst '()))

(define expander (λ(_) ((λ (f)
                          (f
                           ((((λ (y)
                                (λ (f)(f (((y y) f)))))
                              (λ (y)
                                (λ (f)(f (((y y) f))))))f)))) (λ (x) x))))

(define expander2 (λ(_) ((λ(u) (u u)) (λ(x) (x (x x))))))