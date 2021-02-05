#lang racket

(define (rev-lst l)
  (define (loop l r)
    (if (null? l) r
    (loop (cdr l) (cons (car l) r))))
    (loop l '()))

(define (sum-list l)
  (define (h l s)
    (if (null? l) s
        (h (cdr l) (+ s (car l)))))
  (h l 0))

(define (mk-point x y)
  `(point ,x ,y))


(define (filter-numbers lst)
  (define (loop l1 l2)
    (if (null? l1) l2
    (match (car l1)
      [(? number?) (cons (car l1) (loop (cdr l1) l2))]
      [_ (loop (cdr l1) l2)])))
  (loop lst '()))

(define (is-flat? l)
  (if (null? l) #t
    (match (car l)
      [(? null?) #t]
      [(? pair?) #f]
      [_ (is-flat? (cdr l))])))

(define (append-lists l0 l1)
  (define (loop l0 l1)
     (if (null? l0) l1
         (loop (cdr l0) (cons (car l0) l1))))
  (rev-lst (loop l1 (loop l0 '()))))

(define (foo x)
  (match x
    [(? number?) (* x 2)]
    [`(,x ,y ,z) `(,x ,y)]
    [_ "error"]))

(define (elements< l n)
  (match l
    [(? null?) '()]
    [`(,a . ,b)
     #:when (< a n) (cons a (elements< b n))]
    [`(,a . ,b) (elements< b n)]))
    
(define (quicksort l)
  (if (empty? l)
      '()
      (let* ([pivot (first l)]
             [restl (rest l)]
             [elements-lt (elements< restl pivot)] [elements-gt (elements> restl pivot)])
        (append
         (quicksort elements-lt) (list pivot)
         (quicksort elements-gt)))))


(define (elements> l n)
  (match l
    ['() '()]
    [`(,first ,rest ...) #:when (> first n)
                         (cons first (elements> rest n))]
    [else (elements> (rest l) n)]))

(define (my_append lst0 lst1)
  (if (null? lst0)
      lst1
      (cons (car lst0) (append (cdr lst0) lst1))))

(define (my-filter p? lst)
  (if (null? lst)
      '()
      (if (p? (first lst))
          (cons (first lst) (my-filter p? (rest lst)))
          (my-filter p? (rest lst)))))

(define (list->h lst)
  (if (null? lst)
      (hash)
      (hash-set (list->h (cdr lst)) (car lst) (* ((car lst) (car lst))))))

(foldl + 0 '(1 2 3 4))

(define (my_foldl f acc lst)
(if (null? lst)
    acc
    (foldl (f (f (car lst) acc) (cdr lst)))))

(define (list->set lst)
  (foldl set-union (set) (map set lst) ))

(define (list->set2 lst)
  foldl (lambda(x st) (set-add st x)) (set) lst)

(define (foldr f acc lst)
  (foldl f acc (reverse lst)))

(define (my_reverse lst)
  (foldl cons '()  lst))

(define (my_foldr f acc lst)
  (if (null? lst)
      acc
      (f (car lst) (my_foldr f acc (cdr lst)))))

(define (grid-set n)
  (foldl (lambda (x st)
           (foldl (lambda (y st)
                    (set-add st (cons x y)))
                  st
                  (range (add1 n))))
         (set)
         (range (add1 n))))

(define (my_map f lst)
(if (null? lst)
    lst
    (foldl (f (f (car lst) '()) (cdr lst)))))

(define (isectln a1 a2 b1 b2)
  (cond
    [(or (> a1  a2) (> b1  b2)) 0]
    [(and (<= a1 b1) (<= b1 a2) (<= a2 b2)) (cons a2  b1)]
    [(and (<= b1 a1) (<= a1 b2) (<= b2 a2)) (cons b2  a1)]
    [(and (<= a1 b1) (<= b1 b2) (<= b2 a2)) (cons b2  b1)]
    [(and (<= b1 a1) (<= a1 a2) (<= a2 b2)) (cons a2  a1)]
    [else 0]))


(define (st-map str e acc)
   (match str
     [(? stream-empty? str) acc]
     [_ (st-map (stream-rest str) e (set-add acc (set e)))]))

(define (powerset aL)
  (if (empty? aL)
      '(())
      (let ((rst (powerset (rest aL))))
        (append (map (lambda (x) (cons (first aL) x))
                     rst)
                rst))))

(define (powerset1 st)
  (if (stream-empty? st)
      (set(set))
      (let ((rst (powerset1 (stream-rest st))))
        (set-add (set-map (lambda (x) (set-add (stream-first st) x))
                     rst)
                rst))))

(cons '() (map list (set->list(set 1 2 3))))

(define (my_ps st)
  (define (h l0 e)
    (append l0 (map (λ(x) (append x (list e))) l0)))
  (define (loop l1 l2)
    (if (null? l1)
        l2
        (loop (cdr l1) (h l2 (car l1)))))
  (list->set(map list->set (loop (set->list st) (cons '() (map list (set->list st)))))))

(define (rect-area rect)
  (match rect
      [`(rect ,x0 ,y0 ,x1 ,y1) (* (- x1 x0) (- y1 y0))]))

(define (rect-overlap rect1 rect2)
  (match rect1
    [`(rect ,x0 ,y0 ,x1 ,y1)
     (match rect2
        [`(rect ,a0 ,b0 ,a1 ,b1)
         (* (isectlna y0 y1 b0 b1) (isectlna y0 y1 b0 b1))])]))

(define (isectlna a1 a2 b1 b2)
  (cond
    [(or (> a1  a2) (> b1  b2)) (cons 0 0)]
    [(and (<= a1 b1) (<= b1 a2) (<= a2 b2)) (- b1 a2)]
    [(and (<= b1 a1) (<= a1 b2) (<= b2 a2)) (- a1 b2)]
    [(and (<= a1 b1) (<= b1 b2) (<= b2 a2)) (- b1 b2)]
    [(and (<= b1 a1) (<= a1 a2) (<= a2 b2)) (- a1 a2)]
    [else (cons 0 0)]))

