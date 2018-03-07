#!chezscheme

(library (cslib function)

  (export
    id
    compose
    on
    cmp
    <?
    =?
    number-cmp
    string-cmp
    vector-cmp
    curry
    curry-1
    )

  (import
    (chezscheme)
    )

  (define (id x)
    x)

  (define (compose f g)
    (lambda (x)
      (f (g x))))

  (define (on cmp f)
    (lambda (x y)
      (cmp (f x) (f y))))

  (define (number-cmp x y)
    (cond
      [(< x y) 'lt]
      [(> x y) 'gt]
      [(= x y) 'eq]))

  (define (string-cmp x y)
    (cond
      [(string<? x y) 'lt]
      [(string>? x y) 'gt]
      [(string=? x y) 'eq]))

  (define (symbol-cmp x y)
    (string-cmp (symbol->string x) (symbol->string y)))

  (define (boolean-cmp x y)
    ; #f < #t
    (cond
      [(eq? x y) 'eq]
      [x 'gt]
      [y 'lt]))

  (define (vector-cmp x y)
    (let ([r (number-cmp (vector-length x) (vector-length y))])
      (if (not (eq? r 'eq)) r
        (cmp (vector->list x) (vector->list y)))))

  (define (cmp x y)
    ; boolean < number < symbol < string < vector < list
    (cond
      [(and (number? x) (number? y))
       (number-cmp x y)]
      [(eq? x y) 'eq]
      [(and (pair? x) (pair? y))
       (let ([c1 (cmp (car x) (car y))])
         (if (not (eq? c1 'eq)) c1
           (cmp (cdr x) (cdr y))))]
      [(pair? x) 'gt]
      [(pair? y) 'lt]
      [(and (vector? x) (vector? y))
       (vector-cmp x y)]
      [(and (string? x) (string? y))
       (string-cmp x y)]
      [(and (symbol? x) (symbol? y))
       (symbol-cmp x y)]
      [(and (boolean? x) (boolean? y))
       (boolean-cmp x y)]
      [(boolean? x) 'lt]
      [(boolean? y) 'gt]
      [(number? x) 'lt]
      [(number? y) 'gt]
      [(symbol? x) 'lt]
      [(symbol? y) 'gt]
      [(string? x) 'lt]
      [(string? y) 'gt]
      [(vector? x) 'lt]
      [(vector? y) 'gt]
      [else (pretty-print (format "cmp error ~a ~a" x y))]))

  (define (<? x y)
    (eq? (cmp x y) 'lt))

  (define (=? x y)
    (eq? (cmp x y) 'eq))

  (define curry
    (case-lambda
      [(f) f]
      [(f x) (lambda rs (apply f x rs))]
      [(f x y) (lambda rs (apply f x y rs))]
      [(f x y z) (lambda rs (apply f x y z rs))]))

  (define curry-1
    (case-lambda
      [(f) f]
      [(f x) (lambda (r) (f x r))]
      [(f x y) (lambda (r) (f x y r))]
      [(f x y z) (lambda (r) (f x y z r))]))

  )
