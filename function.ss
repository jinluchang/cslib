#!chezscheme

(library (cslib function)

  (export
    id
    compose
    on 
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

  )
