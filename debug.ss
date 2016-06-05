#!chezscheme

(library (cslib debug)

  (export
    observe
    observe-with
    observe-print
    witness
    witness-with 
    )

  (import
    (chezscheme)
    (cslib string)
    )

  (define-syntax observe
    (syntax-rules ()
      [(_ e ...) (observe-with observe-print e ...)]))

  (define (observe-print obj)
    (put-string (current-output-port) "observe:\n")
    (print obj)
    (put-string (current-output-port) "--------\n"))

  (define-syntax observe-with
    (syntax-rules ()
      [(_ f e ...)
       (let ([v (e ...)])
         (f v)
         v)]))

  (define-syntax witness
    (syntax-rules ()
      [(_ s e ...) (witness-with (display s) e ...)]))

  (define-syntax witness-with
    (syntax-rules ()
      [(_ f e ...)
       (begin f (e ...))]))

  )
