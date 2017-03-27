#!chezscheme

(library (cslib utils)

  (export
    load-libraries
    inc
    dec
    )

  (import
    (chezscheme)
    )

  (define load-libraries
    (let ()
      (load-shared-object "libc.so.6")
      (load-shared-object "clib.so")
      #t))

  (define (inc x)
    (+ 1 x))

  (define (dec x)
    (- x 1))

  )
