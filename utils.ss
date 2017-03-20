#!chezscheme

(library (cslib utils)

  (export
    load-libraries
    )

  (import
    (chezscheme)
    )

  (define load-libraries
    (let ()
      (load-shared-object "libc.so.6")
      (load-shared-object "clib.so")
      #t))

  )
