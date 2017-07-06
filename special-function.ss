#!chezscheme

(library (cslib special-function)
  ; )

  (export
    special-k1
    )

  (import
    (chezscheme)
    (cslib utils)
    )

  (define special-k1
    (let ()
      (define f
        (foreign-procedure
          "gsl_sf_bessel_K1" (double) double))
      (assert load-libraries)
      f))

  ; (
  )
