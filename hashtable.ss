#!chezscheme

(library (cslib hashtable)
  ; )

  (export
    make-equal-hashtable
    )

  (import
    (chezscheme)
    )

  (define (make-equal-hashtable)
    (make-hashtable equal-hash equal?))

  ; (
  )
