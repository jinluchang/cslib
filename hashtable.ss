#!chezscheme

(library (cslib hashtable)
  ; )

  (export
    make-equal-hashtable
    hashtable-items
    )

  (import
    (chezscheme)
    (cslib utils)
    )

  (define (make-equal-hashtable)
    (make-hashtable equal-hash equal?))

  (define (hashtable-items hashtable)
    (with-values (hashtable-entries hashtable)
                 (lambda (ks es) es)))

  ; (
  )
