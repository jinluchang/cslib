#!chezscheme

(library (cslib hashtable)
  ; )

  (export
    make-equal-hashtable
    hashtable-items
    hashtable-pairs
    hashtable-import!
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

  (define (hashtable-pairs hashtable)
    (with-values (hashtable-entries hashtable)
                 (lambda (ks es) (vector-map cons ks es))))

  (define (hashtable-import! hashtable pairs)
    (vector-for-each
      (lambda (p)
        (hashtable-set! hashtable (car p) (cdr p)))
      pairs))

  ; (
  )
