#!chezscheme

(library (cslib tree)
  ; )

  (export
    average
    std-deviation
    average-sigma
    jackknife-sigma
    all-same
    tag-pair?
    tree-op
    tree+
    tree-
    tree*
    tree-average
    j-average
    jackknife
    )

  (import
    (chezscheme)
    (cslib utils)
    (cslib math)
    (cslib pmatch)
    (cslib list)
    (cslib vector)
    (cslib matrix)
    (cslib string)
    (cslib lat-io)
    )

  ; -----------------------------------------------------------------------------------------------

  (define (average . xs)
    (if (null? xs) 0
      (/ (apply + xs) (length xs))))

  (define (std-deviation . xs)
    (define (cop op x)
      (cond
        [(real? x) (op x)]
        [(complex? x) (make-rectangular
                        (op (real-part x))
                        (op (imag-part x)))]))
    (if (null? xs) 0
      (if (null? (cdr xs)) (cop abs (car xs))
        (let ([len (length xs)]
              [avg (apply average xs)])
          (cop sqrt (/ (apply + (map (lambda (x) (cop sqr (- x avg)))
                                     xs))
                       (dec len)))))))

  (define (average-sigma . xs)
    (if (null? xs) 0
      (let ([len (length xs)])
        (/ (apply std-deviation xs) (sqrt len)))))

  (define (jackknife-sigma . xs)
    (if (null? xs) 0
      (let ([len (length xs)])
        (* (dec len) (/ (apply std-deviation xs) (sqrt len))))))

  ; -----------------------------------------------------------------------------------------------

  (define (all-same = xs)
    (pmatch xs
      [() #t]
      [(,x . ,rs)
       (for-all (lambda (r) (= x r)) rs)]))

  (define (tag-pair? x)
    (and (pair? x) (eq? 'tag (car x))))

  (define (atom-pair? x)
    (and (pair? x) (eq? 'atom (car x))))

  (define (tree-op op . ats)
    (define (top . ats)
      (apply tree-op op ats))
    (define ts (remq 'empty ats))
    (cond
      [(null? ats)
       (list)]
      [(null? ts)
       'empty]
      [(and (for-all number? ts) (for-all inexact? ts))
       (apply op ts)]
      [(and (for-all number? ts) (for-all exact? ts) (all-same = ts))
       (car ts)]
      [(and (for-all symbol? ts) (all-same eq? ts))
       (car ts)]
      [(and (for-all boolean? ts) (all-same eq? ts))
       (car ts)]
      [(and (for-all string? ts) (all-same string=? ts))
       (car ts)]
      [(for-all lat-data? ts)
       (apply lat-data-op op ts)]
      [(and (for-all tag-pair? ts) (all-same equal? ts))
       (car ts)]
      [(for-all atom-pair? ts)
       (apply op ts)]
      [(for-all list? ts)
       (apply map top ts)]
      [(for-all vector? ts)
       (apply vector-map top ts)]
      [(for-all pair? ts)
       (cons (apply top (map car ts))
             (apply top (map cdr ts)))]
      [else
        (print "warning tree-op" ts)
        (car ts)
        ]))

  (define (lat-data-op op . ts)
    (assert (for-all lat-data? ts))
    (let* ([all-dims (map lat-data-dim-sizes ts)]
           [ld-ret (make-lat-data (car ts))]
           [vs (map (lambda (ld) (lat-data-ref ld (vector))) ts)])
      (assert (all-same equal? all-dims))
      (lat-data-set! ld-ret (vector) (apply tree-op op vs))
      ld-ret))

  (define (tree+ . ts)
    (apply tree-op + ts))

  (define (tree- . ts)
    (apply tree-op - ts))

  (define (tree* fac t)
    (tree-op (lambda (x) (* fac x)) t))

  (define (tree-average . ts)
    (apply tree-op average ts))

  (define (j-average + * samples)
    (* (/ 1.0 (length samples)) (apply + samples)))

  (define (jackknife + - * samples)
    ; return list has one more element than samples
    ; the first element of the return list is the extra element
    ; the extra element is the average of samples
    (let* ([len (length samples)]
           [sum (apply + samples)]
           [psums (map (lambda (s) (- sum s)) samples)])
      (cons (* (/ 1 len) sum)
            (map (lambda (s) (* (/ 1 (dec len)) s)) psums))))

  ; (
  )
