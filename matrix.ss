#!chezscheme

(library (cslib matrix)
  ; )

  (export
    vectors-dp-for-each
    make-cmatrix
    parse-cmatrix
    make-cmatrix-id
    cmatrix-nrows
    cmatrix-ncols
    cmatrix-ref
    cmatrix-set!
    cmatrix+
    cmatrix-
    cmatrix*
    cmatrix-neg
    cmatrix-inv
    cmatrix-trace
    cmatrix-kronecker-product
    make-matrix-id
    matrix-nrows
    matrix-ncols
    matrix-ref
    matrix-set!
    matrix+
    matrix-
    matrix*
    matrix-neg
    matrix-inv
    )

  (import
    (chezscheme)
    (cslib debug)
    (cslib utils)
    (cslib vector)
    )

  (define (cmatrix-nrows bv)
    (bytevector-s64-native-ref bv 0))

  (define (cmatrix-ncols bv)
    (bytevector-s64-native-ref bv 8))

  (define (cmatrix-ref bv i j)
    (let* ([ncols (cmatrix-ncols bv)]
           [idx (* 16 (+ 1 (* ncols i) j))])
      (make-rectangular (bytevector-ieee-double-native-ref bv idx)
                        (bytevector-ieee-double-native-ref bv (+ idx 8)))))

  (define (cmatrix-set! bv i j c)
    (let* ([ncols (cmatrix-ncols bv)]
           [idx (* 16 (+ 1 (* ncols i) j))])
      (bytevector-ieee-double-native-set! bv idx (real-part c))
      (bytevector-ieee-double-native-set! bv (+ idx 8) (imag-part c))))

  (define-syntax vectors-dp-for-each-partial
    ; dp stands for direct product
    (syntax-rules ()
      [(_ (f ...)) (f ...)]
      [(_ (f ...) is ijs ...)
       (vector-for-each (lambda (i)
                          (vectors-dp-for-each-partial (f ... i) ijs ...))
                        is)]))

  (define-syntax vectors-dp-for-each
    ; dp stands for direct product
    (syntax-rules ()
      [(_ f ijs ...) (vectors-dp-for-each-partial (f) ijs ...)]))

  (define make-cmatrix
    (case-lambda
      [(nrows ncols)
       (let ([bv (make-bytevector (+ 8 8 (* 16 nrows ncols)) 0)])
         (bytevector-s64-native-set! bv 0 nrows)
         (bytevector-s64-native-set! bv 8 ncols)
         bv)]
      [(mat)
       (if (number? mat) mat
         (let* ([nrows (matrix-nrows mat)]
                [ncols (matrix-ncols mat)]
                [bv (make-cmatrix nrows ncols)])
           (matrix-i-for-each
             (lambda (i j d)
               (cmatrix-set! bv i j d))
             mat)
           bv))]
      [(nrows ncols double-vector)
       (let* ([is (list->vector (iota nrows))]
              [js (list->vector (iota ncols))]
              [bv (make-cmatrix nrows ncols)])
         (vectors-dp-for-each
           (lambda (i j)
             (cmatrix-set!
               bv i j
               (make-rectangular
                 (vector-ref double-vector (* 2 (+ (* ncols i) j)))
                 (vector-ref double-vector (inc (* 2 (+ (* ncols i) j)))))))
           is js)
         bv)]))

  (define (parse-cmatrix bv)
    (if (number? bv) bv
      (let* ([nrows (cmatrix-nrows bv)]
             [ncols (cmatrix-ncols bv)]
             [is (list->vector (iota nrows))]
             [js (list->vector (iota ncols))])
        (vector-map
          (lambda (i)
            (vector-map
              (lambda (j)
                (cmatrix-ref bv i j))
              js))
          is)
        )))

  (define make-cmatrix-id
    (case-lambda
      [(m value)
       (let* ([dim (if (number? m) m (cmatrix-nrows m))]
              [bv (make-cmatrix dim dim)])
         (for-each (lambda (i) (cmatrix-set! bv i i value)) (iota dim))
         bv)]
      [(m)
       (make-cmatrix-id m 1.0+0.0i)]))

  (define cmatrix+
    (let ()
      (define plus
        (foreign-procedure
          "clib_matrix_plus"
          (u8* u8* u8*)
          void))
      (assert load-libraries)
      (case-lambda
        [(x y)
         (let* ([nrows (cmatrix-nrows x)]
                [ncols (cmatrix-ncols x)]
                [ret (make-cmatrix nrows ncols)])
           (plus ret x y)
           ret)]
        [(x . xs)
         (fold-left cmatrix+ x xs)])))

  (define cmatrix-
    (let ()
      (define minus
        (foreign-procedure
          "clib_matrix_minus"
          (u8* u8* u8*)
          void))
      (assert load-libraries)
      (case-lambda
        [(x y)
         (let* ([nrows (cmatrix-nrows x)]
                [ncols (cmatrix-ncols x)]
                [ret (make-cmatrix nrows ncols)])
           (minus ret x y)
           ret)]
        [(x . xs)
         (fold-left cmatrix- x xs)])))

  (define cmatrix*
    (let ()
      (define mult
        (foreign-procedure
          "clib_matrix_multiply"
          (u8* u8* u8*)
          void))
      (define scale
        (foreign-procedure
          "clib_matrix_scale"
          (u8* u8* double double)
          void))
      (assert load-libraries)
      (case-lambda
        [(x y)
         (if (or (number? x) (number? y))
           (if (and (number? x) (number? y)) (* x y)
             (let* ([pair (if (number? x) (cons x y) (cons y x))]
                    [alpha (car pair)]
                    [mat (cdr pair)]
                    [nrows (cmatrix-nrows mat)]
                    [ncols (cmatrix-ncols mat)]
                    [ret (make-cmatrix nrows ncols)])
               (scale ret mat (exact->inexact (real-part alpha)) (exact->inexact (imag-part alpha)))
               ret))
           (let* ([nrows (cmatrix-nrows x)]
                  [ncols (cmatrix-ncols y)]
                  [ret (make-cmatrix nrows ncols)])
             (mult ret x y)
             ret))]
        [(x . xs)
         (fold-left cmatrix* x xs)])))

  (define cmatrix-neg
    (let ()
      (define neg
        (foreign-procedure
          "clib_matrix_negate"
          (u8* u8*)
          void))
      (assert load-libraries)
      (lambda (x)
        (let* ([nrows (cmatrix-nrows x)]
               [ncols (cmatrix-ncols x)]
               [ret (make-cmatrix nrows ncols)])
          (neg ret x)
          ret))))

  (define cmatrix-inv
    (let ()
      (define inv
        (foreign-procedure
          "clib_matrix_inverse"
          (u8* u8*)
          void))
      (assert load-libraries)
      (lambda (x)
        (let* ([nrows (cmatrix-nrows x)]
               [ncols (cmatrix-ncols x)]
               [ret (make-cmatrix nrows ncols)])
          (inv ret x)
          ret))))

  (define cmatrix-trace
    (let ()
      (define inv
        (foreign-procedure
          "clib_matrix_trace"
          (u8* u8*)
          void))
      (assert load-libraries)
      (lambda (x)
        (let* ([ret (make-bytevector 16)])
          (inv ret x)
          (make-rectangular (bytevector-ieee-double-native-ref ret 0)
                            (bytevector-ieee-double-native-ref ret 8))))))

  (define (cmatrix-kronecker-product x y)
    (let* ([xn (cmatrix-nrows x)]
           [xm (cmatrix-ncols x)]
           [yn (cmatrix-nrows y)]
           [ym (cmatrix-ncols y)]
           [n (* xn yn)]
           [m (* xm ym)]
           [z (make-cmatrix n m)]
           [xis (list->vector (iota xn))]
           [xjs (list->vector (iota xm))]
           [yis (list->vector (iota yn))]
           [yjs (list->vector (iota ym))])
      (vectors-dp-for-each
        (lambda (xi xj yi yj)
          (let* ([i (+ (* xi yn) yi)]
                 [j (+ (* xj ym) yj)])
            (cmatrix-set! z i j (* (cmatrix-ref x xi xj) (cmatrix-ref y yi yj)))))
        xis xjs yis yjs)
      z))

  (define (matrix-nrows m)
    (vector-length m))

  (define (matrix-ncols m)
    (vector-length (vector-ref m 0)))

  (define (matrix-ref m i j)
    (vector-nref m i j))

  (define (matrix-set! m i j c)
    (vector-nset! m i j c))

  (define (matrix+ . xs)
    (parse-cmatrix (apply cmatrix+ (map make-cmatrix xs))))

  (define (matrix- . xs)
    (parse-cmatrix (apply cmatrix- (map make-cmatrix xs))))

  (define (matrix* . xs)
    (parse-cmatrix (apply cmatrix* (map make-cmatrix xs))))

  (define (matrix-neg x)
    (parse-cmatrix (cmatrix-neg (make-cmatrix x))))

  (define (matrix-inv x)
    (parse-cmatrix (cmatrix-inv (make-cmatrix x))))

  (define make-matrix-id
    (case-lambda
      [(m value)
       (let* ([dim (if (number? m) m (matrix-nrows m))]
              [bv (make-cmatrix dim dim)])
         (for-each (lambda (i) (cmatrix-set! bv i i value)) (iota dim))
         (parse-cmatrix bv))]
      [(m)
       (make-matrix-id m 1.0+0.0i)]))

  (define (matrix-kronecker-product x y)
    (parse-cmatrix (cmatrix-kronecker-product (make-cmatrix x) (make-cmatrix y))))

  ; (
  )
