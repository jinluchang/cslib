#!chezscheme

(library (cslib vector)

  (export
    vector-setf!
    vector-nref
    vector-nset!
    vector-nsetf!
    vector-imap
    vector-i-for-each
    vector-filter
    vector-head
    vector-last
    vector-take
    vector-drop
    vector-sum
    vector-append
    subvector
    make-matrix
    matrix-map
    matrix-imap
    matrix-for-each
    matrix-i-for-each
    matrix-cut
    matrix-transpose
    )

  (import
    (chezscheme)
    (cslib math)
    )

  (define (vector-setf! vec n f)
    (vector-set! vec n (f (vector-ref vec n))))

  (define-syntax vector-nref
    (syntax-rules ()
      [(_ v i) (vector-ref v i)]
      [(_ v i j ...) (vector-nref (vector-ref v i) j ...)]))

  (define-syntax vector-nset!
    (syntax-rules ()
      [(_ v i obj) (vector-set! v i obj)]
      [(_ v i j ... obj) (vector-nset! (vector-ref v i) j ... obj)]))

  (define-syntax vector-nsetf!
    (syntax-rules ()
      [(_ v i f) (vector-setf! v i f)]
      [(_ v i j ... f) (vector-nsetf! (vector-ref v i) j ... f)]))

  (define (vector-imap f ls . more)
    (let ([is (list->vector (iota (vector-length ls)))])
      (apply vector-map f is ls more)))

  (define (vector-i-for-each f ls . more)
    (let ([is (list->vector (iota (vector-length ls)))])
      (apply vector-for-each f is ls more)))

  (define (vector-filter f v)
    (list->vector (filter f (vector->list v))))

  (define (vector-head vec)
    (vector-ref vec 0))

  (define (vector-last vec)
    (vector-ref vec (dec (vector-length vec))))

  (define (vector-take n xs)
    (let ([len (vector-length xs)])
      (cond
        [(> len n)
         (subvector xs 0 n)]
        [else xs])))

  (define (vector-drop n xs)
    (let ([len (vector-length xs)])
      (cond
        [(> len n)
         (subvector xs n len)]
        [else (vector)])))

  (define (vector-sum v)
    (apply + (vector->list v)))

  (define (vector-append . vs)
    (list->vector (apply append (map vector->list vs))))

  (define (subvector vec start end)
    (do ([subvec (make-vector (- end start))]
         [i 0 (+ 1 i)]
         [j start (+ 1 j)]
         ) ((= j end) subvec)
      (vector-set! subvec i (vector-ref vec j))))

  (define make-matrix
    (case-lambda
      [(nrows ncols)
       (vector-map (lambda (_) (make-vector ncols)) (make-vector nrows))]
      [(nrows ncols obj)
       (vector-map (lambda (_) (make-vector ncols obj)) (make-vector nrows))]))

  (define (matrix-cut mat)
    (let* ([lens (vector-map vector-length mat)]
           [len (apply min (vector->list lens))])
      (vector-map (lambda (xs) (vector-take len xs)) mat)))

  (define (matrix-transpose mat)
    (let ([len (vector-length mat)])
      (if (= len 0)
        mat
        (let* ([width (vector-length (vector-ref mat 0))]
               [new-mat (make-vector width)])
          (do ([i 0 (+ 1 i)]) ((= i width) new-mat)
            (vector-set! new-mat i (make-vector len))
            (do ([j 0 (+ 1 j)] [row (vector-ref new-mat i)]) ((= j len))
              (vector-set! row j (vector-nref mat j i))))))))

  (define (matrix-map f . ms)
    (define (fv . vs)
      (apply vector-map f vs))
    (apply vector-map fv ms))

  (define (matrix-imap f . ms)
    (define (fv i . vs)
      (apply vector-imap (lambda (j . xs) (apply f i j xs)) vs))
    (apply vector-imap fv ms))

  (define (matrix-for-each f . ms)
    (define (fv . vs)
      (apply vector-for-each f vs))
    (apply vector-for-each fv ms))

  (define (matrix-i-for-each f . ms)
    (define (fv i . vs)
      (apply vector-i-for-each (lambda (j . xs) (apply f i j xs)) vs))
    (apply vector-i-for-each fv ms))

  )
