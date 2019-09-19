#!chezscheme

(library (cslib math)
  ; )

  (export
    pi
    ii
    sqr
    ratio
    atof
    atofi
    make-seq
    fib
    ;
    bisect-search
    ;
    adaptive-simpsons
    adaptive-simpsons-recursive-limit
    adaptive-simpsons-recursive-low-limit
    peek-bytevector
    poke-bytevector!
    double-list->bytevector
    bytevector->double-list
    gsl-minimization
    gsl-minimization-iter
    ;
    expand-fcn
    run-expanded-fcn
    efcn-add-constraint
    sqr-minimization
    ;
    make-ss
    ss-acc
    ss-scale
    ss+
    ss-
    make-ve
    err+
    val-err-0-sample
    val-err-1-sample
    val-err
    ve-scale
    ve-inverse
    make-ve-avg
    ve+
    ve-
    )

  (import
    (chezscheme)
    (cslib utils)
    (cslib pmatch)
    (cslib list)
    (cslib vector)
    (cslib matrix)
    (cslib string)
    )

  (define pi 3.141592653589793)

  (define ii (make-rectangular 0 1))

  (define (sqr x)
    (* x x))

  (define (ratio x y)
    (/ (exact->inexact x) (exact->inexact y)))

  (define atof
    (let ()
      (assert load-libraries)
      (foreign-procedure "atof" (string) double)))

  (define (atofi s)
    (let ([x (atof s)])
      (if (not (flinteger? x)) x
        (inexact->exact x))))

  ; -----------------------------------------------------------------------------------------------

  (define (fib n)
    (if (<= n 1) n
      (+ (fib (- n 1)) (fib (- n 2)))))

  (define make-seq
    (case-lambda
      [(start end sep)
       (let ([send (+ end (* 0.5 sep))])
         (let loop ([v start])
           (if (>= v send) (list)
             (cons v (loop (+ v sep))))))]
      [(start end)
       (make-seq start end 1)]
      [(end)
       (make-seq 0 end)]))

  ; -----------------------------------------------------------------------------------------------

  (define make-ss
    (case-lambda
      [() (cons 0.0 0.0)]
      [(sum sum-sq) (cons sum sum-sq)]))

  (define (ss-acc ss v)
    (make-ss (+ v (car ss)) (+ (sqr v) (cdr ss))))

  (define (ss-scale ss a)
    (make-ss (* a (car ss)) (* (sqr a) (cdr ss))))

  (define (ss+ . sss)
    (make-ss (apply + (map car sss)) (apply + (map cdr sss))))

  (define (ss- . sss)
    (make-ss (apply - (map car sss)) (apply + (map cdr sss))))

  (define make-ve
    (case-lambda
      [() (cons 0.0 0.0)]
      [(val err) (cons val err)]))

  (define (err+ . es)
    (sqrt (apply + (map sqr es))))

  (define val-err-0-sample
    (make-parameter (lambda (ss) (cons +nan.0 +nan.0))))

  (define val-err-1-sample
    (make-parameter (lambda (ss) (cons (car ss) (sqrt (cdr ss))))))

  (define (val-err nsample)
    (cond
      [(>= 0 nsample) (val-err-0-sample)]
      [(= 1 nsample) (val-err-1-sample)]
      [else
        (lambda (ss) ; sum-sum-sq
          (let* ([sum (car ss)]
                 [sum-sq (cdr ss)]
                 [avg (ratio sum nsample)]
                 [var (- (ratio sum-sq nsample) (sqr avg))]
                 [var (abs var)]
                 [err (sqrt (ratio var (- nsample 1)))])
            (make-ve avg err)))]))

  (define (ve-scale ve a)
    (make-ve (* a (car ve)) (* a (cdr ve))))

  (define (ve-inverse ve)
    (make-ve (ratio 1.0 (car ve)) (ratio (cdr ve) (sqr (car ve)))))

  (define (make-ve-avg vs)
    (let ([ss (fold-left ss-acc (make-ss) vs)])
      ((val-err (length vs)) ss)))

  (define (ve+ . ves)
    (make-ve (apply + (map car ves)) (apply err+ (map cdr ves))))

  (define (ve- . ves)
    (make-ve (apply - (map car ves)) (apply err+ (map cdr ves))))

  ; -----------------------------------------------------------------------------------------------

  (define (bisect-search f a b err-limit)
    (define (sign-different? x y)
      (cond
        [(and (> x 0.0) (< y 0.0)) #t]
        [(and (< x 0.0) (> y 0.0)) #t]
        [else #f]))
    (define (go a b fa fb)
      (let ([len (- b a)]
            [mid (/ (+ a b) 2.0)])
        (cond
          [(>= err-limit len) (+ a (* len (/ fa (- fa fb))))]
          [else
            (let ([fm (f mid)])
              (cond
                [(= 0 fm) mid]
                [(sign-different? fa fm) (go a mid fa fm)]
                [(sign-different? fm fb) (go mid b fm fb)]
                [else #f]))])))
    (go a b (f a) (f b)))

  ; -----------------------------------------------------------------------------------------------

  (define adaptive-simpsons-recursive-limit
    (make-parameter 32))

  (define adaptive-simpsons-recursive-low-limit
    (make-parameter 8))

  (define (simpson f a b c eps s fa fb fc l ll)
    (let* ([h (- b a)]
           [d (/ (+ a c) 2.0)]
           [e (/ (+ c b) 2.0)]
           [fd (f d)]
           [fe (f e)]
           [sleft (* (/ h 12.0) (+ fa (* 4.0 fd) fc))]
           [sright (* (/ h 12.0) (+ fc (* 4.0 fe) fb))]
           [s2 (+ sleft sright)])
      (if (or (< l 0)
              (and (<= ll 0)
                   (<= (abs (- s2 s)) (* 8.0 eps))))
        (+ s2 (/ (- s2 s) 8.0))
        (+ (simpson f a c d (/ eps 2.0) sleft fa fc fd (dec l) (dec ll))
           (simpson f c b e (/ eps 2.0) sright fc fb fe (dec l) (dec ll))))))

  (define adaptive-simpsons
    (case-lambda
      [(f start end)
       (adaptive-simpsons f start end 1.0e-8)]
      [(f start end epsilon)
       (cond
         [(> start end) (- (adaptive-simpsons f end start epsilon))]
         [(= start end) 0.0]
         [(< start end)
          (cond
            [(and (= start -inf.0) (= end +inf.0))
             (+ (adaptive-simpsons f start 0.0 (/ epsilon 2.0))
                (adaptive-simpsons f 0.0 end (/ epsilon 2.0)))]
            [(= end +inf.0)
             (let ([f (lambda (x)
                        (if (= x 1.0) 0.0
                          (/ (f (+ start (/ x (- 1.0 x))))
                             (sqr (- 1.0 x)))))])
               (adaptive-simpsons f 0.0 1.0 epsilon))]
            [(= start -inf.0)
             (let ([f (lambda (x)
                        (if (= x 1.0) 0.0
                          (/ (f (- end (/ x (- 1.0 x))))
                             (sqr (- 1.0 x)))))])
               (adaptive-simpsons f 0.0 1.0 epsilon))]
            [else
              (let* ([center (/ (+ start end) 2.0)]
                     [width (- end start)]
                     [fstart (f start)]
                     [fend (f end)]
                     [fcenter (f center)]
                     [sestimate (* (/ width 6.0) (+ fstart (* 4.0 fcenter) fend))])
                (simpson f start end center epsilon sestimate fstart fend fcenter
                         (adaptive-simpsons-recursive-limit)
                         (adaptive-simpsons-recursive-low-limit)))])]
         [else (error "adaptive-simpsons" "start end comparison failed")])]))

  ; -----------------------------------------------------------------------------------------------

  (define memcpy-bv-uptr
    (let ()
      (assert load-libraries)
      (foreign-procedure "memcpy" (u8* void* size_t) void)))

  (define memcpy-uptr-bv
    (let ()
      (assert load-libraries)
      (foreign-procedure "memcpy" (void* u8* size_t) void)))

  (define (peek-bytevector size address)
    (let ([bv (make-bytevector size)])
      (memcpy-bv-uptr bv address size)
      bv))

  (define (poke-bytevector! size address bv)
    (assert (<= (bytevector-length bv) size))
    (memcpy-uptr-bv address bv size))

  (define (bytevector->double-list bv)
    (let* ([size (bytevector-length bv)]
           [_ (assert (= 0 (mod size 8)))]
           [len (/ size 8)])
      (map (lambda (i)
             (bytevector-ieee-double-native-ref bv (* 8 i)))
           (iota len))))

  (define (double-list->bytevector ds)
    (let* ([len (length ds)]
           [size (* 8 len)]
           [bv (make-bytevector size)])
      (for-each (lambda (i d)
                  (bytevector-ieee-double-native-set! bv (* 8 i) d))
                (iota len) ds)
      bv))

  (define (gsl-minimization-iter k f params step-sizes epsabs max-iter)
    (if (<= k 1)
        (gsl-minimization f params step-sizes epsabs max-iter)
        (gsl-minimization-iter
          (dec k) f
          (car (gsl-minimization f params step-sizes epsabs max-iter))
          step-sizes epsabs max-iter)))

  (define gsl-minimization
    ; input (gsl-minimization f params step-sizes epsabs max-iter)
    ; return (mini-params mini-epsabs fvalue iter)
    ; e.g.
    ; (gsl-minimization (lambda (x y) (+ (sqr (- x 5)) (sqr (- y 3)))) (list 1.0 2.0) 0.1)
    ; --> ((5.000000000000002 3.0000000000000018) 2.614726469808159e-15 6.310887241768095e-30 110)
    (let ()
      (define (make-cs-func f)
        (lambda (size address)
          (let ([ds (bytevector->double-list (peek-bytevector size address))])
            (apply f ds))))
      (define c-mini
        (foreign-procedure
          "clib_gsl_mult_minimization_nmsimplex2"
          (int void* u8* long)
          long))
      (define (mini c-f params step-sizes epsabs max-iter)
        (let* ([n-params (length params)]
               [_ (assert (= n-params (length step-sizes)))]
               [double-inputs (append params step-sizes (list epsabs 0.0))]
               [bv (double-list->bytevector double-inputs)]
               [_ (lock-object bv)]
               [iter (c-mini n-params (foreign-callable-entry-point c-f) bv max-iter)]
               [_ (unlock-object bv)]
               [double-outputs (bytevector->double-list bv)]
               [mini-params (take n-params double-outputs)]
               [epsabs-fvalue (drop (* 2 n-params) double-outputs)]
               [mini-epsabs (list-ref epsabs-fvalue 0)]
               [fvalue (list-ref epsabs-fvalue 1)])
          (list mini-params mini-epsabs fvalue iter)))
      (assert load-libraries)
      (case-lambda
        [(f params)
         (gsl-minimization f params (map (lambda (_) 1.0) params))]
        [(f params step-sizes)
         (if (number? step-sizes)
           (gsl-minimization f params (map (lambda (_) step-sizes) params))
           (gsl-minimization f params step-sizes 1.0e-8 1000000))]
        [(f params step-sizes epsabs max-iter)
         (pmatch step-sizes
           [(__ . __)
            (let* ([c-f (foreign-callable (make-cs-func f) (int void*) double)]
                   [_ (lock-object c-f)]
                   [ret (mini c-f params step-sizes epsabs max-iter)]
                   [_ (unlock-object c-f)])
              ret)]
           [,s (gsl-minimization f params (map (lambda (_) s) params) epsabs max-iter)])])))

  ; -----------------------------------------------------------------------------------------------

  (define (shift-param param idx eps)
    (define size (length param))
    (define idxs (iota size))
    (map
      (lambda (i p)
        (if (= i idx)
          (+ p eps)
          p))
      idxs param))

  (define (shift-param-2 param idx-1 eps-1 idx-2 eps-2)
    (shift-param
      (shift-param param idx-1 eps-1)
      idx-2 eps-2))

  (define (get-m-matrix fcn param eps)
    (define size (length param))
    (define idxs (list->vector (iota size)))
    (vector-map
      (lambda (i)
        (vector-map
          (lambda (j)
            (* 1/4 (/ 1 (sqr eps))
               (- (+ (fcn (shift-param-2 param i eps j eps))
                     (fcn (shift-param-2 param i (- eps) j (- eps))))
                  (+ (fcn (shift-param-2 param i (- eps) j eps))
                     (fcn (shift-param-2 param i eps j (- eps)))))))
          idxs))
      idxs))

  (define (get-b-vector fcn param eps)
    (define size (length param))
    (define idxs (list->vector (iota size)))
    (vector-map
      (lambda (i)
        (* 1/2 (/ 1 eps)
           (- (fcn (shift-param param i eps))
              (fcn (shift-param param i (- eps))))))
      idxs))

  (define (expand-fcn fcn param eps)
    (if (and (pair? fcn) (eq? 'expanded-fcn (car fcn)))
      (begin
        (when (not (equal? param (cdr (list-ref fcn 1))))
          (print (format "ERROR: expand-fcn expansion point do not match")))
        fcn)
      (list 'expanded-fcn
            (cons 'tag param)
            (get-m-matrix fcn param eps)
            (get-b-vector fcn param eps)
            (fcn param))))

  (define (run-expanded-fcn efcn)
    (if (not (and (pair? efcn) (eq? 'expanded-fcn (car efcn)))) efcn
      (let ([x0 (cdr (list-ref efcn 1))]
            [m (list-ref efcn 2)]
            [b (list-ref efcn 3)]
            [f0 (list-ref efcn 4)])
        (lambda (param)
          (define dx (list->vector (map - param x0)))
          (+ (vector-nref (matrix* 1/2 (vector dx) m (vector-map vector dx))
                          0 0)
             (vector-sum (vector-map * b dx))
             f0)))))

  (define (efcn-add-constraint efcn sigma)
    (let ([mat (list-ref efcn 2)])
      (list 'expanded-fcn
            (list-ref efcn 1)
            (matrix+ mat (make-matrix-id mat (/ 2.0 (sqr sigma))))
            (list-ref efcn 3)
            (list-ref efcn 4))))

  (define (sqr-minimization fcn param eps)
    (define efcn (expand-fcn fcn param eps))
    (define best
      (map
        -
        param
        (vector->list
          (vector-map
            (lambda (v) (real-part (vector-head v)))
            (matrix* (matrix-inv (list-ref efcn 2))
                     (vector-map vector (list-ref efcn 3)))))))
    (list best (map (lambda (_) 0) best) ((run-expanded-fcn efcn) best) 1))

  ; (
  )
