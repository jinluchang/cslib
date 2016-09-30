#!chezscheme

(library (cslib math)
  ; )

  (export
    pi
    inc
    dec
    sqr
    ratio
    atof
    atofi
    fib
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
    bisect-search
    adaptive-simpsons
    adaptive-simpsons-recursive-limit
    adaptive-simpsons-recursive-low-limit
    )

  (import
    (chezscheme)
    )

  (define pi 3.141592653589793)

  (define (inc x)
    (+ 1 x))

  (define (dec x)
    (- x 1))

  (define (sqr x)
    (* x x))

  (define (ratio x y)
    (/ (exact->inexact x) (exact->inexact y)))

  (define atof
    (begin
      (load-shared-object "libc.so.6")
      (foreign-procedure "atof" (string) double)))

  (define (atofi s)
    (let ([x (atof s)])
      (if (not (flinteger? x)) x
        (inexact->exact x))))

  ; -----------------------------------------------------------------------------------------------

  (define (fib n)
    (if (<= n 1) n
      (+ (fib (- n 1)) (fib (- n 2)))))

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

  ; (
  )
