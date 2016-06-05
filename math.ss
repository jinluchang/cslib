#!chezscheme

(library (cslib math)

  (export
    pi
    inc
    dec 
    sqr
    ratio
    atof
    atofi 
    make-ss
    ss-acc
    ss+
    ss-
    make-ve
    err+
    val-err
    ve-scale
    ve-inverse
    make-ve-avg
    ve+
    ve-
    bisect-search
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

  (define make-ss
    (case-lambda
      [() (cons 0.0 0.0)]
      [(sum sum-sq) (cons sum sum-sq)]))

  (define (ss-acc ss v)
    (make-ss (+ v (car ss)) (+ (sqr v) (cdr ss))))

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

  (define (val-err nsample)
    (cond
      [(>= 0 nsample) (error "val-err" "not enough sample")]
      [(= 1 nsample) (lambda (ss) (cons (car ss) (car ss)))]
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

  )
