#!chezscheme

(library (cslib lat-io)

  (export
    make-lat-data
    lat-data?
    load-lat-data
    save-lat-data
    lat-data-set-zero!
    lat-data-print
    lat-data-complex?
    lat-data-dims
    lat-data-ref
    lat-data-set!
    lat-data+
    lat-data-
    lat-data*
    )

  (import
    (chezscheme)
    (cslib utils)
    )

  (define make-lat-data
    (let* ([_ (assert load-libraries)]
           [lat-data-new!
             (foreign-procedure "clib_lat_data_new"
                                () void*)]
           [lat-data-delete!
             (foreign-procedure "clib_lat_data_delete"
                                (void*) void)]
           [malloc-guardian (make-guardian)])
      (case-lambda
        [()
         (let f ()
           (let ([x (malloc-guardian)])
             (when x
               (lat-data-delete! (cadr x))
               (f))))    
         (let ([x (list 'lat-data (lat-data-new!))])
           (malloc-guardian x)
           x)]
        [(x)
         (cond
           [(lat-data? x)
            (make-lat-data-copy (cadr x))]
           [(lat-data-dim? x)
            (make-lat-data-dims x)])]
        [dims
          (apply make-lat-data-dims dims)])))

  (define make-lat-data-copy
    (let* ([_ (assert load-libraries)]
           [lat-data-copy!
             (foreign-procedure "clib_lat_data_copy"
                                (void* void*) void)])
      (lambda (ld1)
        (assert (lat-data? ld1))
        (let* ([ld (make-lat-data)]
               [cld (cadr ld)])
          (lat-data-copy! cld (cadr ld1))
          ld))))

  (define (lat-data-dim? x)
    (and (list? x)
         (>= (length x) 2)
         (string? (car x))
         (number? (cadr x))))

  (define make-lat-data-dims
    (let* ([_ (assert load-libraries)]
           [lat-data-add-dim!
             (foreign-procedure "clib_lat_data_add_dim"
                                (void* string long) void)])
      (lambda dims
        (let* ([ld (make-lat-data)]
               [cld (cadr ld)])
          (for-each
            (lambda (d)
              (assert (lat-data-dim? d))
              (lat-data-add-dim! cld (car d) (cadr d)))
            dims)
          ld))))

  (define (lat-data? ld)
    (and (list? ld)
         (eq? 'lat-data (car ld))))

  (define lat-data-set-zero!
    (let* ([_ (assert load-libraries)]
           [c-set-zero! (foreign-procedure "clib_lat_data_set_zero"
                                           (void*) void)])
      (lambda (ld)
        (assert (lat-data? ld))
        (c-set-zero! (cadr ld)))))

  (define lat-data-load!
    (let ()
      (assert load-libraries)
      (foreign-procedure "clib_lat_data_load"
                         (void* string) void)))

  (define lat-data-save!
    (let ()
      (assert load-libraries)
      (foreign-procedure "clib_lat_data_save"
                         (void* string) void)))

  (define lat-data-print!
    (let ()
      (assert load-libraries)
      (foreign-procedure "clib_lat_data_print"
                         (void*) void)))

  (define lat-data-complex?
    (let* ([_ (assert load-libraries)]
           [f (foreign-procedure "clib_lat_data_is_complex"
                                 (void*) boolean)])
      (lambda (ld)
        (assert (lat-data? ld))
        (f (cadr ld)))))
 
  (define (load-lat-data fn)
    (let ([ld (make-lat-data)])
      (lat-data-load! (cadr ld) fn)
      ld))

  (define (save-lat-data ld fn)
    (assert (lat-data? ld))
    (lat-data-save! (cadr ld) fn))

  (define (lat-data-print ld)
    (assert (lat-data? ld))
    (lat-data-print! (cadr ld)))

  (define lat-data*
    (let* ([_ (assert load-libraries)]
           [f-scale! (foreign-procedure "clib_lat_data_scale"
                                        (void* void* double) void)]
           [scale (lambda (ld ld1 factor)
                    (f-scale! (cadr ld) (cadr ld1) factor)
                    ld)])
      (case-lambda
        [(x y)
         (cond
           [(and (number? x) (number? y)) (* x y)]
           [(and (number? x) (lat-data? y)) (scale (make-lat-data) y (exact->inexact x))]
           [(and (number? y) (lat-data? x)) (scale (make-lat-data) x (exact->inexact y))]
           [else (assert #f)])]
        [(x . xs)
         (fold-left lat-data* x xs)])))

  (define lat-data+
    (let* ([_ (assert load-libraries)]
           [f-plus! (foreign-procedure "clib_lat_data_plus"
                                       (void* void* void*) void)]
           [plus (lambda (ld ld1 ld2)
                   (f-plus! (cadr ld) (cadr ld1) (cadr ld2))
                   ld)])
      (case-lambda
        [(x y)
         (let* ([ret (make-lat-data)])
           (assert (lat-data? x))
           (assert (lat-data? y))
           (plus ret x y))]
        [(x . xs)
         (fold-left lat-data+ x xs)])))

  (define lat-data-
    (let* ([_ (assert load-libraries)]
           [f-minus! (foreign-procedure "clib_lat_data_minus"
                                        (void* void* void*) void)]
           [minus (lambda (ld ld1 ld2)
                    (f-minus! (cadr ld) (cadr ld1) (cadr ld2))
                    ld)])
      (case-lambda
        [(x) (lat-data* -1.0 x)]
        [(x y)
         (let* ([ret (make-lat-data)])
           (assert (lat-data? x))
           (assert (lat-data? y))
           (minus ret x y))]
        [(x . xs)
         (fold-left lat-data- x xs)])))

  (define lat-data-dims
    (let* ([_ (assert load-libraries)]
           [get-ndim (foreign-procedure "clib_lat_data_ndim"
                                        (void*) int)]
           [get-dim-name (foreign-procedure "clib_lat_data_dim_name"
                                            (void* int) string)]
           [get-dim-size (foreign-procedure "clib_lat_data_dim_size"
                                            (void* int) long)])
      (lambda (ld)
        (assert (lat-data? ld))
        (let* ([cld (cadr ld)]
               [ndim (get-ndim cld)])
          (map (lambda (dim)
                 (list (get-dim-name cld dim)
                       (get-dim-size cld dim)))
               (iota ndim))))))

  (define (make-bytevector-from-long-vec v)
    (let* ([n (vector-length v)]
           [bv (make-bytevector (* 8 n))]
           [_ (for-each (lambda (i) (bytevector-s64-native-set! bv (* 8 i) (vector-ref v i))) (iota n))])
      bv))

  (define lat-data-ref
    (let* ([_ (assert load-libraries)]
           [get-re (foreign-procedure "clib_lat_data_get"
                                      (void* u8* int) double)]
           [get-im (foreign-procedure "clib_lat_data_get_im"
                                      (void* u8* int) double)])
      (lambda (ld v)
        (let ([cld (cadr ld)]
              [n (vector-length v)]
              [bv (make-bytevector-from-long-vec v)])
          (if (lat-data-complex? ld)
            (make-rectangular (get-re cld bv n) (get-im cld bv n))
            (get-re cld bv n))))))

  (define lat-data-set!
    (let* ([_ (assert load-libraries)]
           [set-re! (foreign-procedure "clib_lat_data_set"
                                       (void* u8* int double) void)]
           [set-im! (foreign-procedure "clib_lat_data_get_im"
                                       (void* u8* int double) void)])
      (lambda (ld v val)
        (let ([cld (cadr ld)]
              [n (vector-length v)]
              [bv (make-bytevector-from-long-vec v)])
          (if (lat-data-complex? ld)
            (begin (set-re! cld bv n (exact->inexact (real-part val)))
                   (set-im! cld bv n (exact->inexact (imag-part val))))
            (set-re! cld bv n (exact->inexact val)))))))

  )
