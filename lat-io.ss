#!chezscheme

(library (cslib lat-io)
  ; )

  (export
    make-lat-data
    lat-data?
    load-lat-data
    save-lat-data
    lat-data-set-zero!
    lat-data-print
    lat-data-complex?
    lat-data-dim-sizes
    lat-data-dims
    lat-data-size
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
            (make-lat-data-copy x)]
           [(lat-data-dim? x)
            (make-lat-data-dims x)]
           [(and (list? x) (lat-data-dim? (car x)))
            (apply make-lat-data-dims x)])]
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
         (= (length x) 3)
         (string? (car x))
         (number? (cadr x))
         (list? (caddr x))))

  (define make-lat-data-dims
    (let* ([_ (assert load-libraries)]
           [lat-data-add-dim!
             (foreign-procedure "clib_lat_data_add_dim"
                                (void* string long) void)]
           [lat-data-add-dim-idx!
             (foreign-procedure "clib_lat_data_add_dim_idx_name"
                                (void* int string) void)])
      (lambda dims
        (let* ([ld (make-lat-data)]
               [cld (cadr ld)])
          (for-each
            (lambda (ndim dim)
              (assert (lat-data-dim? dim))
              (lat-data-add-dim! cld (car dim) (cadr dim))
              (for-each
                (lambda (name)
                  (lat-data-add-dim-idx! cld ndim name))
                (caddr dim)))
            (iota (length dims)) dims)
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

  (define lat-data-complex?
    (let* ([_ (assert load-libraries)]
           [f (foreign-procedure "clib_lat_data_is_complex"
                                 (void*) boolean)])
      (lambda (ld)
        (assert (lat-data? ld))
        (f (cadr ld)))))

  (define load-lat-data
    (let* ([_ (assert load-libraries)]
           [f (foreign-procedure "clib_lat_data_load"
                         (void* string) void)])
      (lambda (fn)
        (let ([ld (make-lat-data)])
          (f (cadr ld) fn)
          ld))))

  (define save-lat-data
    (let* ([_ (assert load-libraries)]
           [f (foreign-procedure "clib_lat_data_save"
                                 (void* string) void)])
      (lambda (ld fn)
        (assert (lat-data? ld))
        (f (cadr ld) fn))))

  (define lat-data-print
    (let* ([_ (assert load-libraries)]
           [f (foreign-procedure "clib_lat_data_print"
                         (void*) void)])
      (lambda (ld)
        (assert (lat-data? ld))
        (f (cadr ld)))))

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

  (define lat-data-ndim
    (let* ([_ (assert load-libraries)]
           [f (foreign-procedure "clib_lat_data_ndim"
                                        (void*) int)])
      (lambda (ld)
        (assert (lat-data? ld))
        (let* ([cld (cadr ld)])
          (f cld)))))

  (define lat-data-dim-sizes
    (let* ([_ (assert load-libraries)]
           [f (foreign-procedure "clib_lat_data_dim_sizes"
                                 (void* u8* int) void)])
      (lambda (ld)
        (assert (lat-data? ld))
        (let* ([cld (cadr ld)]
               [ndim (lat-data-ndim ld)]
               [dv (make-bytevector (* 8 ndim))])
          (f cld dv ndim)
          (vector-map
            (lambda (dim)
              (bytevector-s64-native-ref dv (* 8 dim)))
            (list->vector (iota ndim)))))))

  (define lat-data-dims
    (let* ([_ (assert load-libraries)]
           [get-dim-name (foreign-procedure "clib_lat_data_dim_name"
                                            (void* int) string)]
           [get-dim-size (foreign-procedure "clib_lat_data_dim_size"
                                            (void* int) long)]
           [get-dim-idx-size (foreign-procedure "clib_lat_data_dim_idx_size"
                                                (void* int) long)]
           [get-dim-idx-name (foreign-procedure "clib_lat_data_dim_idx_name"
                                                (void* int long) string)])
      (lambda (ld)
        (assert (lat-data? ld))
        (let* ([cld (cadr ld)]
               [c? (lat-data-complex? ld)]
               [sizes (append (vector->list (lat-data-dim-sizes ld)) (if c? (list 2) (list)))]
               [ndim (length sizes)])
          (map
            (lambda (dim size)
              (assert (= size (get-dim-size cld dim)))
              (list (get-dim-name cld dim)
                    size
                    (map
                      (lambda (k)
                        (get-dim-idx-name cld dim k))
                      (iota (get-dim-idx-size cld dim)))))
            (iota ndim) sizes)))))

  (define lat-data-double-size
    (let* ([_ (assert load-libraries)]
           [f (foreign-procedure "clib_lat_data_double_size"
                                        (void* int) int)])
      (lambda (ld level)
        (assert (lat-data? ld))
        (f (cadr ld) level))))

  (define (lat-data-size ld level)
    (if (lat-data-complex? ld)
      (/ (lat-data-double-size ld level) 2)
      (lat-data-double-size ld level)))

  (define (make-bytevector-from-long-vec v)
    (let* ([n (vector-length v)]
           [bv (make-bytevector (* 8 n))]
           [_ (for-each (lambda (i) (bytevector-s64-native-set! bv (* 8 i) (vector-ref v i))) (iota n))])
      bv))
 
  (define lat-data-set!
    (let* ([_ (assert load-libraries)]
           [f (foreign-procedure "clib_lat_data_set"
                                 (void* u8* int u8* long) void)])
      (lambda (ld v dvec)
        (assert (lat-data? ld))
        (let* ([cld (cadr ld)]
               [c? (lat-data-complex? ld)]
               [level (vector-length v)]
               [dsize-max (* 8 (lat-data-double-size ld level))]
               [dsize (* 8 (if c? 2 1) (vector-length dvec))]
               [bv (make-bytevector-from-long-vec v)]
               [data (make-bytevector dsize)])
          (assert (<= dsize dsize-max))
          (if c?
            (for-each (lambda (i)
                        (let ([val (vector-ref dvec i)]
                              [offset (* 16 i)])
                          (bytevector-ieee-double-native-set! data offset (real-part val))
                          (bytevector-ieee-double-native-set! data (+ offset 8) (imag-part val))))
                      (iota (vector-length dvec)))
            (for-each (lambda (i)
                        (let ([val (vector-ref dvec i)]
                              [offset (* 8 i)])
                          (bytevector-ieee-double-native-set! data offset val)))
                      (iota (vector-length dvec))))
          (f cld bv level data dsize)))))

  (define lat-data-ref
    (let* ([_ (assert load-libraries)]
           [f (foreign-procedure "clib_lat_data_ref"
                                 (void* u8* int u8* long) void)])
      (lambda (ld v)
        (assert (lat-data? ld))
        (let* ([cld (cadr ld)]
               [c? (lat-data-complex? ld)]
               [level (vector-length v)]
               [dsize (* 8 (lat-data-double-size ld level))]
               [size (/ dsize 8 (if c? 2 1))]
               [bv (make-bytevector-from-long-vec v)]
               [data (make-bytevector dsize)])
          (f cld bv level data dsize)
          (if c?
            (vector-map
              (lambda (i)
                (let ([offset (* 16 i)])
                  (make-rectangular
                    (bytevector-ieee-double-native-ref data offset)
                    (bytevector-ieee-double-native-ref data (+ offset 8)))))
              (list->vector (iota size)))
            (vector-map
              (lambda (i)
                (let ([offset (* 8 i)])
                  (bytevector-ieee-double-native-ref data offset)))
              (list->vector (iota size))))))))

  ; (
  )
