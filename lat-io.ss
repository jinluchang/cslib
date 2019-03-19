#!chezscheme

(library (cslib lat-io)

  (export
    make-lat-data
    lat-data?
    load-lat-data
    save-lat-data
    )

  (import
    (chezscheme)
    (cslib utils)
    )

  (define lat-data-new!
    (let ()
      (assert load-libraries)
      (foreign-procedure "clib_lat_data_new"
                         () void*)))

  (define lat-data-delete!
    (let ()
      (assert load-libraries)
      (foreign-procedure "clib_lat_data_delete"
                         (void*) void)))

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

  (define make-lat-data
    (let ([malloc-guardian (make-guardian)])
      (lambda ()
        (let f ()
          (let ([x (malloc-guardian)])
            (when x
              (lat-data-delete! (cdr x))
              (f))))
        (let ([x (cons 'lat-data (lat-data-new!))])
          (malloc-guardian x)
          x))))

  (define (lat-data? ld)
    (and (pair? ld)
         (eq? 'lat-data (car ld))))

  (define (load-lat-data fn)
    (let ([cld (lat-data-new!)])
      (lat-data-load! cld fn)
      (cons 'lat-data cld)))

  (define (save-lat-data ld fn)
    (assert (lat-data? ld))
    (lat-data-save! (cdr ld) fn))

  )
