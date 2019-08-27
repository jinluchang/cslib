#!chezscheme

(library (cslib utils)

  (export
    load-libraries
    inc
    dec
    with-values
    set-omp-num-threads
    )

  (import
    (chezscheme)
    )

  (define load-libraries-0
    (let ()
      (load-shared-object "libc.so.6")
      (load-shared-object "clib.so")
      #t))

  (define set-omp-num-threads
    (let ([_ (assert load-libraries-0)])
      (foreign-procedure "clib_set_omp_num_threads"
                         (int) void)))

  (define load-libraries
    (let ()
      (set-omp-num-threads 1)
      #t))

  (define (inc x)
    (+ 1 x))

  (define (dec x)
    (- x 1))

  (define-syntax with-values
    (syntax-rules ()
      [(_ expr consumer)
       (call-with-values (lambda () expr) consumer)]))

  )
