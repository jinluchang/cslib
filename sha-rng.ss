#!chezscheme

(library (cslib sha-rng)

  (export
    sizeof-rng-state
    make-rng-state
    get-global-rng-state
    set-global-rng-state!
    rand-gen!
    u-rand-gen!
    g-rand-gen!
    rand-gen-g!
    u-rand-gen-g!
    g-rand-gen-g!
    )

  (import
    (chezscheme)
    (cslib utils)
    )

  (define sizeof-rng-state
    (let ()
      (assert load-libraries)
      (* 4 ((foreign-procedure "clib_rng_state_num_of_int32" () size_t)))))

  (define (mk-state)
    (make-bytevector sizeof-rng-state))

  (define make-rng-state
    ; Four forms:
    ; (make-rng-state)
    ; (make-rng-state rs)
    ; (make-rng-state sindex)
    ; (make-rng-state rs sindex)
    (let ()
      (define clib_set_rng_state_root
        (foreign-procedure "clib_set_rng_state_root"
                           (u8*) void))
      (define clib_set_rng_state_seed_long
        (foreign-procedure "clib_set_rng_state_seed_long"
                           (u8* long) void))
      (define clib_set_rng_state_seed_string
        (foreign-procedure "clib_set_rng_state_seed_string"
                           (u8* string) void))
      (define clib_set_rng_state_split_long
        (foreign-procedure "clib_set_rng_state_split_long"
                           (u8* u8* long) void))
      (define clib_set_rng_state_split_string
        (foreign-procedure "clib_set_rng_state_split_string"
                           (u8* u8* string) void))
      (define clib_set_rng_state_type
        (foreign-procedure "clib_set_rng_state_type"
                           (u8* unsigned-long) void))
      (assert load-libraries)
      (case-lambda
        [()
         (let ([rs (mk-state)])
           (clib_set_rng_state_root rs)
           rs)]
        [(sindex)
         (if (bytevector? sindex) (bytevector-copy sindex)
           (let ([rs (mk-state)])
             (cond
               [(number? sindex)
                (clib_set_rng_state_seed_long rs sindex)]
               [(string? sindex)
                (clib_set_rng_state_seed_string rs sindex)])
             rs))]
        [(rs0 sindex)
         (let ([rs (mk-state)])
           (cond
             [(number? sindex)
              (clib_set_rng_state_split_long rs rs0 sindex)]
             [(string? sindex)
              (clib_set_rng_state_split_string rs rs0 sindex)])
           rs)])))

  (define make-rng-state-type
    ; (set-rng-state-type! rs type)
    (let ()
      (define clib_set_rng_state_type
        (foreign-procedure "clib_set_rng_state_type"
                           (u8* unsigned-long) void))
      (assert load-libraries)
      (lambda (rs type)
        (let ([new-rs (make-rng-state rs)])
          (clib_set_rng_state_type new-rs type)
          new-rs))))

  (define get-global-rng-state
    (let ()
      (define clib_get_global_rng_state
        (foreign-procedure "clib_get_global_rng_state"
                           (u8*) void))
      (assert load-libraries)
      (lambda ()
        (let ([rs (mk-state)])
          (clib_get_global_rng_state rs)
          rs))))

  (define set-global-rng-state!
    (let ()
      (define clib_set_global_rng_state
        (foreign-procedure "clib_set_global_rng_state"
                           (u8*) void))
      (assert load-libraries)
      (lambda (rs)
        (clib_set_global_rng_state rs))))

  (define rand-gen!
    (let ()
      (define clib_rand_gen
        (foreign-procedure "clib_rand_gen"
                           (u8*) unsigned-64))
      (assert load-libraries)
      (lambda (rs)
        (clib_rand_gen rs))))

  (define u-rand-gen!
    (let ()
      (define clib_u_rand_gen
        (foreign-procedure "clib_u_rand_gen"
                           (u8* double double) double))
      (assert load-libraries)
      (case-lambda
        [(rs)
         (clib_u_rand_gen rs 1.0 0.0)]
        [(rs upper lower)
         (clib_u_rand_gen rs upper lower)])))

  (define g-rand-gen!
    (let ()
      (define clib_g_rand_gen
        (foreign-procedure "clib_g_rand_gen"
                           (u8* double double) double))
      (assert load-libraries)
      (case-lambda
        [(rs)
         (clib_g_rand_gen rs 0.0 1.0)]
        [(rs center sigma)
         (clib_g_rand_gen rs center sigma)])))

  (define rand-gen-g!
    (let ()
      (define clib_rand_gen_g
        (foreign-procedure "clib_rand_gen_g"
                           () unsigned-64))
      (assert load-libraries)
      (lambda ()
        (clib_rand_gen_g))))

  (define u-rand-gen-g!
    (let ()
      (define clib_u_rand_gen_g
        (foreign-procedure "clib_u_rand_gen_g"
                           (double double) double))
      (assert load-libraries)
      (case-lambda
        [()
         (clib_u_rand_gen_g 1.0 0.0)]
        [(upper lower)
         (clib_u_rand_gen_g upper lower)])))

  (define g-rand-gen-g!
    (let ()
      (define clib_g_rand_gen_g
        (foreign-procedure "clib_g_rand_gen_g"
                           (double double) double))
      (assert load-libraries)
      (case-lambda
        [()
         (clib_g_rand_gen_g 0.0 1.0)]
        [(center sigma)
         (clib_g_rand_gen_g center sigma)])))

  )
