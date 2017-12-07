#!chezscheme

(library (cslib fork)
  ; )

  (export
    fork
    wait-pid
    fork-exec
    wait-all
    fork-limit
    fork-for-each
    flip-fork-for-each
    )

  (import
    (chezscheme)
    (cslib utils)
    (cslib list)
    )

  (define fork
    (let ()
      (assert load-libraries)
      (foreign-procedure "fork" () int)))

  (define wait-pid
    (let ()
      (assert load-libraries)
      (let ([w (foreign-procedure "waitpid" (int void* int) int)])
        (case-lambda
          [() (w -1 0 0)]
          [(pid) (w pid 0 0)]))))

  (define-syntax fork-exec
    (syntax-rules ()
      [(_ e ...)
       (let ([pid (fork)])
         (if (not (= 0 pid)) pid
           (begin e ... (exit))))]))

  (define (wait-all)
    (when (> (wait-pid) 0)
      (wait-all)))

  (define fork-limit
    (make-parameter 8))

  (define (fork-for-each f l . ls)
    (define np-limit (fork-limit))
    (if (= np-limit 1)
      (apply for-each f l ls)
      (let ([vs (apply map list l ls)]
            [ht (make-eqv-hashtable)])
        (let loop ([np 0]
                   [jobs vs])
          (if (null? jobs)
            (for-each wait-pid (vector->list (hashtable-keys ht)))
            (if (>= np np-limit)
              (begin
                (hashtable-delete! ht (wait-pid))
                (loop (dec np) jobs))
              (begin
                (hashtable-set! ht (fork-exec (apply f (car jobs))) #t)
                (loop (inc np) (cdr jobs))))
            ))
        (void))))

  (define (flip-fork-for-each . lsf)
    (let ([f (last lsf)]
          [ls (init lsf)])
      (apply fork-for-each f ls)))

  ; (
  )
