#!chezscheme

(library (cslib fork)
  ; )

  (export
    fork
    wait-pid
    fork-exec
    wait-all
    )

  (import
    (chezscheme)
    (cslib utils)
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

  ; (
  )
