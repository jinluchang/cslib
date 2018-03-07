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
    fork-for-each-map
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
      (let ([w (foreign-procedure "waitpid" (int void* int) int)]
            [update-fork-number
              (lambda (pid)
                (when (> pid 0)
                  (fork-number (dec (fork-number))))
                pid)])
        (case-lambda
          [() (update-fork-number (w -1 0 0))]
          [(pid) (update-fork-number (w pid 0 0))]))))

  (define-syntax fork-exec
    (syntax-rules ()
      [(_ e ...)
       (let* ([fn (fork-number)]
              [_ (when (>= fn (fork-limit)) (wait-pid))]
              [_ (fork-number (inc (fork-number)))]
              [pid (fork)])
         (if (not (= 0 pid)) pid
           (begin e ... (exit))))]))

  (define (wait-all)
    (when (> (wait-pid) 0)
      (wait-all)))

  (define fork-limit
    (make-parameter 8))

  (define fork-number
    (make-parameter 0))

  (define (fork-for-each f l . ls)
    (if (= (fork-limit) 1)
      (apply for-each f l ls)
      (begin
        (apply
          for-each
          (lambda ls
            (fork-exec (apply f ls)))
          l ls)
        (wait-all)
        (void))))

  ; (define (fork-for-each f l . ls)
  ;   (define np-limit (fork-limit))
  ;   (if (= np-limit 1)
  ;     (apply for-each f l ls)
  ;     (let ([vs (apply map list l ls)]
  ;           [ht (make-eqv-hashtable)])
  ;       (let loop ([np 0]
  ;                  [jobs vs])
  ;         (if (null? jobs)
  ;           (for-each wait-pid (vector->list (hashtable-keys ht)))
  ;           (if (>= np np-limit)
  ;             (begin
  ;               (hashtable-delete! ht (wait-pid))
  ;               (loop (dec np) jobs))
  ;             (begin
  ;               (hashtable-set! ht (fork-exec (apply f (car jobs))) #t)
  ;               (loop (inc np) (cdr jobs))))
  ;           ))
  ;       (void))))

  (define (flip-fork-for-each . lsf)
    (let ([f (last lsf)]
          [ls (init lsf)])
      (apply fork-for-each f ls)))

  (define (fork-for-each-map f l . ls)
    (apply fork-for-each f l ls)
    (apply map f l ls))

  ; (
  )
