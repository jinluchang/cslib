#!chezscheme

(library (cslib threads)

  (export
    bqueue?
    make-bqueue
    bqueue-capacity
    bqueue-length
    bqueue-wait
    bqueue-de!
    bqueue-en!
    threads-job-init
    threads-job-wait
    threads-job-terminate
    threads-job-submit
    )

  (import
    (chezscheme)
    (cslib math)
    (cslib path)
    )

  (define-record-type bqueue
    (fields
      (immutable data)
      (mutable head)
      (mutable tail)
      (immutable mutex)
      (immutable ready)
      (immutable room) 
      (immutable clear))
    (protocol
      (lambda (new)
        (lambda (bound)
          (new (make-vector bound) 0 0 (make-mutex)
               (make-condition) (make-condition) (make-condition))))))

  (define bqueue-de!
    (lambda (q)
      (with-mutex (bqueue-mutex q)
        (let loop ()
          (let ([head (bqueue-head q)]
                [tail (bqueue-tail q)])
            (cond
              [(= head tail)
               (condition-signal (bqueue-clear q))
               (condition-wait (bqueue-ready q) (bqueue-mutex q))
               (loop)]
              [else
                (let ([head^ (incr q head)])
                  (bqueue-head-set! q head^)
                  (condition-signal (bqueue-room q))
                  (vector-ref (bqueue-data q) head))]))))))

  (define bqueue-en!
    (lambda (q item)
      (with-mutex (bqueue-mutex q)
        (let loop ()
          (let* ([tail (bqueue-tail q)] [tail^ (incr q tail)])
            (cond
              [(= tail^ (bqueue-head q))
               (condition-wait (bqueue-room q) (bqueue-mutex q))
               (loop)]
              [else
                (vector-set! (bqueue-data q) tail item)
                (bqueue-tail-set! q tail^)
                (condition-signal (bqueue-ready q))]))))))

  (define (bqueue-wait q)
    (with-mutex (bqueue-mutex q)
      (unless (= (bqueue-head q) (bqueue-tail q))
        (condition-wait (bqueue-clear q) (bqueue-mutex q)))))

  (define (bqueue-capacity q)
    (vector-length (bqueue-data q)))

  (define (bqueue-length q)
    (with-mutex (bqueue-mutex q)
      (let ([len (bqueue-length q)]
            [head (bqueue-head q)]
            [tail (bqueue-tail q)])
        (modulo (- tail head) len))))

  (define incr
    (lambda (q i)
      (modulo (+ i 1) (vector-length (bqueue-data q)))))

  (define-record-type counter
    (fields
      (mutable n)
      (immutable mutex)
      (immutable clear))
    (protocol
      (lambda (new)
        (lambda ()
          (new 0 (make-mutex) (make-condition))))))

  (define (counter-inc! c)
    (with-mutex (counter-mutex c)
      (counter-n-set! c (inc (counter-n c)))))

  (define (counter-dec! c)
    (with-mutex (counter-mutex c)
      (counter-n-set! c (dec (counter-n c)))
      (when (= 0 (counter-n c)) (condition-signal (counter-clear c)))))

  (define (counter-wait c)
    (with-mutex (counter-mutex c)
      (unless (= 0 (counter-n c)) (condition-wait (counter-clear c) (counter-mutex c)))))

  (define threads-worker-number
    (make-parameter 0))

  (define threads-job-bqueue
    (make-parameter #f))

  (define threads-result-queue
    (make-parameter #f))

  (define threads-job-counter
    (make-counter))

  (define (make-threads-job-thunk job-id)
    (let ([job-bqueue (threads-job-bqueue)]
          [result-queue (threads-result-queue)])
      (rec loop
        (lambda ()
          (let ([job (bqueue-de! job-bqueue)])
            (cond
              [(procedure? job)
               (bqueue-en! result-queue (job))
               (counter-dec! threads-job-counter)
               (loop)]
              [(and (pair? job) (eq? 'void (car job)))
               ((cdr job))
               (counter-dec! threads-job-counter)
               (loop)]
              [(eq? 'terminate job)
               (counter-dec! threads-job-counter)]
              [else
                (counter-dec! threads-job-counter)
                (error "threads-job" "unrecognized job")]))))))

  (define (threads-job-bqueue-en! bq job)
    (counter-inc! threads-job-counter)
    (bqueue-en! bq job))

  (define threads-job-init
    (case-lambda
      [() (threads-job-init 4 8)]
      [(n) (threads-job-init n (* 2 n))]
      [(n m)
       (threads-worker-number n)
       (threads-job-bqueue (make-bqueue m))
       (threads-result-queue (make-bqueue m))
       (for-each (lambda (job-id) (fork-thread (make-threads-job-thunk job-id))) (iota n))]))

  (define (threads-job-wait)
    (counter-wait threads-job-counter))

  (define (threads-job-terminate)
    (for-each (lambda (_) (threads-job-submit 'terminate)) (iota (threads-worker-number)))
    (threads-job-wait)
    (threads-worker-number 0))

  (define-syntax threads-job-submit
    (syntax-rules ()
      [(_ e ...)
       (let ([job-bqueue (threads-job-bqueue)])
         (if (eq? job-bqueue #f)
           (begin e ...)
           (threads-job-bqueue-en! job-bqueue
                                   (cons 'void (lambda () e ...)))))]))

  )
