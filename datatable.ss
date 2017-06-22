#!chezscheme

(library (cslib datatable)

  (export
    datatable?
    get-datatable
    get-lines
    load-datatable
    load-lines
    read-datatable
    put-datatable
    print-datatable
    save-datatable
    show-datatable
    datatable-pad-value
    datatable-pad
    vector-pad
    show-vector-line
    read-vector-line
    datatable->table
    table->datatable
    )

  (import
    (chezscheme)
    (cslib debug)
    (cslib string)
    (cslib pmatch)
    (cslib utils)
    )

  (define (datatable? table)
    (and (vector? table)
         (< 0 (vector-length table))
         (vector? (vector-ref table 0))))

  (define (datatable->table dt)
    (map vector->list (vector->list dt)))

  (define (table->datatable t)
    (list->vector (map list->vector t)))

  (define (show-vector-line v)
    (define (show-number x)
      (if (real? x) (number->string x)
        (string-append (number->string (real-part x)) " " (number->string (imag-part x)) "i")))
    (unwords (map show-number (vector->list v))))

  (define (read-vector-line s)
    (define (read-number n)
      (let ([len (string-length n)])
        (if (string-suffix? "i" n)
          (make-rectangular 0 (exact->inexact (string->number (substring n 0 (dec len)))))
          (string->number n))))
    (define (combine xs)
      (pmatch xs
        [() (list)]
        [(,x) (guard (real? x)) (list x)]
        [(,x ,y . ,rs)
         (guard (real? x) (not (real? y)))
         (cons (+ x y) (combine rs))]
        [(,x ,y . ,rs)
         (guard (real? x) (real? y))
         (cons x (combine (cons y rs)))]
        [else
          (print "warning combine" xs)
          xs]))
    (list->vector (combine (map read-number (words s)))))

  (define (get-lines port) ; port is a input-port
    (let loop ()
      (let ([line (get-line port)])
        (cond
          [(eof-object? line) '()]
          [else (cons line (loop))]))))

  (define (get-datatable port) ; port is a input-port
    (define (fl line)
      ; (vector-map string->number (list->vector (words line)))
      (read-vector-line line)
      )
    (define (comment? line)
      (eqv? #\# (string-ref line 0)))
    (define (finalize-lines lines)
      (datatable-pad (list->vector lines)))
    (finalize-lines
      (map fl (remp comment? (get-lines port)))))

  (define datatable-pad-value
    (make-parameter #f))

  (define datatable-pad
    (case-lambda
      [(table) (datatable-pad table (datatable-pad-value))]
      [(table v)
       (if (= 0 (vector-length table))
         (vector)
         (let* ([lens (vector-map vector-length table)]
                [len (apply max (vector->list lens))])
           (vector-map (lambda (xs) (vector-pad xs len v)) table)))]))

  (define (vector-pad xs len v)
    (let ([n (vector-length xs)])
      (cond
        [(> len n)
         (do ([xsp (make-vector len v)]
              [i 0 (+ 1 i)])
           ((= i n) xsp)
           (vector-set! xsp i (vector-ref xs i)))]
        [else xs])))

  (define (load-lines path)
    (let* ([p (open-input-file path)]
           [lines (get-lines p)])
      (close-input-port p)
      lines))

  (define (load-datatable path)
    (let* ([p (open-input-file path)]
           [table (get-datatable p)])
      (close-input-port p)
      table))

  (define (read-datatable str)
    (get-datatable (open-string-input-port str)))

  (define (put-datatable port table) ; port is a output-port
    (define (fl xs)
      (put-string port
                  ; (unwords (vector->list (vector-map number->string xs)))
                  (show-vector-line xs)
                  )
      (newline port))
    (vector-for-each fl table))

  (define (print-datatable table)
    (put-datatable (current-output-port) table))

  (define (save-datatable table path)
    (call-with-output-file path (lambda (p) (put-datatable p table)) 'truncate))

  (define (show-datatable table)
    (let-values ([(port g) (open-string-output-port)])
      (put-datatable port table)
      (g)))

  )
