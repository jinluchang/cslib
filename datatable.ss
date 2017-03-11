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
    )

  (import
    (chezscheme)
    (cslib string)
    )

  (define (datatable? table)
    (and (vector? table)
         (< 0 (vector-length table))
         (vector? (vector-ref table 0))))

  (define (get-lines port) ; port is a input-port
    (let loop ()
      (let ([line (get-line port)])
        (cond
          [(eof-object? line) '()]
          [else (cons line (loop))]))))

  (define (get-datatable port) ; port is a input-port
    (define (fl line)
      (vector-map string->number (list->vector (words line))))
    (define (comment? line)
      (eqv? #\# (string-ref line 0)))
    (define (finalize-lines lines)
      (datatable-pad (list->vector lines)))
    (finalize-lines
      (let loop ()
        (let ([line (get-line port)])
          (cond
            [(eof-object? line) '()]
            [(comment? line) (loop)]
            [else (cons (fl line) (loop))])))))

  (define datatable-pad-value
    (make-parameter #f))

  (define datatable-pad
    (case-lambda
      [(table) (datatable-pad table (datatable-pad-value))]
      [(table v)
       (let* ([lens (vector-map vector-length table)]
              [len (apply max (vector->list lens))])
         (vector-map (lambda (xs) (vector-pad xs len v)) table))]))

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
      (put-string port (unwords (vector->list (vector-map number->string xs))))
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
