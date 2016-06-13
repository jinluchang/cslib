#!chezscheme

(library (cslib list)

  (export
    setf-car!
    setf-cdr!
    head
    tail
    take
    drop
    intersperse
    mapM
    imap
    ass-lookup
    )

  (import
    (chezscheme)
    )

  (define (setf-car! p f)
    (set-car! p (f (car p))))

  (define (setf-cdr! p f)
    (set-cdr! p (f (cdr p))))

  (define (head xs)
    (car xs))

  (define (tail xs)
    (car (last-pair xs)))

  (define (take n xs)
    (cond
      [(null? xs) xs]
      [(<= n 0) '()]
      [(pair? xs) (cons (car xs) (take (- n 1) (cdr xs)))]))

  (define (drop n xs)
    (cond
      [(<= n 0) xs]
      [(null? xs) xs]
      [(pair? xs) (drop (- n 1) (cdr xs))]))

  (define (intersperse x vs)
    (if (null? vs)
      '()
      (let loop ([vs vs])
        (cond
          [(null? (cdr vs)) vs]
          [else (cons* (car vs) x (loop (cdr vs)))]))))

  (define (mapM f ls . more)
    (if (null? more)
      (let map ([ls ls])
        (if (null? ls)
          '()
          (cons (f (car ls))
                (map (cdr ls)))))
      (let map-more ([ls ls] [more more])
        (if (null? ls)
          '()
          (cons
            (apply f (car ls) (mapM car more))
            (map-more (cdr ls) (mapM cdr more)))))))

  (define (imap f ls . more)
    (let ([is (iota (length ls))])
      (apply map is ls more)))

  (define (ass-lookup tag alist)
    (cdr (assq tag alist)))

  )
