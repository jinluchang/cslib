#!chezscheme

(library (cslib list)

  (export
    setf-car!
    setf-cdr!
    head
    tail
    init
    last
    take
    drop
    intersperse
    mapM
    imap
    i-for-each
    ass-lookup
    list-group
    list-gsort
    alist-gsort
    )

  (import
    (chezscheme)
    (cslib function)
    )

  (define (setf-car! p f)
    (set-car! p (f (car p))))

  (define (setf-cdr! p f)
    (set-cdr! p (f (cdr p))))

  (define (head xs)
    (car xs))

  (define (tail xs)
    (cdr xs))

  (define (init xs)
    (let loop ([x (car xs)]
               [rs (cdr xs)])
      (if (not (pair? rs)) rs
        (cons x (loop (car rs) (cdr rs))))))

  (define (last xs)
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
      (apply map f is ls more)))

  (define (i-for-each f ls . more)
    (let ([is (iota (length ls))])
      (apply for-each f is ls more)))

  (define (ass-lookup . tags-alist)
    (define (go tags alist)
      (if (not (pair? tags)) alist
        (let ([p (assq (car tags) alist)])
          (if (eq? #f p) #f
            (go (cdr tags) (cdr p))))))
    (let ([tags (init tags-alist)]
          [alist (last tags-alist)])
      (go tags alist)))

  (define (list-group = xs)
    (define (go x g gs xs)
      (cond
        [(null? xs) (reverse (cons g gs))]
        [(pair? xs)
         (if (= x (car xs))
           (go x (cons (car xs) g) gs (cdr xs))
           (go (car xs) (list (car xs)) (cons (reverse g) gs) (cdr xs)))]))
    (cond
      [(null? xs) xs]
      [(pair? xs) (go (car xs) (list (car xs)) (list) (cdr xs))]))

  (define (list-gsort = < xs)
    (list-group = (list-sort < xs)))

  (define (alist-gsort = < pairs)
    (map (lambda (ps)
           (cons (caar ps) (map cdr ps)))
         (list-gsort (on = car) (on < car) pairs)))

  )