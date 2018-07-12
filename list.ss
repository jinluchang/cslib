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
    take-while
    drop
    select
    flip-select
    take-drop
    block-list
    list-nref
    intersperse
    mapM
    imap
    i-for-each
    ass-lookup
    list-group
    list-gsort
    alist-gsort
    flip-map
    flip-mapM
    flip-for-each
    all
    any
    )

  (import
    (chezscheme)
    (cslib utils)
    (cslib function)
    (cslib pmatch)
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

  (define (select l . idxs)
    (map (lambda (i) (list-ref l i)) idxs))

  (define (flip-select . idxs-l)
    (apply select (last idxs-l) (init idxs-l)))

  (define (selector . idxs)
    (lambda (l) (apply select l idxs)))

  (define (last xs)
    (car (last-pair xs)))

  (define (take n xs)
    (cond
      [(null? xs) xs]
      [(<= n 0) '()]
      [(pair? xs) (cons (car xs) (take (- n 1) (cdr xs)))]))

  (define (take-while p xs)
    (cond
      [(null? xs) xs]
      [(and (pair? xs) (p (car xs)))
       (cons (car xs) (take-while p (cdr xs)))]
      [else '()]))

  (define (drop n xs)
    (cond
      [(<= n 0) xs]
      [(null? xs) xs]
      [(pair? xs) (drop (- n 1) (cdr xs))]))

  (define (take-drop n xs)
    (cond
      [(null? xs) (cons xs xs)]
      [(<= n 0) (cons '() xs)]
      [(pair? xs)
       (let ([p (take-drop (dec n) (cdr xs))])
         (cons (cons (car xs) (car p)) (cdr p)))]))

  (define (block-list n xs)
    ; n is the size of an individual block
    (let loop ([xs xs])
      (pmatch (take-drop n xs)
        [(,b . ,rs)
         (if (or (pair? rs) (= n (length b)))
           (cons b (loop rs))
           (list))])))

  (define-syntax list-nref
    (syntax-rules ()
      [(_ l i) (list-ref l i)]
      [(_ l i j ...) (list-nref (list-ref l i) j ...)]))

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

  (define (flip-map l . lsf)
    (let ([f (last lsf)]
          [ls (init lsf)])
      (apply map f l ls)))

  (define (flip-mapM l . lsf)
    (let ([f (last lsf)]
          [ls (init lsf)])
      (apply mapM f l ls)))

  (define (flip-for-each l . lsf)
    (let ([f (last lsf)]
          [ls (init lsf)])
      (apply for-each f l ls)))

  (define (all p . ls)
    (eq? #f (memp (lambda (l) (not (apply p l)))
                  (apply map list ls))))

  (define (any p . ls)
    (not (eq? #f (memp (lambda (l) (apply p l))
                       (apply map list ls)))))

  )
