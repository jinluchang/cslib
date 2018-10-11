#!chezscheme

(library (cslib path)

  (export
    scpath-split
    sclist-filter
    sclist-find
    sclist-find-f
    make-scpair
    scpair-find
    scpair-find-f
    make-property-pair
    scpath-sort
    </>
    filepath-append
    directory-empty?
    directory-nonempty?
    directory-list-paths
    directory-list-directory-paths
    directory-list-paths-recursive
    delete-recursive
    ls
    ls-R
    rm
    rmdir
    rm-r
    mkdir-p
    with-cd
    with-mkdir-cd
    save-fasl-obj
    load-fasl-obj
    load-or-compute
    )

  (import
    (chezscheme)
    (cslib function)
    (cslib list)
    (cslib string)
    (cslib utils)
    )

  (define (scpath-split path . suffixs)
    (string-split (apply string-drop-suffix path suffixs) "/" " ; "))

  (define (sclist-filter property sclist)
    (define (f value)
      (string-prefix? property value))
    (define (d value)
      (substring value (string-length property) (string-length value)))
    (map d (filter f sclist)))

  (define (sclist-find default property sclist)
    (last (cons default (sclist-filter property sclist))))

  (define (sclist-find-f default f property sclist)
    (let* ([str (sclist-find #f property sclist)])
      (if (eq? #f str) default (f str))))

  (define (make-scpair path . suffixs)
    (cons path (apply scpath-split path suffixs)))

  (define (scpair-find default property scpair)
    (sclist-find default property (cdr scpair)))

  (define (scpair-find-f default f property scpair)
    (sclist-find-f default f property (cdr scpair)))

  (define (make-property-pair default f property path . suffixs)
    (cons (scpair-find-f default f property
                         (apply make-scpair path suffixs))
          path))

  (define (scpath-sort < default f property paths . suffixs)
    (map cdr
         (list-sort
           (on < car)
           (map (lambda (path)
                  (apply make-property-pair default f property path suffixs))
                paths))))

  (define </>
    (string (directory-separator)))

  (define (filepath-append . names)
    (apply string-append (intersperse </> names)))

  (define (directory-empty? dir)
    ; non-existant directory is empty
    (not (directory-nonempty? dir)))

  (define (directory-nonempty? dir)
    (and (file-directory? dir)
         (not (null? (directory-list dir)))))

  (define rm delete-file)

  (define rmdir delete-directory)

  (define ls
    (case-lambda
      [(dir)
       (if (not (file-directory? dir)) '()
         (map (lambda (file) (filepath-append dir file)) (directory-list dir)))]
      [dirs
        (apply append (map ls dirs))]))

  (define (directory-list-directory-paths path)
    (let ([files (ls path)])
      (filter file-directory? files)))

  (define (ls-R . paths)
    (with-values
      (partition file-directory? paths)
      (lambda (dirs files)
        (apply append files
               (map (lambda (dir)
                      (cons dir (apply ls-R (ls dir))))
                    dirs)))))

  (define (rm-r . paths)
    (with-values
      (partition file-directory? paths)
      (lambda (dirs files)
        (for-each rm files)
        (for-each (lambda (dir)
                    (apply rm-r (ls dir))
                    (rmdir dir))
                  dirs))))

  (define directory-list-paths ls)

  (define directory-list-paths-recursive ls-R)

  (define delete-recursive rm-r)

  (define (mkdir-p path)
    (cond
      [(= 0 (string-length path)) #t]
      [(file-directory? path) #t]
      [(mkdir-p (path-parent path)) (mkdir path)]
      [else (error "mkdir-p" "can not make")]))

  (define-syntax with-cd
    (syntax-rules ()
      [(_ dir e ...)
       (let ([cwd (cd)])
         (cd dir)
         e ...
         (cd cwd))]))

  (define-syntax with-mkdir-cd
    (syntax-rules ()
      [(_ dir e ...)
       (begin
         (mkdir-p dir)
         (with-cd dir e ...))]))

  (define (save-fasl-obj path obj)
    (mkdir-p (path-parent path))
    (let ([tpath (string-append path ".partial")])
      (call-with-port (open-file-output-port tpath (file-options no-fail)) (lambda (p) (fasl-write obj p)))
      (rename-file tpath path)))

  (define (load-fasl-obj path)
    (if (not (file-regular? path)) #f
      (call-with-port (open-file-input-port path) fasl-read)))

  (define (load-or-compute path thunk)
    (let ([result (load-fasl-obj path)])
      (if (not (eq? result #f)) result
        (let ([result (thunk)])
          (save-fasl-obj path result)
          result))))

  )
