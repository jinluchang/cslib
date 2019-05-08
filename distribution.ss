#!chezscheme

(library (cslib distribution)
  ; )

  (export
    make-distribution-datatable
    get-distribution-func
    make-distribution-curve-datatable
    make-distribution-curve
    make-gaussian-list!
    shift-scale-list
    make-ss-gaussian-curve-datatable
    )

  (import
    (chezscheme)
    (cslib utils)
    (cslib math)
    (cslib pmatch)
    (cslib list)
    (cslib vector)
    (cslib matrix)
    (cslib string)
    (cslib lat-io)
    (cslib tree)
    (cslib sha-rng)
    )

  ; -----------------------------------------------------------------------------------------------

  (define (make-distribution-datatable vals)
    (let* ([svals (list-sort < vals)]
           [len (length vals)]
           [is (iota len)])
      (list->vector
        (apply append
               (map
                 (lambda (i v)
                   (list
                     (vector v i)
                     (vector v (inc i))))
                 is svals)))))

  (define (get-distribution-func vals)
    (lambda (x)
      (length (filter (lambda (v) (<= v x)) vals))))

  (define (make-distribution-curve xs vals)
    (map (get-distribution-func vals) xs))

  (define (make-distribution-curve-datatable xs valss)
    (let* ([cs
             (map
               (lambda (vals)
                 (list->vector
                   (map exact->inexact (make-distribution-curve xs vals))))
               valss)])
      (vector-map
        (lambda (x v e)
          (vector x v e (- v e) (+ v e)))
        (list->vector xs)
        (apply tree-average cs)
        (apply tree-op std-deviation cs))))

  (define (make-gaussian-list! rs center sigma size)
    (let* ([vec (make-vector size)]
           [is (iota size)])
      (for-each (lambda (i) (vector-set! vec i (g-rand-gen! rs center sigma))) is)
      (vector->list vec)))

  (define (shift-scale-list vals)
    (let* ([center (apply average vals)]
           [sigma (apply std-deviation vals)])
      (map
        (lambda (v)
          (/ (- v center) sigma))
        vals)))

  (define (make-ss-gaussian-curve-datatable rs xs size hits)
    ; the list has been applied shift-scale-list
    (let* ([valss
             (map (lambda (i)
                    (make-gaussian-list!
                      (make-rng-state rs i)
                      0.0 1.0 size))
                  (iota hits))]
           [ss-valss (map shift-scale-list valss)])
      (make-distribution-curve-datatable xs ss-valss)))

  ; (
  )
