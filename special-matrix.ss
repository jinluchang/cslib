#!chezscheme

(library (cslib special-matrix)
  ; )

  (export
    unit-2
    unit-3
    sigma
    gamma
    gamma-cps
    gamma-cps-wm
    gamma-5
    gamma-wm-5
    )

  (import
    (chezscheme)
    (cslib utils)
    (cslib matrix)
    )

  (define unit-2
    (make-cmatrix-id 2))

  (define unit-3
    (make-cmatrix-id 3))

  (define sigma
    (let ()
      (define sigma-x
        (vector
          (vector 0 1)
          (vector 1 0)))
      (define sigma-y
        (vector
          (vector 0 -i)
          (vector +i 0)))
      (define sigma-z
        (vector
          (vector 1 0)
          (vector 0 -1)))
      (define unit
        (make-matrix-id 2))
      (define sigmas
        (vector-map make-cmatrix (vector sigma-x sigma-y sigma-z unit)))
      (case-lambda
        [() sigmas]
        [(i) (vector-ref sigmas i)])))

  (define gamma
    (let ()
      (define gamma-t
        (vector
          (vector 0 1)
          (vector 1 0)))
      (define gamma-s
        (matrix*
          -i
          (vector
            (vector 0 1)
            (vector -1 0))))
      (define gammas
        (vector-map (lambda (m s) (cmatrix-kronecker-product (make-cmatrix m) s)) (vector gamma-s gamma-s gamma-s gamma-t) (sigma)))
      (case-lambda
        [() gammas]
        [(mu) (vector-ref gammas mu)])))

  (define gamma-cps
    (let ()
      (define gammas
        (vector (cmatrix- (gamma 0)) (gamma 1) (cmatrix- (gamma 2)) (gamma 3)))
      (case-lambda
        [() gammas]
        [(mu) (vector-ref gammas mu)])))

  (define gamma-wm
    (let ()
      (define gammas
        (vector-map (lambda (m) (cmatrix-kronecker-product m unit-3)) (gamma)))
      (case-lambda
        [() gammas]
        [(mu) (vector-ref gammas mu)])))

  (define gamma-cps-wm
    (let ()
      (define gammas
        (vector-map (lambda (m) (cmatrix-kronecker-product m unit-3)) (gamma-cps)))
      (case-lambda
        [() gammas]
        [(mu) (vector-ref gammas mu)])))

  (define gamma-5
    (cmatrix* (gamma 0) (gamma 1) (gamma 2) (gamma 3)))

  (define gamma-wm-5
    (cmatrix-kronecker-product gamma-5 unit-3))

  ; (
  )
