#!chezscheme

(library (cslib coordinate)

  (export
    make-coordinate
    make-origin
    coordinate-dimension
    coordinate-regularize
    coordinate-relativize
    coordinate-subtract
    coordinate-add
    coordinate-length-square
    coordinate-distance-square
    coord-regularize
    coord-relativize
    )

  (import
    (chezscheme)
    (cslib vector)
    (cslib math)
    )

  (define (make-coordinate . vs)
    (list->vector vs))

  (define (make-origin dim)
    (make-vector dim 0))

  (define coordinate-dimension
    (case-lambda
      [() (error "coordinate-dimension" "no arguement")]
      [(x) (vector-length x)]
      [xs (let ([ds (map coordinate-dimension xs)])
            (cond
              [(apply = ds) (car ds)]
              [else (error "coordinate-dimension" "do not match")]))]))

  (define (coordinate-regularize size x)
    (vector-map coord-regularize size x))

  (define (coordinate-relativize size x)
    (vector-map coord-relativize size x))

  (define (coordinate-subtract size x y) ; return a relativized coordinate
    (coordinate-relativize size (vector-map - x y)))

  (define (coordinate-add size x y) ; return a regularized coordinate
    (coordinate-regularize size (vector-map + x y)))

  (define (coordinate-length-square x)
    (vector-sum (vector-map sqr x)))

  (define (coordinate-distance-square size x y)
    (coordinate-length-square (coordinate-subtract size x y)))

  (define (coord-regularize size x)
    (mod x size))

  (define (coord-relativize size x)
    (mod0 x size))

  )
