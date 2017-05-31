#!chezscheme

(library (cslib io)
  ; )

  (export
    read-all
    )

  (import
    (chezscheme)
    )

  (define read-all
    (case-lambda
      [()
       (read-all (current-input-port))]
      [(port)
       (let loop ()
         (let ([obj (read port)]) 
           (if (eof-object? obj)
             (list)
             (cons obj (loop)))))]))

  ; (
  )
