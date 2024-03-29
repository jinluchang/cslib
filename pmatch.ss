#!chezscheme

(library (cslib pmatch)

  (export
    pmatch
    )

  (import
    (chezscheme)
    )

  ; Code written by Oleg Kiselyov
  ; (http://pobox.com/~oleg/ftp/)
  ;
  ; Taken from leanTAP.scm
  ; http://kanren.cvs.sourceforge.net/kanren/kanren/mini/leanTAP.scm?view=log

  ; Taken from https://github.com/yinwang0/lightsabers/blob/master/pmatch.scm

  ; A simple linear pattern matcher
  ; It is efficient (generates code at macro-expansion time) and simple.

  ; (pmatch exp <clause> ...[<else-clause>])
  ; <clause> ::= (<pattern> exp ...)
  ;            | (<pattern> (guard <guard> ..) exp ...)
  ; <else-clause> ::= (else exp ...)
  ; <guard> ::= boolean-exp
  ; <pattern> :: =
  ;        ,var  -- matches always and binds the var
  ;                 pattern must be linear! No check is done
  ;        __    -- matches always
  ;        'exp  -- comparison with exp (using equal?)
  ;        exp   -- comparison with exp (using equal?)
  ;        (<pattern1> <pattern2> ...) -- matches the list of patterns
  ;        (<pattern1> . <pattern2>)  -- ditto
  ;        ()    -- matches the empty list

  (define-syntax pmatch
    (syntax-rules (else guard __)
      [(_ (rator rand ...) cs ...)
       (let ([v (rator rand ...)])
         (pmatch v cs ...))]
      [(_ v) (error 'pmatch "failed: ~s" v)]
      [(_ v (else e0 e ...)) (begin e0 e ...)]
      [(_ v (pat (guard g ...) e0 e ...) cs ...)
       (let ([fk (lambda () (pmatch v cs ...))])
         (ppat v pat (if (and g ...) (begin e0 e ...) (fk)) (fk)))]
      [(_ v (pat e0 e ...) cs ...)
       (let ([fk (lambda () (pmatch v cs ...))])
         (ppat v pat (begin e0 e ...) (fk)))]))

  (define-syntax ppat
    (syntax-rules (__ quote unquote)
      [(_ v __ kt kf) kt]
      [(_ v () kt kf) (if (null? v) kt kf)]
      [(_ v (quote lit) kt kf) (if (equal? v (quote lit)) kt kf)]
      [(_ v (unquote var) kt kf) (let ([var v]) kt)]
      [(_ v (x . y) kt kf)
       (if (pair? v)
         (let ([vx (car v)] [vy (cdr v)])
           (ppat vx x (ppat vy y kt kf) kf))
         kf)]
      [(_ v lit kt kf) (if (equal? v (quote lit)) kt kf)]))

  )
