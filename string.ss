#!chezscheme

(library (cslib string)

  (export
    string-split-p
    words
    unwords
    lines
    unlines
    string-prefix?
    string-suffix?
    string-search
    string-search-char
    string-drop-suffix
    string-drop-prefix
    string-split
    string-replace-all
    glob-match
    take-string
    drop-string
    print
    display-ln
    display-string-ln
    )

  (import
    (chezscheme)
    (cslib utils)
    (cslib list)
    )

  (define (string-split-p str sep-char?)
    (let loop ([len (string-length str)]
               [index (dec (string-length str))]
               [ss '()])
      (if (= -1 index)
        (if (= 0 len) ss
          (cons (substring str 0 len) ss))
        (let ([c (string-ref str index)])
          (cond
            [(not (sep-char? c)) (loop len (dec index) ss)]
            [(> len (inc index)) (loop index (dec index) (cons (substring str (inc index) len) ss))]
            [(= len (inc index)) (loop (dec len) (dec index) ss)]
            [else (error "string-split-p" "index len match")])))))

  (define (words line)
    (string-split-p line char-whitespace?))

  (define (unwords ws)
    (apply string-append (intersperse " " ws)))

  (define (lines doc)
    (string-split-p doc (lambda (c) (char=? c #\newline))))

  (define (unlines ls)
    (apply string-append (intersperse "\n" ls)))

  (define (glob-match p s)
    (define pn (string-length p))
    (define (simpl n)
      (if (= pn n) (list)
        (let ([c (string-ref p n)])
          (cond
            [(char=? c #\*) (simpl* (inc n))]
            [(char=? c #\?) (cons '? (simpl (inc n)))]
            [(char=? c #\\) (simpl-q (inc n))]
            [else (cons c (simpl (inc n)))]))))
    (define (simpl-q n)
      (if (= pn n) (list #\\)
        (let ([c (string-ref p n)])
          (cond
            [(char=? c #\*) (cons #\* (simpl (inc n)))]
            [(char=? c #\?) (cons #\? (simpl (inc n)))]
            [(char=? c #\\) (cons #\\ (simpl (inc n)))]
            [else (cons* #\\ c (simpl (inc n)))]))))
    (define (simpl* n)
      (if (= pn n) (list '*)
        (let ([c (string-ref p n)])
          (cond
            [(char=? c #\*) (simpl* (inc n))]
            [(char=? c #\?) (cons '? (simpl* (inc n)))]
            [(char=? c #\\) (cons '* (simpl-q (inc n)))]
            [else (cons* '* c (simpl (inc n)))]))))
    (define ps (simpl 0))
    (define sn (string-length s))
    (define (go pl k)
      (if (null? pl) (= sn k)
        (let ([pc (car pl)])
          (cond
            [(char? pc)
             (and (< k sn)
                  (char=? pc (string-ref s k))
                  (go (cdr pl) (inc k)))]
            [(eq? pc '?)
             (and (< k sn)
                  (go (cdr pl) (inc k)))]
            [(eq? pc '*)
             (or (go (cdr pl) k)
                 (and (< k sn)
                      (go* (cdr pl) (inc k))))]))))
    (define (go* pl k)
      (or (go pl k)
          (and (< k sn)
               (go* pl (inc k)))))
    (go ps 0))

  (define (string-prefix? prefix s)
    (let ([n (string-length prefix)]
          [len (string-length s)])
      (and (>= len n)
           (let loop ([i 0])
             (or (= i n)
                 (and (char=? (string-ref s i) (string-ref prefix i))
                      (loop (+ 1 i))))))))

  (define (string-suffix? suffix s)
    (let ([n (string-length suffix)]
          [len (string-length s)])
      (and (>= len n)
           (let loop ([i 0] [j (- len n)])
             (or (= i n)
                 (and (char=? (string-ref s j) (string-ref suffix i))
                      (loop (+ 1 i) (+ 1 j))))))))

  (define string-search
    ; opitional start-pos for search start position (inclusive)
    (case-lambda
      [(pattern str) (string-search pattern str 0)]
      [(pattern str start-pos)
       (let* ((pat-len (string-length pattern))
              (search-span (- (string-length str) pat-len))
              (c1 (if (zero? pat-len) #f (string-ref pattern 0)))
              (c2 (if (<= pat-len 1) #f (string-ref pattern 1))))
         (cond
           ((not c1) start-pos)           ; empty pattern, matches upfront
           ((not c2) (string-search-char c1 str start-pos)) ; one-char pattern
           (else                  ; matching a pattern of at least two chars
             (let outer ((pos start-pos))
               (cond
                 [(> pos search-span) #f]	; nothing was found thru the whole str
                 [(not (char=? c1 (string-ref str pos)))
                  (outer (+ 1 pos))]	; keep looking for the right beginning
                 [(not (char=? c2 (string-ref str (+ 1 pos))))
                  (outer (+ 1 pos))]	; could've done pos+2 if c1 == c2....
                 [else                  	; two char matched: high probability
                   ; the rest will match too
                   (let inner ([i-pat 2] [i-str (+ 2 pos)])
                     (if (>= i-pat pat-len) pos ; whole pattern matched
                       (if (char=? (string-ref pattern i-pat)
                                   (string-ref str i-str))
                         (inner (+ 1 i-pat) (+ 1 i-str))
                         (outer (+ 1 pos)))))])))))]))

  (define string-search-char
    (case-lambda
      [(a-char str) (string-search-char a-char str 0)]
      [(a-char str start-pos)
       (let loop ((pos start-pos))
         (cond
           ((>= pos (string-length str)) #f) ; whole string has been searched, in vain
           ((char=? a-char (string-ref str pos)) pos)
           (else (loop (+ 1 pos)))))]))

  (define (string-drop-suffix s . suffixes)
    (define drop-one
      (lambda (s suffix)
        (if (string-suffix? suffix s)
          (substring s 0 (- (string-length s) (string-length suffix)))
          s)))
    (define drop-many
      (lambda (s rs)
        (cond
          [(null? rs) s]
          [(string-suffix? (car rs) s)
           (drop-many (drop-one s (car rs)) suffixes)]
          [else (drop-many s (cdr rs))])))
    (drop-many s suffixes))

  (define (string-drop-prefix s . prefixes)
    (define drop-one
      (lambda (s prefix)
        (if (string-prefix? prefix s)
          (substring s (string-length prefix) (string-length s))
          s)))
    (define drop-many
      (lambda (s rs)
        (cond
          [(null? rs) s]
          [(string-prefix? (car rs) s)
           (drop-many (drop-one s (car rs)) prefixes)]
          [else (drop-many s (cdr rs))])))
    (drop-many s prefixes))

  (define (string-split s . seps)
    (define split-one
      (lambda (s sep)
        (let ([n (string-length sep)])
          (reverse
            (let loop ([start-pos 0]
                       [ss '()])
              (let ([index (string-search sep s start-pos)])
                (cond
                  [(eq? index #f) (cons (substring s start-pos (string-length s)) ss)]
                  [(number? index) (loop (+ n index) (cons (substring s start-pos index) ss))]
                  [else (error "string-split" "string-contains index type")])))))))
    (define split-many
      (lambda (ss seps)
        (cond
          [(null? seps) ss]
          [else (let ([sep (car seps)])
                  (split-many (apply append (map (lambda (s) (split-one s (car seps))) ss)) (cdr seps)))])))
    (split-many (list s) seps))

  (define (string-replace-all str pattern replacement)
    (apply string-append (intersperse replacement (string-split str pattern))))

  (define (take-string n str)
    (substring str 0 (min n (string-length str))))

  (define (drop-string n str)
    (let ([len (string-length str)])
      (substring str (min n len) len)))

  (define (print . vs)
    (for-each pretty-print vs))

  (define display-ln
    (case-lambda
      [(x) (display x)
           (newline)]
      [(x p) (display x p)
             (newline p)]))

  (define display-string-ln
    (case-lambda
      [(x) (display-string x)
           (newline)]
      [(x p) (display-string x p)
             (newline p)]))

  )
