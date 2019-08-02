#!chezscheme

(library (cslib gnuplot)

  (export
    escape
    gnuplot-png-density
    mkdtemp
    plot-save
    plot-view
    mk-plot-line
    )

  (import
    (chezscheme)
    (cslib string)
    (cslib path)
    (cslib list)
    (cslib datatable)
    )

  (define (escape str)
    (format "~a" str))

  (define (mkdtemp template)
    (get-line (list-ref (process (string-append "mktemp -d -p /tmp " (escape template))) 0)))

  (define (make-gnuplot-dir fn)
    (if (eq? fn #f) (mkdtemp "cslib-gnuplot-XXXX")
      (let* ([tdir (string-append (string-drop-suffix fn ".pdf" ".eps" ".png") ".cslib-plot-dir")])
        (delete-recursive tdir)
        (mkdir-p tdir)
        tdir)))

  (define (make-mp-to-eps-script tdir fn)
    (define strs
      (list
        ; (format "cd '~a'" (escape tdir))
        "for i in *.mp ; do"
        "fn=${i%.mp}"
        "rm mpost-job.* 2>&1"
        "TEX=latex mpost -jobname mpost-job $fn.mp"
        "for i in mpost-job.? ; do"
        "echo \"$i\""
        "mv \"$i\" \"$fn\"-\"${i#mpost-job.}\".eps"
        "done"
        "done"))
    (with-output-to-file (filepath-append tdir fn) (lambda () (for-each display-string-ln strs)) 'truncate))

  (define gnuplot-png-density
    (make-parameter 500))

  (define (make-makefile tdir fn)
    (define strs
      (list
        "all: gnuplot mpost pdf png"
        ""
        "gnuplot:"
        "\tgnuplot plotfile"
        ""
        "mpost:"
        "\tbash ./convert.sh"
        ""
        "pdf:"
        "\tepstopdf plot-0.eps"
        "\tpdftops -eps plot-0.pdf"
        ""
        "png:"
        (format
          "\tpdftoppm -r ~a -png plot-0.pdf > plot-0.png" (gnuplot-png-density))
        ))
    (with-output-to-file (filepath-append tdir fn) (lambda () (for-each display-string-ln strs)) 'truncate))

  (define (make-gnuplot-script tdir fn cmds)
    (define datatable-filenames
      (map car (filter pair? cmds)))
    (define strs
      (append
        (list
          "set term mp color latex prologues 3 amstex"
          (format "set output 'plot.mp'"))
        (filter string? cmds)))
    (with-output-to-file (filepath-append tdir fn) (lambda () (for-each display-string-ln strs)) 'truncate))

  (define (save-gnuplot-datatables tdir cmds)
    (define pairs
      (filter pair? cmds))
    (define (save-pair p)
      (save-datatable (cdr p) (filepath-append tdir (car p))))
    (for-each save-pair pairs))

  (define (make-plot fn . cmds)
    (let* ([tdir (make-gnuplot-dir fn)])
      (make-mp-to-eps-script tdir "convert.sh")
      (make-makefile tdir "Makefile")
      (make-gnuplot-script tdir "plotfile" cmds)
      (save-gnuplot-datatables tdir cmds)
      (system (format "make -C '~a' >> '~a'/log" (escape tdir) (escape tdir)))
      tdir))

  (define (get-suffix-list fn)
    (cond
      [(string-suffix? ".eps" fn)
       (cons "eps" (get-suffix-list (string-drop-suffix fn ".eps")))]
      [(string-suffix? ".pdf" fn)
       (cons "pdf" (get-suffix-list (string-drop-suffix fn ".pdf")))]
      [(string-suffix? ".png" fn)
       (cons "png" (get-suffix-list (string-drop-suffix fn ".png")))]
      [else (list)]))

  (define (save-file tdir fn)
    (let ([fn-base (string-drop-suffix fn ".eps" ".pdf" ".png")]
          [suffix-list (get-suffix-list fn)])
      (for-each
        (lambda (suffix)
          (system (format "mv '~a'/plot-0.~a '~a'.~a"
                          (escape tdir) suffix
                          (escape fn-base) suffix)))
        suffix-list)))

  (define (plot-save fn . cmds)
    (if (not (or (string-suffix? ".eps" fn) (string-suffix? ".pdf" fn) (string-suffix? ".png" fn)))
      (let ([ffn (filepath-append fn (car cmds))])
        (if (not (or (string-suffix? ".eps" ffn) (string-suffix? ".pdf" ffn) (string-suffix? ".png" ffn)))
          #f
          (apply plot-save ffn (cdr cmds))))
      (let* ([tdir (apply make-plot fn cmds)])
        (save-file tdir fn)))
    (void))

  (define (plot-view . cmds)
    (let ([tdir (apply make-plot #f cmds)])
      (system (format "epstopdf '~a'/plot-0.eps --outfile='~a'/plot-0.pdf" (escape tdir) (escape tdir)))
      (system (format "evince '~a'/plot-0.pdf >>'~a'/log 2>&1 &" (escape tdir) (escape tdir))))
    (void))

  (define (mk-plot-line plot-str . lines)
    (apply string-append plot-str " \\\n    " (intersperse ", \\\n    " (filter string? lines))))

  )
