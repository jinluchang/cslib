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

  (define (make-makefile tdir fn fn-makefile)
    (define strs
      (list
        "all: gnuplot mpost pdf png install"
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
        ""
        "install:"
        (if (eq? fn #f) ""
            (let ([fn-base (path-last (string-drop-suffix fn ".eps" ".pdf" ".png"))]
                  [suffix-list (get-suffix-list fn)])
              (apply
                string-append
                (map
                  (lambda (suffix)
                    (format "\tmv plot-0.~a ../'~a'.~a\n"
                            suffix
                            (escape fn-base) suffix))
                  suffix-list))))))
    (with-output-to-file (filepath-append tdir fn-makefile) (lambda () (for-each display-string-ln strs)) 'truncate))

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
      (save-datatable (filepath-append tdir (car p)) (cdr p)))
    (for-each save-pair pairs))

  (define (make-plot fn . cmds)
    (let* ([tdir (make-gnuplot-dir fn)])
      (make-mp-to-eps-script tdir "convert.sh")
      (make-makefile tdir fn "Makefile")
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

  (define (plot-save fn . cmds)
    (print (format "plot '~a'" fn))
    (if (not (or (string-suffix? ".eps" fn) (string-suffix? ".pdf" fn) (string-suffix? ".png" fn)))
      (let ([ffn (filepath-append fn (car cmds))])
        (if (not (or (string-suffix? ".eps" ffn) (string-suffix? ".pdf" ffn) (string-suffix? ".png" ffn)))
          #f
          (apply plot-save ffn (cdr cmds))))
      (apply make-plot fn cmds))
    (void))

  (define (plot-view . cmds)
    (let ([tdir (apply make-plot #f cmds)])
      (system (format "epstopdf '~a'/plot-0.eps --outfile='~a'/plot-0.pdf" (escape tdir) (escape tdir)))
      (system (format "evince '~a'/plot-0.pdf >>'~a'/log 2>&1 &" (escape tdir) (escape tdir))))
    (void))

  (define (mk-plot-line plot-str . lines)
    (apply string-append plot-str " \\\n    " (intersperse ", \\\n    " (filter string? lines))))

  )
