#!chezscheme

(library (cslib gnuplot)

  (export
    escape
    mkdtemp
    plot-save
    plot-view
    )

  (import
    (chezscheme)
    (cslib string)
    (cslib path)
    (cslib datatable)
    )

  (define (escape str)
    (format "~s" str))

  (define (mkdtemp template)
    (get-line (list-ref (process (string-append "mktemp -d -p /tmp " (escape template))) 0)))

  (define (make-gnuplot-dir)
    (mkdtemp "cslib-gnuplot-XXXX"))

  (define (make-mp-to-eps-script tdir fn)
    (define strs
      (list
        (format "cd ~a" tdir)
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

  (define (make-gnuplot-script tdir fn cmds)
    (define datatable-filenames
      (map car (filter pair? cmds)))
    (define (fix-cmd-path cmd)
      (fold-left
        (lambda (cmd v) (string-replace-all cmd (format "'~a'" v) (format "'~a/~a'" tdir v)))
        cmd datatable-filenames))
    (define strs
      (append
        (list
          "set term mp color latex prologues 3 amstex"
          (format "set output '~a/plot.mp'" tdir))
        (map fix-cmd-path (filter string? cmds))))
    (with-output-to-file (filepath-append tdir fn) (lambda () (for-each display-string-ln strs)) 'truncate))

  (define (save-gnuplot-datatables tdir cmds)
    (define pairs
      (filter pair? cmds))
    (define (save-pair p)
      (save-datatable (filepath-append tdir (car p)) (cdr p)))
    (for-each save-pair pairs))

  (define (make-plot . cmds)
    (let ([tdir (make-gnuplot-dir)])
      (make-mp-to-eps-script tdir "convert.sh")
      (make-gnuplot-script tdir "plotfile" cmds)
      (save-gnuplot-datatables tdir cmds)
      (system (format "gnuplot ~a/plotfile >>~a/log" tdir tdir))
      (system (format "bash ~a/convert.sh >>~a/log" tdir tdir))
      tdir))

  (define (plot-save fn . cmds)
    (let ([tdir (apply make-plot cmds)])
      (if (string-suffix? ".pdf" fn)
        (system (format "epstopdf ~a/plot-0.eps --outfile=~a/plot-0.pdf ; mv ~a/plot-0.pdf ~s" tdir tdir tdir fn))
        (system (format "mv ~a/plot-0.eps ~s" tdir fn))))
    (void))

  (define (plot-view . cmds)
    (let ([tdir (apply make-plot cmds)])
      (system (format "evince ~a/plot-0.eps >>~a/log 2>&1 &" tdir tdir)))
    (void))

  )
