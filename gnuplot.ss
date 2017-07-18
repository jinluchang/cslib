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

  (define (make-gnuplot-dir wd fn)
    (if (eq? fn #f) (mkdtemp "cslib-gnuplot-XXXX")
      (let ([tdir (filepath-append wd (string-append (string-drop-suffix fn ".pdf" ".eps") ".cslib-plot-dir"))])
        (delete-recursive tdir)
        (mkdir-p tdir)
        tdir)))

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
      (save-datatable (cdr p) (filepath-append tdir (car p))))
    (for-each save-pair pairs))

  (define (make-plot wd fn . cmds)
    (let ([tdir (make-gnuplot-dir wd fn)])
      (make-mp-to-eps-script tdir "convert.sh")
      (make-gnuplot-script tdir "plotfile" cmds)
      (save-gnuplot-datatables tdir cmds)
      (system (format "cd ~s ; pwd >>~a/log" wd tdir))
      (system (format "cd ~s ; gnuplot ~a/plotfile >>~a/log" wd tdir tdir))
      (system (format "cd ~s ; bash ~a/convert.sh >>~a/log" wd tdir tdir))
      tdir))

  (define (plot-save wd fn . cmds)
    (let ([wd (if (string=? wd "") "." wd)]
          [tdir (apply make-plot wd fn cmds)])
      (cond
        [(or (string-suffix? ".eps.pdf" fn) (string-suffix? ".pdf.eps" fn))
         (system (format "epstopdf ~a/plot-0.eps --outfile=~a/plot-0.pdf ; mv ~a/plot-0.pdf ~s/~s.pdf"
                         tdir tdir tdir wd (string-drop-suffix fn ".pdf.eps" ".eps.pdf")))
         (system (format "mv ~a/plot-0.eps ~s/~s.eps"
                         tdir wd (string-drop-suffix fn ".pdf.eps" ".eps.pdf")))]
        [(string-suffix? ".pdf" fn)
         (system (format "epstopdf ~a/plot-0.eps --outfile=~a/plot-0.pdf ; mv ~a/plot-0.pdf ~s/~s" tdir tdir tdir wd fn))]
        [(string-suffix? ".eps" fn)
         (system (format "mv ~a/plot-0.eps ~s/~s" tdir wd fn))]
        [else
          (error "plot-save" "unsupported extension")]))
    (void))

  (define (plot-view wd . cmds)
    (let ([wd (if (string=? wd "") "." wd)]
          [tdir (apply make-plot wd #f cmds)])
      (system (format "epstopdf ~a/plot-0.eps --outfile=~a/plot-0.pdf" tdir tdir))
      (system (format "evince ~a/plot-0.pdf >>~a/log 2>&1 &" tdir tdir)))
    (void))

  )
