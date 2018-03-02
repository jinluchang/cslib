#!chezscheme

(library (cslib gnuplot)

  (export
    escape
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

  (define (make-gnuplot-dir wd fn)
    (if (eq? fn #f) (mkdtemp "cslib-gnuplot-XXXX")
      (let* ([tdir (string-append (string-drop-suffix fn ".pdf" ".eps") ".cslib-plot-dir")]
             [wd-tdir (filepath-append wd tdir)])
        (delete-recursive wd-tdir)
        (mkdir-p wd-tdir)
        tdir)))

  (define (make-wd-tdir wd tdir)
    (if (eqv? #\/ (string-ref tdir 0)) tdir
      (filepath-append wd tdir)))

  (define (make-mp-to-eps-script wd tdir fn)
    (define strs
      (list
        (format "cd '~a'" (escape tdir))
        "for i in *.mp ; do"
        "fn=${i%.mp}"
        "rm mpost-job.* 2>&1"
        "TEX=latex mpost -jobname mpost-job $fn.mp"
        "for i in mpost-job.? ; do"
        "echo \"$i\""
        "mv \"$i\" \"$fn\"-\"${i#mpost-job.}\".eps"
        "done"
        "done"))
    (with-output-to-file (filepath-append (make-wd-tdir wd tdir) fn) (lambda () (for-each display-string-ln strs)) 'truncate))

  (define (make-gnuplot-script wd tdir fn cmds)
    (define datatable-filenames
      (map car (filter pair? cmds)))
    (define (fix-cmd-path cmd)
      (fold-left
        (lambda (cmd v) (string-replace-all cmd (format "'~a'" v) (format "'~a/~a'" (escape tdir) (escape v))))
        cmd datatable-filenames))
    (define strs
      (append
        (list
          "set term mp color latex prologues 3 amstex"
          (format "set output '~a/plot.mp'" (escape tdir)))
        (map fix-cmd-path (filter string? cmds))))
    (with-output-to-file (filepath-append (make-wd-tdir wd tdir) fn) (lambda () (for-each display-string-ln strs)) 'truncate))

  (define (save-gnuplot-datatables wd-tdir cmds)
    (define pairs
      (filter pair? cmds))
    (define (save-pair p)
      (save-datatable (cdr p) (filepath-append wd-tdir (car p))))
    (for-each save-pair pairs))

  (define (make-plot wd fn . cmds)
    (let* ([tdir (make-gnuplot-dir wd fn)]
           [wd-tdir (make-wd-tdir wd tdir)])
      (make-mp-to-eps-script wd tdir "convert.sh")
      (make-gnuplot-script wd tdir "plotfile" cmds)
      (save-gnuplot-datatables wd-tdir cmds)
      (system (format "cd '~a' ; pwd >>'~a'/log" (escape wd) (escape tdir)))
      (system (format "cd '~a' ; gnuplot '~a'/plotfile >>'~a'/log" (escape wd) (escape tdir) (escape tdir)))
      (system (format "cd '~a' ; bash '~a'/convert.sh >>'~a'/log" (escape wd) (escape tdir) (escape tdir)))
      wd-tdir))

  (define (plot-save wd fn . cmds)
    (let* ([wd (if (string=? wd "") "." wd)]
           [wd-tdir (apply make-plot wd fn cmds)])
      (cond
        [(or (string-suffix? ".eps.pdf" fn) (string-suffix? ".pdf.eps" fn))
         (system (format "epstopdf '~a'/plot-0.eps --outfile='~a'/plot-0.pdf ; mv '~a'/plot-0.pdf '~a'/'~a'.pdf"
                         (escape wd-tdir) (escape wd-tdir) (escape wd-tdir) (escape wd) (escape (string-drop-suffix fn ".pdf.eps" ".eps.pdf"))))
         (system (format "mv '~a'/plot-0.eps '~a'/'~a'.eps"
                         (escape wd-tdir) (escape wd) (escape (string-drop-suffix fn ".pdf.eps" ".eps.pdf"))))]
        [(string-suffix? ".pdf" fn)
         (system (format "epstopdf '~a'/plot-0.eps --outfile='~a'/plot-0.pdf ; mv '~a'/plot-0.pdf '~a'/'~a'"
                         (escape wd-tdir) (escape wd-tdir) (escape wd-tdir) (escape wd) (escape fn)))]
        [(string-suffix? ".eps" fn)
         (system (format "mv '~a'/plot-0.eps '~a'/'~a'" (escape wd-tdir) (escape wd) (escape fn)))]
        [else
          (error "plot-save" "unsupported extension")]))
    (void))

  (define (plot-view wd . cmds)
    (let ([wd (if (string=? wd "") "." wd)]
          [wd-tdir (apply make-plot wd #f cmds)])
      (system (format "epstopdf '~a'/plot-0.eps --outfile='~a'/plot-0.pdf" (escape wd-tdir) (escape wd-tdir)))
      (system (format "evince '~a'/plot-0.pdf >>'~a'/log 2>&1 &" (escape wd-tdir) (escape wd-tdir))))
    (void))

  (define (mk-plot-line plot-str . lines)
    (apply string-append plot-str " " (intersperse ", " lines)))

  )
