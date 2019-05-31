#!chezscheme

(library (cslib constants)

  (export
    alpha-qed
    muon-mass/mev
    electron-mass/mev
    charged-pion-mass/mev
    neutral-pion-mass/mev
    /fm/gev
    fm*gev
    )

  (import
    (chezscheme)
    )

  (define alpha-qed
    (/ 1.0 137.035999074))

  (define muon-mass/mev
    105.6583745)

  (define electron-mass/mev
    0.5109989461)

  (define /fm/gev
    0.197326979)

  (define charged-pion-mass/mev
    139.57061)

  (define neutral-pion-mass/mev
    134.9770)

  (define fm*gev
    (/ 1.0 /fm/gev))

  )
