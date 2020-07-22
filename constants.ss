#!chezscheme

(library (cslib constants)

  (export
    alpha-qed
    muon-mass/mev
    electron-mass/mev
    charged-pion-mass/mev
    neutral-pion-mass/mev
    /fm/gev
    /gev/fm
    fm*gev
    proton-mass/mev
    neutron-mass/mev
    lambda-baryon-mass/mev
    sigma-baryon-mass/mev
    xi-baryon-mass/mev
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

  (define /gev/fm
    0.197326979)

  (define /fm/gev
    /gev/fm)

  (define charged-pion-mass/mev
    139.57061)

  (define neutral-pion-mass/mev
    134.9770)

  (define fm*gev
    (/ 1.0 /fm/gev))

  (define proton-mass/mev
    938.272081)

  (define neutron-mass/mev
    939.565413)

  (define lambda-baryon-mass/mev
    1115.683)

  (define sigma-baryon-mass/mev
    1189.37)

  (define xi-baryon-mass/mev
    1314.86)

  )
