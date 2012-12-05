FUNCTION gvrad, vr, vhel, w50
  RETURN, gaussian(vr, [1.0, vhel, w50/2.35])
END 

FUNCTION niicalc,filtfile,vhel,w50,NIIFRAC=niifrac,NIIERR=niierr, $
                 KNII=knii,SIGMA=sigma
; Calculates the [NII] correction on the fly.
; INPUTS
;   filtfile    Name of filter definition file to use, including path.
; OPTIONAL INPUT
;   niifrac     [NII(6854)]/[Halpha] ratio to begin with (default 0.35)
;   niierr      Uncertainty in NIIfrac
; OUTPUT
;   (function)  Filter-corrected NII fraction, corrected for BOTH NII lines.
; OPTIONAL OUTPUT
;   knii        [NII] K-value.  We don't use it for much.
;   sigma       Uncertainty in the function output

  IF NOT KEYWORD_SET(niifrac) THEN niifrac = 0.35
  IF NOT KEYWORD_SET(niierr) THEN niierr = 0.0

; If you pass in an array of niifrac's, only the function output will
; be an array.  Everything else will still be a single value.

  c     = 2.9978e5 ; speed of light [km/s]
  vres  = 10.0     ; step size of v_r [km/s]
  lamha = 6562.817
  lamna = 6583.45
  lamnb = 6548.05
  dlam  = lamha*vres/c ; step size, in Angstroms
   
  readcol, filtfile, lam, plam,COMMENT='#',FORMAT='F,F',/SILENT
  lhel = ((vhel/c)+1.0)*lamha
  IF lhel LT MIN(lam) OR lhel GT MAX(lam) THEN BEGIN
; This source is outside of the filtfile's range.  Assume it was an
; unintentional overlap.
    knii = 0.0
    sigma = 0.0
    RETURN,1.0 ; 1.0 means all of the flux is "Halpha".
  ENDIF

  nl    = N_ELEMENTS(lam)
  nlam  = 1 + FIX((lam[nl-1] - lam[0])/dlam)
  lam1  = lam[0] + dlam*findgen(nlam)
  plam1 = spline(lam, plam, lam1)
  vha   = c*(lam1/lamha - 1.0)
  vna   = c*(lam1/lamna - 1.0)
  vnb   = c*(lam1/lamnb - 1.0)

  gha   = gvrad(vha,vhel,w50)*dlam
  gna   = gvrad(vna,vhel,w50)*dlam
  gnb   = gvrad(vnb,vhel,w50)*dlam

  spec  = gha + MEAN(niifrac)*(1.0031*gna + 0.337*gnb)
  spec2 = spec*plam1
  spec3 = gha*plam1

  hha   = TOTAL(gha*plam1*lam1)
  hna   = TOTAL(gna*plam1*lam1)
  hnb   = TOTAL(gnb*plam1*lam1)

  knii  = (1.0031*hna + 0.337*hnb)/hha
  hfrac = 1.0/(1.0 + niifrac*knii)

  sigma = (niierr*knii) * hfrac^2.0
  kfrac = TOTAL(spec3*lam1)/TOTAL(spec2*lam1)

  RETURN,hfrac

END
