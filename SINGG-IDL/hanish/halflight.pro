PRO halflight,Flux,Ferr,binA,Aend, Ahalf,Aerr, $
              FOUT=Fout,THRESH=thresh,SB=sb,SILENT=silent
; Given an input array and a peak bin in linear form,
; find the half-light radius
; INPUTS:
; Flux      Flux curve
; Ferr      Flux uncertainty
; binA      A (in pixels) as a function of bin number
; Aend      final radius (in pixels)
; OUTPUTS:
; Ahalf     Half-light radius, in pixels along the major axis
; Aerr      Uncertainty in Ahalf
; OPTIONAL INPUTS:
; Fout[2]   Flux and uncertainty outside the aperture
; thresh    Override the usual 50% cut with some other fraction
; OPTIONAL OUTPUT:
; SB[2]     Face-on surface brightness within the given radius [value,sigma]

; We want the routine to be capable of doing more than just the halflight.
  IF NOT KEYWORD_SET(thresh) THEN thresh = 0.5
  IF NOT KEYWORD_SET(Fout) THEN Fout=[0.0,0.0]

  sb = DBLARR(2)-999.0
  num_bins = N_ELEMENTS(binA)

  Amax = MIN(SQRT(abs(binA-Aend[0])),/NAN,endbin)  ; added abs GRM 5/2010
  jj     = sort(sqrt(abs(bina-Aend[0])))
  endbin = jj[0]
  endbin = (endbin - 1) < (num_bins-1)
  FluxHi = Flux + Ferr
  FluxLo = Flux - Ferr

; Default is the "invalid" result:
  Ahalf = -999.0
  Aerr = -999.0
  AHi = -999.0
  ALo = -999.0
  
; We want to trim anything beyond the peaks off, and only use the part
; where it's pretty much monotonically increasing.

; First, do the center curve.
  Fmax = MAX(Flux[0:endbin],maxbin)
  index = WHERE(Flux GT (Fmax+Fout[0])*thresh/2.0 AND $
                binA LE binA[(maxbin+1)],count)
  IF count GE 2 AND Fout[0]/Fmax LT (1.0/thresh - 1.0) THEN BEGIN
    Ahalf = findzero(Flux[index]-((Fmax+Fout[0])*thresh),binA[index])
    sb[0] = (Fmax+Fout[0])*thresh / (!pi*Ahalf^2)
  ENDIF ELSE BEGIN
    IF NOT KEYWORD_SET(silent) THEN PRINT,"ERROR in halflight: not enough data points on center curve ",count
; Return -999s
    RETURN
  ENDELSE

; Now, the high curve
  FHimax = MAX(FluxHi[0:endbin],maxhibin)
  index = WHERE(FluxHi GT (FHimax+Fout[0]+Fout[1])*thresh/2.0 AND $
                binA LE binA[(maxhibin+1)],count)
  IF count GE 2 AND (Fout[0]+Fout[1])/FHimax LT (1.0/thresh - 1.0) THEN BEGIN
    AHi = findzero(FluxHi[index]-((FHimax+Fout[0]+Fout[1])*thresh),binA[index])
    sbHi = (FHimax+Fout[0]+Fout[1])*thresh / (!pi*AHi^2)
  ENDIF ELSE BEGIN
    IF NOT KEYWORD_SET(silent) THEN PRINT,"ERROR in halflight: not enough data points on Hi curve ",count
; Return -999s
    RETURN
  ENDELSE

; And finally, the low curve.
  FLomax = MAX(FluxLo[0:endbin],maxlobin)
  index = WHERE(FluxLo GT (Flomax+Fout[0]-Fout[1])*thresh/2.0 AND $
                binA LE binA[(maxlobin+1)],count)
  IF count GE 2 AND (Fout[0]-Fout[1])/FLomax LT (1.0/thresh - 1.0) THEN BEGIN
    ALo = findzero(FluxLo[index]-((FLomax+Fout[0]-Fout[1])*thresh),binA[index])
    Aerr = ABS(AHi - ALo)/2.0

    sbLo = (FLomax+Fout[0]-Fout[1])*thresh / (!pi*ALo^2)
    sb[1] = ABS(sbHi - sbLo)/2.0
  ENDIF ELSE BEGIN
; This is the only one where we don't abort.  If Lo is invalid, just
; dump it.
    Aerr = ABS(AHi - Ahalf)
    sb[1] = ABS(sbHi - sb[0])

    IF NOT KEYWORD_SET(silent) THEN PRINT,"WARNING in halflight: not enough data points on Lo curve ",count
; Return -999s
    RETURN
  ENDELSE

  RETURN

END
