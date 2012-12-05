PRO seg_trans,val,flags,varvals
; Translate value in segmentation mask to flag states.

  IF val EQ 0 THEN BEGIN
    flags = ["No object"]
  ENDIF ELSE BEGIN
    IF val GT 0 THEN BEGIN
      flags = ["User override","Ha:R ratio far from continuum ratio","Ellipse center", $
               "Net-image inclusion mask","No flags of any kind"]
    ENDIF ELSE BEGIN
      flags = ["User override","Within 200 pixels of edge of frame", $
               "More than twice the distance of ellipse","Size < seeing", $
               "CLASS_STAR > 0.2","FLAGS indicate star", $
               "Ha:R ratio close to continuum ratio","Saturated centerpoint", $
               "R-band magnitude < -16.0"]
    ENDELSE
  ENDELSE

  n_vars = N_ELEMENTS(flags)
  varvals = BYTARR(n_vars)
  FOR ii = 0,n_vars-1 DO BEGIN
    IF (ABS(val) MOD 2^(ii+1)) GE 2^ii THEN varvals[ii] = 1b
  ENDFOR

  RETURN
END
