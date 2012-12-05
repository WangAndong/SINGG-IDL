PRO ssoup_transform_mask, hdi, mski, bvali, hdo, msko, limits=limits, invert=invert
  ;
  ; transform positions of bad pixels from an input mask
  ; to an output mask, with (probably) a different pixel 
  ; coord grid.
  ;
  ; hdi     -> fits header with WCS of input mask
  ; mski    -> byte array input mask. 
  ;            bad pixels have value bvali
  ; bvali   -> bad pixels in mski have this value
  ; hdo     -> fits header with wcs of output mask
  ; msko    <- byte array output mask.  
  ;            Bad pixels will have a value 1b
  ;            and good pixels will have a value 0b.  
  ; limits  -> If set the limits in the input array to transform.
  ;            This is a 4 element array with values
  ;            0 : min column
  ;            1 : max column
  ;            2 : min row
  ;            3 : max row
  ; invert  -> if set, bad pixels will have a value 0b,
  ;            and good pixels a value 1b
  ;
  ; make empty output mask
  nxx     = sxpar(hdo, 'naxis1')
  nyy     = sxpar(hdo, 'naxis2')
  IF NOT keyword_set(invert) THEN BEGIN 
     msko  = make_array(nxx, nyy, /byte, value=0b)
     bvalo = 1b
  ENDIF ELSE BEGIN 
     msko  = make_array(nxx, nyy, /byte, value=1b)
     bvalo = 0b
  ENDELSE 
  ;
  ; define limits even if not set
  lims     = float([0, nxx-1, 0, nyy-1])
  IF keyword_set(limits) THEN lims = float(limits)
  ;
  ; pointer to bad pixels in input coord sys.
  pp         = where(mski EQ bvali, npp)
  IF npp GT 0 THEN BEGIN 
     siz     = size(mski)
     ;
     ; coords of bad pixels in input optical coord sys
     xp      = pp MOD siz[1]
     yp      = fix(float(pp)/float(siz[1]))
     ;
     ; use lims to select pixels to transform
     ll      = where(xp GE lims[0] AND xp LE lims[1] AND yp GE lims[2] AND yp LE lims[3], nll)
     IF nll GT 0 THEN BEGIN 
        ;
        ; transform
        xyad, hdi, xp, yp, ap, dp       ; ra, dec
        adxy, hdo, ap, dp, xpo, ypo  ; output coord sys
        ;
        ; round to nearest pixel
        xpo     = round(xpo)
        ypo     = round(ypo)
        ;
        ; only care about pixels within valid pixel range
        qq      = where(xpo GE 0 AND xpo LE nxx-1 AND ypo GE 0 AND ypo LE nyy-1, nqq)
        IF nqq GT 0 THEN BEGIN 
           ;
           ; get uniq indecis of bad pixels
           idx           = nxx*ypo[qq]+xpo[qq]
           jj            = sort(idx)
           kk            = uniq(idx[jj])
           idx           = idx[jj[kk]]
           ;
           ; set mask pixels
           msko[idx]     = bvalo
        ENDIF 
     ENDIF 
  ENDIF
END 
