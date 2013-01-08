PRO ssoup_cp_calcmags, ll, mag0, dredf, snlimit, $
                       flx, eflxt, eflxc, eflxs, $
                       mag, emagt, emagc, emags, $
                       mflag, emflag, lflag, elflag, flag_trailing=flag_trailing
  ;
  ; convert linear fluxes and error arrays to magnitude or log(flux) 
  ; scale arrays.  This should work for both fluxes and surface 
  ; brightnesses.
  ;
  ;  ll      -> logical unit number for log file
  ;  mag0    -> magnitude zeropoints (an array of <nb> strings)
  ;  dredf   -> factor required to remove foreground dust extinction 
  ;             (an array of <nb> strings)
  ;  snlimit -> s/n limit variable
  ;  flx     -> flux array (dimensions [<nrings>,<nb>])
  ;  eflxt   -> total errors in flux array (dimensions [<nrings>,<nb>])
  ;  eflxc   -> continuum subtraction errors, only valid for H-alpha 
  ;             (dimensions [<nrings>])
  ;  eflxs   -> sky subtraction errors, only valid for H-alpha
  ;             (dimensions [<nrings>])
  ;  mag     <- magnitudes (and log flux of Halpha)
  ;             (dimensions [<nrings>,<nb>])
  ;  emagt   <- total errors on magnitudes (or log flux)
  ;             (dimensions [<nrings>,<nb>])
  ;  emagc   <- continuum subtraction error (log) for Halpha
  ;  emags   <- sky subtraction error (log) for Halpha
  ;  mflag   -> value used to flag magnitudes
  ;  emflag  -> value used to flag errors in magnitudes
  ;  lflag   -> value used to flag log fluxes
  ;  elflag  -> value used to flog errors in log flux
  ;  flag_trailing -> if set then flag the magnitude values of large 
  ;                   aperture/annulus that have errors flagged.  This
  ;                   will overwrite upper limits, and is useful for
  ;                   aperture integrated values.
  ;
  ; G. Meurer 5/2011 (ICRAR/UWA)
  prog    = 'SSOUP_CP_CALCMAGS: '
  ;
  plog,ll,prog,'----------------- starting '+prog+'----------------------'
  COMMON bands, band, nband, bandnam, bandavail, nbandavail, combo, ncombo 
  ;
  ; get dimensions of array, find which band is 'HALPHA'
  sz      = size(flx)
  IF sz[0] NE 2 THEN stop, prog+'**** FLX array must be 2d'
  nap     = sz[1]
  nb      = sz[2]
  ih      = where(bandavail EQ band.HALPHA)
  ;
  ; make output arrays
  mag     = 0.0*flx
  emagt   = mag
  emagc   = 0.0*eflxc
  emags   = emagc
  ;
  ; calculate signal / noise level: 
  sn    = flx / eflxt      ; total
  snc   = flx[*,ih]/eflxc  ; continuum subtraction (Halpha)
  sns   = flx[*,ih]/eflxs  ; sky subtraction + photon (Halpha)
  finsn = finite(sn)
  plog,ll,prog,'flux calibrating and foreground dust correct all bands, flagging low s/n data'
  FOR ii = 0, nb-1 DO BEGIN 
     ;
     ; define good and bad data by S/N limit
     kg       = where((sn[*,ii] GE snlimit) and (finsn[*,ii] eq 1), nkg)
     kb       = where((sn[*,ii] LT snlimit) or (finsn[*,ii] eq 0), nkb)
     ;
     ; calibrate logarithmic or mag scale surface brightness profiles
     IF nkg GT 0 THEN BEGIN 
        IF ii NE ih THEN BEGIN 
           mag[kg,ii]   = mag0[ii] - 2.5*alog10(dredf[ii]*flx[kg,ii]) 
           emagt[kg,ii] = 2.5*alog10(1.0+eflxt[kg,ii]/flx[kg,ii]) 
        ENDIF ELSE BEGIN 
           mag[kg,ii]   = mag0[ii] + alog10(dredf[ii]*flx[kg,ii])
           emagt[kg,ii] = alog10(1.0+eflxt[kg,ii]/flx[kg,ii])
           emagc[kg]    = alog10(1.0+eflxc[kg]/flx[kg,ii])
           emags[kg]    = alog10(1.0+eflxs[kg]/flx[kg,ii])
        ENDELSE 
     ENDIF
     IF nkb GT 0 THEN BEGIN 
        ;
        ; set logarithmic (mag) surface brightness to upper limits 
        ; and errors to upperlimit flags
        uplim = dredf[ii]*snlimit*eflxt[kb,ii]
        IF ii NE ih THEN BEGIN 
           mag[kb,ii]   = mag0[ii] - 2.5*alog10(uplim)
           emagt[kb,ii] = emflag
        ENDIF ELSE BEGIN 
           mag[kb,ii]   = mag0[ii] + alog10(uplim) 
           emagt[kb,ii] = elflag
           emagc[kb]    = elflag
           emags[kb]    = elflag
        ENDELSE 
     ENDIF
     IF ii EQ ih THEN BEGIN 
        ;
        ; set the continuum subtraction and sky errors for the 
        ; Halpha observations
        kg  = where(snc GE snlimit, nkg)
        kb  = where(snc LT snlimit, nkb)
        IF nkg GT 0 THEN emagc[kg] = alog10(1.0 + 1.0/snc[kg])
        IF nkb GT 0 THEN emagc[kb] = emflag
        kg  = where(sns GE snlimit, nkg)
        kb  = where(sns LT snlimit, nkb)
        IF nkg GT 0 THEN emags[kg] = alog10(1.0 + 1.0/sns[kg])
        IF nkb GT 0 THEN emags[kb] = emflag
     ENDIF 
  ENDFOR
  ;
  ; flag trailing magnitudes in each band if keyword is set
  ; **** will need to fix this for multiple galaxies...
  if keyword_set(flag_trailing) then begin 
     plog,ll,prog,'will flag bad magnitudes and log fluxes of outer apertures'
     for ii = 0, nb-1 do begin
        ;
        ; set which error flag value to check and replacement value
        if ii ne ih then begin 
           eftest = emflag
           fset   = mflag
        endif else begin 
           eftest = elflag
           fset   = lflag
        endelse
        ;
        ; start at end of array (for band) and replace mag values
        ; where error array is flagged until a non flagged vaue is 
        ; found
        jj     = nap-1
        echeck = emagt[jj,ii]
        while ((echeck eq eftest) and (jj ge 0)) do begin 
           mag[jj,ii] = fset
           jj         = jj - 1
           if jj ge 0 then echeck = emagt[jj,ii]
        endwhile
     endfor
  endif
END 
