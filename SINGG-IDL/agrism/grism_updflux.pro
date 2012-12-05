PRO grism_updflux, bintab, exptime=exptime, rate=rate
   ;
   ; calculate flux and eflux using sensitvity curve
   ;
   ; bintab  <> binary table to update
   ; exptime -> if set or > 0, then normalize count by exptime
   ;            before converting to count / flux. If not set,
   ;            then count column is assumed already to be in 
   ;            count rate units.
   ;
   ; G. Meurer (late 2002)
   ;
   COMMON sensmod, lams, sens, esens, hsens
   IF keyword_set(exptime) THEN BEGIN 
      IF exptime GT 0.0 THEN expt = exptime ELSE expt = 1.0
   ENDIF ELSE BEGIN
      expt     = 1.0
   ENDELSE 
   IF keyword_set(rate) THEN bintab.count = exptime*bintab.count
   count         = bintab.count 
   lam           = bintab.lambda
   nl            = n_elements(lam)
   k             = indgen(nl)
   l             = ((k+1) < (nl-1))
   dlam          = lam[l] - lam[k]
   dlam[nl-1]    = dlam[nl-2]
   slam          = interpol(sens,lams,lam)*dlam*expt
   ; slam          = interpol(sens,lams,lam)*expt
   flx           = count / slam
   errspec, bintab, eflx, /update
   eflx          = eflx / slam
   bintab.flux   = flx
   bintab.ferror = eflx
END 
