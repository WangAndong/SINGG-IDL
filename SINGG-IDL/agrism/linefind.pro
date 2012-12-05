PRO linefind, spec, espec, nsmooth, nsig, smspec, resid, nem, nabs, $
              minsn, maxsn, x_maxsn, x_minsn, lrange=lrange, lam=lam
   ;
   ; Finds emission or absorption lines
   ;
   ; spec    -> spectrum 
   ; espec   -> error spectrum
   ; nsmooth -> smoothing scale
   ; nsig    -> find pixels in residual spec +/- nsig*espec
   ; medspec <- median smoothed spectrum
   ; resid   <- residual spectrum
   ; nem     <- number of pixels > nsig*espec in residual spec
   ; nabs    <- number of pixels < nsig*espec in residual spec
   ; maxsn   <- Maximum S/N of residuals
   ; minsn   <- Minimum S/N of residuals
   ;            These above two quatities are only evaluated 
   ;            within lrange.
   ; x_maxsn <- wavelength (or pixel) position of maximun s/n
   ; x_minsn <- wavelength (or pixel) position of minimum s/n
   ; lrange  -> valid pixel or wavelength range for finding lines
   ; lam     -> wavelength scale
   IF n_elements(lam) EQ n_elements(spec) THEN x = lam ELSE x = indgen(n_elements(spec))
   IF n_elements(lrange) EQ 2 THEN BEGIN 
      xmin = min(lrange)
      xmax = max(lrange)
   ENDIF ELSE BEGIN 
      xmin = min(x)
      xmax = max(x)
   ENDELSE 
   ;
   smspec = smooth(median(spec, nsmooth), nsmooth, /edge_truncate)
   resid  = spec - smspec
   ;
   hi     = where(resid GT nsig*espec,nem)
   lo     = where(resid LT -1.0*nsig*espec,nabs)
   ;
   IF nem GT 0 THEN  high   = where(x[hi] GE xmin AND x[hi] LE xmax,nem)
   IF nabs GT 0 THEN low    = where(x[lo] GE xmin AND x[lo] LE xmax,nabs)
   ;
   good   = where(espec GT 0.0 AND x GE xmin AND x LE xmax, count)
   IF count GT 0 THEN BEGIN 
      rat     = resid[good] / espec[good]
      t       = reverse(sort(rat))
      x_maxsn = t[0]
      x_minsn = t[count-1]
      maxsn   = rat(x_maxsn)
      minsn   = rat(x_minsn)
      x_maxsn = x[good[x_maxsn]]
      x_minsn = x[good[x_minsn]]
   ENDIF ELSE BEGIN 
      maxsn   =  0.0
      minsn   =  0.0
      x_maxsn = -1.0
      x_minsn = -1.0
   ENDELSE 
END 
