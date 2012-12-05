PRO grm_avsigclip, array, sigclip, maxit, mean, sigma, nuse, nrej, nit, $
                   verbose=verbose, fixmean=fixmean, fixmean0=fixmean0
   ;
   ; Calculate sigma and rms after iterative sigma clipping
   ;
   ; array    -> array to meaure clipped average
   ; sigclip  -> factor times sigma at which to clip.  
   ;             Values within mean +/- sigclip*sigma are included.
   ; maxit    -> Maximum number of iterations
   ; mean     <> clipped mean.  
   ;             This is a retuned value unless fixmean is passed
   ; sigma    <- clipped sigma
   ; nuse     <- number of elements used
   ; nrej     <- number of elements rejected
   ; nit      <- number of iterations
   ; verbose  -> if set then results on each iteration are printed
   ; fixmean  -> if set then mean is held to be the passed value,
   ;             and only sigma is calculated.
   ; fixmean0 -> if you want the mean set to 0.0 then use /fixmean0
   ;             rather than fixmean=0.0 which IDL won't recognize as 
   ;             a passed value
   ;
   ; G. Meurer ~2000
   ; G. Meurer 09/2004 (documentation updated, fixmean added)
   ; G. Meurer 08/2007 fixed bug in fixmean logic added /fixmean0 option.
   ;
   narr    = long(n_elements(array))
   nit     = 0
   nch     = 0
   usenew  = indgen(narr, /long)
   nusenew = long(narr)
   good    = make_array(narr, /byte, value=0b)
   good[usenew] = 1b
   REPEAT BEGIN
      use     = usenew
      nuse    = nusenew
      mom     = moment(array[use], sdev=sigma)
      IF keyword_set(fixmean0) THEN mean = 0.0 ELSE $
       IF NOT keyword_set(fixmean) THEN mean    = mom[0] ELSE mean = fixmean
      resid   = array - mean
      sigma   = sqrt(total(resid[use]*resid[use])/float(nuse-1))
      usenew  = where(abs(resid) LE sigclip*sigma, nusenew)
      IF nusenew GT 2 THEN BEGIN 
         goodnew = 0b*good
         goodnew[usenew] = 1b
         nch     = total(abs(fix(good) - fix(goodnew)))
         good    = goodnew
         nit     = nit + 1
         ENDIF 
      IF keyword_set(verbose) THEN $
       print, 'nit,mean,sigma,nusenew,nch = ',nit,mean,sigma,nusenew,nch
   ENDREP UNTIL (nch EQ 0 OR nit GE maxit OR nusenew EQ 0)
   nrej    = narr - nuse
END 

