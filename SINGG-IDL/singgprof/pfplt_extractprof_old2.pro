PRO pfplt_extractprof, file, netflag, sma, fint, dfint, ngood, nbad, sb, esb, $
                       dfraw, efintsk, efintcn, pixsize=pixsize, fscale=fscale, $
                       ferrclip=ferrclip, lstart=lstart, lend=lend
   ;
   ; Extract radial profiles from Hanish .profile files
   ;
   ; file     -> Name of profile file to read
   ; netflag  -> 1b if the file contains line flux profile(s)
   ; sma      <- semi-major axis of apertures (scaled by pixsize
   ;             if set)
   ; fint     <- flux interior (scaled by fscale if fscale set)
   ; dfint    <- flux in annulus (scaled by fscale if fscale set)
   ; ngood    <- number of good pixels in annulus
   ; nbad     <- number of bad pixels in annulus
   ; sb       <- surface brightness in annulus 
   ;             (scaled by fscale & pixsize^{-2} if these are set).
   ; esb      <- error in surface brightness sb
   ; dfraw    <- ?
   ; efintsk  <- error in interior flux fint due to sky subtraction
   ;             (uses same scaling as fint).
   ; efintcn  <- error in interior flux fint due to continuum subtraction.
   ;             (uses same scaling as fint).
   ; pixsize  -> if set, this is the pixelsize in arcsec.  defaults to 1.0
   ; fscale   -> if set this is the scale factor that is applied to 
   ;             fluxes (to convert from c/s to physical units).  
   ;             Defaults to 1.0
   ; ferrclip -> If set, then only the portion of the profile having 
   ;             esb/sb < ferrclip is reurned
   ; lstart   -> if set, first line of file to read
   ;             this is useful if there are more than one profiles in file
   ; lend     -> if set, last line of file to read
   ;             this is useful if there are more than one profiles in file
   ;
   ; G. Meurer 10/2004
   ; G. Meurer 09/2007 moved to pfplt_extractprof_old2.pro to make way 
   ;                   for revisions from Dan
   ;
   fmtn   = '(f,f,f,l,l,f,f,f,f,f)'
   fmtc   = '(f,f,f,l,l,f,f,f,f)'
   IF keyword_set(lstart) THEN BEGIN 
      skipline = lstart - 1 
   ENDIF ELSE BEGIN 
      lstart   = 1
      skipline = 0
   ENDELSE 
   IF keyword_set(lend) THEN BEGIN 
      numline  = lend - lstart + 1
   ENDIF ELSE BEGIN 
      numline  = 0
   ENDELSE 
   ;
   IF netflag THEN $
    readcol, file, sma, fint, dfint, ngood, nbad, sb, esb, dfraw, $
             efintsk, efintcn, format=fmtn, skipline=skipline, numline=numline ELSE BEGIN 
      readcol, file, sma, fint, dfint, ngood, nbad, sb, esb, dfraw, $
               efintsk, format=fmtc, skipline=skipline, numline=numline
      efintcn = 0.0 * efintsk
   ENDELSE 
   kk     = where(sma GT 0.0)
   ;
   ; calibrate if need be
   IF keyword_set(pixsize) THEN psize = pixsize ELSE psize = 1.0
   IF keyword_set(fscale)  THEN fsc   = fscale  ELSE fsc   = 1.0
   parea  = psize*psize
   ngood  = long(ngood[kk])
   nbad   = long(nbad[kk])
   sma    = psize * sma[kk]
   fint   = fint[kk] * fsc
   dfint  = dfint[kk] * fsc
   sb     = sb[kk] * fsc / parea
   esb    = esb[kk] * fsc / parea
   dfraw  = dfraw[kk] * fsc
   efintsk  = efintsk[kk] * fsc / parea
   efintcn  = efintcn[kk] * fsc / parea
   ;
   ; clip arrays to where fractional surfacebrightness error <= ferrclip if this
   ; keyword is set
   IF keyword_set(ferrclip) THEN BEGIN 
      ferr   = abs(esb/sb)
      kk     = where(ferr LE ferrclip, nkk) 
      IF nkk GT 0 THEN BEGIN 
         ngood   = ngood[kk]
         nbad    = nbad[kk]
         sma     = sma[kk]
         fint    = fint[kk]
         dfint   = dfint[kk]
         sb      = sb[kk]
         esb     = esb[kk]
         dfraw   = dfraw[kk]
         efintsk   = efintsk[kk]
         efintcn   = efintcn[kk]
      ENDIF 
   ENDIF 
END 
