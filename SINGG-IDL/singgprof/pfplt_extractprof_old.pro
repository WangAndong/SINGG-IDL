PRO pfplt_extractprof, file, netflag, sma, fint, dfint, ngood, nbad, sb, esb, dfraw, efintsk, efintcn, $
                       pixsize=pixsize, fscale=fscale, ferrclip=ferrclip
   ;
   ; Extract radial profiles from Hanish .profile files
   ;
   fmtn   = '(f,f,f,l,l,f,f,f,f,f)'
   fmtc   = '(f,f,f,l,l,f,f,f,f)'
   ;
   IF netflag THEN $
    readcol, file, sma, fint, dfint, ngood, nbad, sb, esb, dfraw, efintsk, efintcn, format=fmtn ELSE BEGIN 
      readcol, file, sma, fint, dfint, ngood, nbad, sb, esb, dfraw, efintsk
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
