PRO grism_outascii, file, bintab, mag, specz, x, y, lamlim=lamlim
   ; write grism spectra to an ascii file
   IF keyword_set(lamlim) THEN BEGIN 
      lmin = min(lamlim)
      lmax = max(lamlim)
   ENDIF ELSE BEGIN 
      lmin = min(bintab.lambda)
      lmax = max(bintab.lambda)
   ENDELSE 
   ;
   lam  = bintab.lambda
   flx  = bintab.flux
   eflx = bintab.ferror
   k    = where(lam GE lmin AND lam LE lmax,nk)
   ;
   IF nk GT 0 THEN BEGIN 
      openw,luno,file,/get_lun
      printf,luno,"# lambda flux eflux ", mag, specz, x, y
      FOR i = 0, n_elements(k)-1 DO BEGIN 
         printf,luno,lam[k[i]], flx[k[i]], eflx[k[i]]
      ENDFOR 
      close,luno
      free_lun,luno
   ENDIF ELSE BEGIN 
      print, '**** Warning in GRISM_OUTASCII, no valid pixels for spectrum.  Will not write ', file
   ENDELSE 
   ;
END 
