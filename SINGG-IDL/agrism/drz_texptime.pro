FUNCTION drz_texptime, fitsfile
   ;
   ; returns the total exposure time from a multi-extension fits 
   ; image.  It does this by reading the header and summing the
   ; individual frame exposure time in the drizzle output keywords. 
   ;
   ; G. Meurer  08/2005
   ; G. Meurer  06/2006 updated to look for EXPTIME in header if
   ;                    the individual drizzle image kwds are not there.
   ;
   fits_open, fitsfile, fcb
   fits_read, fcb, img, hdr, exten_no=0
   ndrizim  = sxpar(hdr, 'NDRIZIM')
   texptime = 0.0
   FOR ii = 1, ndrizim DO BEGIN 
      istr = strtrim(ii,2)
      IF ii LT 10 THEN istr = '0'+istr
      IF ii LT 100 THEN istr = '0'+istr
      kwd = 'D'+istr+'DEXP'
      tt       = float(sxpar(hdr, kwd, count=count))
      IF count GE 1 THEN texptime = texptime + tt[0]
   ENDFOR 
   IF texptime LE 0.0 OR ndrizim LE 0 THEN BEGIN 
      tt = float(sxpar(hdr, 'exptime', count=count))
      IF count GE 1 THEN texptime = tt[0]
   ENDIF 
   fits_close, fcb
   return, texptime
END 
