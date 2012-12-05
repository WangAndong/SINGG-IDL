PRO grcl_logline, lu, id, objpar, minsn, maxsn, ncontam, results=results
   ;
   ; write an entry in the logfile
   ;
   ; lu      -> logical unit to print to.
   ; id      -> object id #
   ; objpar  -> parameters from sextractor 
   ; minsn   -> minimum S/N in spectrum
   ; maxsn   -> maximum S/N in spectrum
   ; ncontam -> ?
   ; results -> structure containing Gaussian fitting results
   ;
   ; G. Meurer - 2002: originally written....
   ; G. Meurer - 11/2004: tidy up.  Adjusted to print continuum and EW.
   ;
   kra   = 8
   kdec  = 9
   fmt1  = '(i5,f8.1,f8.1,f7.2,f6.1,f6.1,f7.1,f7.2,f7.3,f11.5,f11.5,f7.1,f7.1,i6,1x,a)' 
   fmt2  = '(i5,f8.1,f8.1,f7.2,f6.1,f6.1,f7.1,f7.2,f7.3,f11.5,f11.5,f7.1,f7.1,i6,i4,i4,f8.1,f7.1,e10.2,e10.2,f8.1,1x,a27)' 
   ;
   rdstr = adstring(objpar[kra], objpar[kdec], 2)
   IF NOT keyword_set(results) THEN printf,lu,id,objpar,minsn,maxsn,ncontam,rdstr,format=fmt1 $
    ELSE printf,lu,id,objpar,minsn,maxsn,ncontam,results.linenum,results.quality,results.center,$
         results.width,results.flux,results.continuum,results.ew,rdstr,format=fmt2
END 

