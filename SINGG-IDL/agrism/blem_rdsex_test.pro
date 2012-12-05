PRO blem_rdsex_test, aper, id, x, y, mag, tflux, a, b, theta, fwhm, flag, awkfil=awkfil
   ;
   ; read important params from sextractor catalog
   tfile = '_blem_rdsex.tmp'
   IF NOT keyword_set(awkfil) THEN af = 'blem_rdsex_test.awk' ELSE af = awkfil
   file_delete,tfile,/quiet
   ;
   ; awk out the important columns
   cmd = 'awk -f '+af+' '+cat+' > '+tfile
   err = 0
   spawn,cmd,exit_status=err
   IF err NE 0 THEN BEGIN 
      print,'**** Error in BLEM_RDSEX from spawn of awk cmd.  exit_status = ', err
      print,'**** exiting BLEM_RDSEX.'
      return
   ENDIF 
   ;
   ; read columns
   readcol,tfile, id, x, y, mag, tflux, bkgd, a, b, theta, fwhm, flag, format='(i,f,f,f,f,f,f,f,f,f,i)'
   tflux = tflux + bkgd*!dpi*0.25*aper*aper   ; fix total flux by adding sky back.
   ;
   ; delete temporary file
   file_delete,tfile,/quiet
END 

