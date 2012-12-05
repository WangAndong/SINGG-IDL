PRO blem_rdsex, cat, id, x, y, mag, a, b, theta, fwhm, flag, class_star, awkfil=awkfil
   ;
   ; read important params from sextractor catalog
   tfile = '_blem_rdsex.tmp'
   IF NOT keyword_set(awkfil) THEN af = 'blem_rdsex.awk' ELSE af = awkfil
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
   readcol,tfile, id, x, y, mag, a, b, theta, fwhm, flag, class_star, format='(i,f,f,f,f,f,f,f,i)'
   ;
   ; delete temporary file
   file_delete,tfile,/quiet
END 

