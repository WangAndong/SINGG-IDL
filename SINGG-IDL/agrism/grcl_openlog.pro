PRO grcl_openlog, lun, file, fitlog=fitlog
   ;
   ; Open a log file for grism_classify results and put 
   ; a header line.  Return the logical unit that is opened.
   ;
   ; lun    <- logical unit that is opened
   ; file   -> name of file to open
   ; fitlog -> if set then the file will hold Gaussian line
   ;           fit results.
   ;
   ; G. Meurer 2002 - originally written
   ; G. Meurer 11/2004 - tidy and adjust to accomodate 
   ;                     continuum and EW.
   ;
   head = '#  id    xim     yim    mag     a     b   theta   w50   class    RA[deg]   Dec[deg]  minsn  maxsn  qual  RA,Dec [sexigessimal]     '
   IF keyword_set(fitlog) THEN $
    head = '#  id    xim     yim    mag     a     b   theta   w50   class    RA[deg]   Dec[deg]  minsn  maxsn nord0 lin qual   cen   width  flux      continuum    EW   RA,Dec [sexigessimal]     '
   openw, lun, file, /get_lun 
   printf, lun, head
END   
