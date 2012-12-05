PRO read_header, hd, object, exptime, texptime, filters, ncombine, bunit, bcomment
   print,'In read_header'
   object   = sxpar(hd, 'OBJECT',   '**** OBJECT keyword not found', count=matches)
   IF matches EQ 0  THEN object = 'UNKNOWN' 
   ;
   bunit    = 'DN/S'
   bcomment = 'Count rate image'
   exptime  = sxpar(hd, 'EXPTIME',  '**** EXPTIME keyword not found', count=matches)
   IF (matches EQ 0 OR exptime LE 1.0) THEN BEGIN
      bunit    = 'DN'
      bcomment = 'Average counts image '
      exptime  = 1.0
   ENDIF 
   ;
   filters  = sxpar(hd, 'FILTERS',  '**** FILTERS keyword not found', count=matches)
   IF matches EQ 0  THEN filters = 'UNKNOWN' 
   ;
   ncombine = sxpar(hd, 'NCOMBINE', '**** NCOMBINE keyword not found', count=matches)
   IF matches EQ 0  THEN ncombine = 1
   texptime = float(ncombine)*exptime
   ;
   print, 'Object        : ', object
   print, 'Avg Exptime   : ', exptime
   print, 'Total exptime : ', texptime
   print, 'Filters       : ', filters
   ;
END
