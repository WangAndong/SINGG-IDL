PRO make_cadc_indb, outfile
   ; convert singg_sample database into an input file for cadc 
   ; digital sky survey interface
   ;
   ; G. Meurer 08/2007 - converted from make_cadc_in.pro
   ;
   tab = string(9B)
   sdb = 'singg_sample'
   ;
   ; open database get required quantities
   dbopen, sdb
   dbext, -1, 'name,ra,dec', name, ra, dec
   name = strtrim(name,2)
   dbclose
   ;
   ; convert hipass corrds to string
   rastr  = degsexi(ra,/ra,prec=1)
   decstr = degsexi(dec,prec=0)
   ;
   openw, lu, outfile, /get_lun
   FOR i = 0, n_elements(name)-1 DO BEGIN
      print,name[i]+tab+rastr[i]+tab+decstr[i]+tab+'2000'+tab+'20:00'
      printf,lu,name[i]+tab+rastr[i]+tab+decstr[i]+tab+'2000'+tab+'20:00'
   ENDFOR 
   free_lun, lu
END 
