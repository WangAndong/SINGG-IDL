PRO mk_hipass_curlcmd
   ;
   ; Write a command file with curl commands needed to 
   ; get HIPASS spectra of all of the SINGG sample 
   ; (at least all of the sample found in HICAT)
   ;
   ; G. Meurer 7/2007
   ;
   sdb     = 'singg_sample'
   hdb     = 'hicat_feb04'
   filo    = 'curl_hipass.cmd'
   ;
   ; find entries in hicat
   dbopen, sdb
   list    =  dbfind('entry_hicat > 0')
   nn      =  n_elements(list)
   dbext, list, 'entry_hicat', listh
   dbclose
   ;
   ; get info from hicat 
   dbopen, hdb
   dbext, listh, 'hipass_name,ra,dec,cube', name, ra, dec, cube
   dbclose
   ;
   ; turn things into strings
   rastr   = degsexi(ra, /ra, prec=1)
   decstr  = degsexi(dec, prec=0)
   url     = hipass_ascii_spec_url(cube, rastr, decstr)
   name    = strtrim(name,2)
   ;
   ; write file
   openw, lu, filo, /get_lun
   printf, lu, '#!/bin/sh'
   printf, lu, '#'
   FOR ii = 0, nn-1 DO printf, lu, 'curl "'+url[ii]+'" > '+name[ii]+'.dat' 
   free_lun, lu
   ;
   ; make file executable
   cmd      = 'chmod ugo+x '+filo
   spawn, cmd
END 
