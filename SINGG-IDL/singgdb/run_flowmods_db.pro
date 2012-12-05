PRO run_flowmods_db
   ;
   ; run jphflow for all sources in relevent SINGG and HIPASS databases.
   vmin = -999.99
   ;
   ; 1. SINGG Sample
   dbopen, 'singg_sample'
   list = dbfind('RA > -0.1')
   dbext, list, 'NAME,RA,DEC,VHEL', name, ra, dec, vhel
   dbclose, dum
   ;
   badv = where(vhel LT vmin, nb)
   IF nb GT 0 THEN vhel[badv] = vmin
   mkjphflowin, name, ra, dec, vhel
   spawn, '/home/meurer/bin/jphflow'
   spawn, 'mv flow.out flow_singg.out'
   spawn, 'mv flow.simple flow_singg.simple'
   ;
   ; 2. HICAT Feb04
   dbopen, 'hicat_feb04'
   list = dbfind('RA > -9999.99')
   dbext, list, 'HIPASS_NAME,RA,DEC,VUSE', name, ra, dec, vhel
   dbclose, dum
   ;
   badv = where(vhel LT vmin, nb)
   IF nb GT 0 THEN vhel[badv] = vmin
   mkjphflowin, name, ra, dec, vhel
   spawn, '/home/meurer/bin/jphflow'
   spawn, 'mv flow.out flow_hicat_feb04.out'
   spawn, 'mv flow.simple flow_hicat_feb04.simple'
   ;
   ; 3. HICAT May02 (optical convention)
   dbopen, 'hicat_may02_optconv'
   list = dbfind('RA > -9999.99')
   dbext, list, 'HIPASS_NAME,RA,DEC,VUSE', name, ra, dec, vhel
   dbclose, dum
   ;
   badv = where(vhel LT vmin, nb)
   IF nb GT 0 THEN vhel[badv] = vmin
   mkjphflowin, name, ra, dec, vhel
   spawn, '/home/meurer/bin/jphflow'
   spawn, 'mv flow.out flow_hicat_may02_optconv.out'
   spawn, 'mv flow.simple flow_hicat_may02_optconv.simple'
   ;
   ; 3. HICAT May02 
   dbopen, 'hicat_may02'
   list = dbfind('RA > -9999.99')
   dbext, list, 'HIPASS_NAME,RA,DEC,VUSE', name, ra, dec, vhel
   dbclose, dum
   ;
   badv = where(vhel LT vmin, nb)
   IF nb GT 0 THEN vhel[badv] = vmin
   mkjphflowin, name, ra, dec, vhel
   spawn, '/home/meurer/bin/jphflow'
   spawn, 'mv flow.out flow_hicat_may02.out'
   spawn, 'mv flow.simple flow_hicat_may02.simple'
END 
