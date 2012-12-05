PRO compare_sungg_singg
   ;
   ; compare entries in the sungg_derived and singg_derived
   ; databases.  In the first instance we want to ensure that
   ; the sungg detections include all the singg galaxies.  
   ; We should also compare positions, PA and axial ratio.
   ;
   ; G. Meurer  11/2007
   ;
   hdb     = 'singg_derived'  
   udb     = 'sungg_derived'  
   sdb     = 'sungg_sample'   
   filo    = 'compare_singg_sungg.out'
   fmto = '(i5,i5,i5,2x,a135,f8.1)'
   hdr1    = '# sungg singg    <----------  sungg derived -------------------------------------------><-------------- singg_derived --------------------------------->'
   hdr2    = '# drvd samp drvd field             name              HIPASS    OPTID                    singg_name           HIPASS       OPTID                           sep-arcsec'
   ;
   ; open sungg derived database, get relevant quantities for
   ; all NUV measurements
   dbopen, udb
   list    = dbfind('filter = nuv')
   dbext, list, 'entry,field,name,hipname,optid,ra,dec,fluxrad_brt,pa,axerat,entry_sungg_sample', $
                entu, fldu, namu, hnamu, optu, rau, decu, fradu, pau, abu, entus
   nu      = n_elements(entu)
   print, 'Number of sungg_derived NUV objects : ', nu
   dbclose
   ;
   ; determine the unique sungg_sample entries
   kk      = sort(entus)
   jj      = uniq(entus[kk])
   entusu  = entus[kk[jj]]
   nuu     = n_elements(entusu)
   print, 'Number of unique sungg_sample objects : ', nuu
   ;
   ; Get the corresponding singg_sample entries from sungg_sample
   dbopen, sdb
   dbext, entusu, 'entry_singg', entsu
   ;forprint, entusu, entsu
   dbclose
   ;
   ; populate entry in singg_sample for each sungg_derived object
   entuss  = make_array(nu, /long, value=-1l)
   FOR ii = 0, nuu-1 DO BEGIN 
      jj         = where(entus EQ entusu[ii], njj)
      entuss[jj] = entsu[ii]
   ENDFOR 
   ;
   ; get unique singg_sample entries
   kk      = sort(entuss)
   jj      = uniq(entuss[kk])
   ii      = where(entuss[kk[jj]] GT 0, nss)
   entssu  = entuss[kk[jj[ii]]]
   ;forprint, entssu
   ;
   ; open singg_derived get all the good entries
   dbopen, hdb
   list    = good_derived2()
   dbext, list, 'entry,name,object,optid,ra,dec,axerat,pa,rmax_f,entry_sample', $
                 enth,namh,hnamh,opth,rah,dech,abh,pah,fradh,enthss
   dbclose
   ;
   ; match sungg_derived entries to singg_derived using entry numbers
   ptrhdb = make_array(nu, /long, value=-1l)
   enthdb = make_array(nu, /long, value=-1l)
   numhdb = make_array(nu, /int, value=0)
   dishdb = make_array(nu, /float, value=-1.0)
   FOR ii = 0, nu -1 DO BEGIN 
      IF entuss[ii] GT -1 THEN BEGIN 
         jj = where(enthss EQ entuss[ii], njj)
         IF njj GT 0 THEN BEGIN
            gcircd, 2, rau[ii], decu[ii], rah[jj], dech[jj], dis 
            IF njj EQ 1 THEN BEGIN 
               ;
               ; only one match, take it
               ptrhdb[ii] = jj[0]
               enthdb[ii] = enth[jj[0]]
               numhdb[ii] = njj
               dishdb[ii] = dis[0]*3600.0
            ENDIF ELSE BEGIN 
               ;
               ; multiple matches, find best by Ra, dec
               kk = sort(dis)
               ptrhdb[ii] = jj[kk[0]]
               enthdb[ii] = enth[jj[kk[0]]]
               numhdb[ii] = njj
               dishdb[ii] = dis[kk[0]]*3600.0
            ENDELSE 
         ENDIF 
      ENDIF 
   ENDFOR 
   ;
   forprint, entu, entus, ptrhdb, enthdb, numhdb, dishdb
   ;
   ; print name matches
   openw, lu, filo, /get_lun
   kk = where(ptrhdb GT 0, nkk)
   printf,-1,hdr1
   printf,-1,hdr2
   printf,lu,hdr1
   printf,lu,hdr2
   FOR ii = 0, nkk-1 DO BEGIN 
      printf, lu, entu[kk[ii]], entuss[kk[ii]], enthdb[kk[ii]], fldu[kk[ii]]+' '+namu[kk[ii]]+' '+hnamu[kk[ii]]+' '+optu[kk[ii]]+' '$
       +namh[ptrhdb[kk[ii]]]+' '+hnamh[ptrhdb[kk[ii]]]+' '+opth[ptrhdb[kk[ii]]], dishdb[kk[ii]], format=fmto
      printf, -1, entu[kk[ii]], entuss[kk[ii]], enthdb[kk[ii]], fldu[kk[ii]]+' '+namu[kk[ii]]+' '+hnamu[kk[ii]]+' '+optu[kk[ii]]+' '$
       +namh[ptrhdb[kk[ii]]]+' '+hnamh[ptrhdb[kk[ii]]]+' '+opth[ptrhdb[kk[ii]]], dishdb[kk[ii]], format=fmto
   ENDFOR 
   free_lun, lu
END
