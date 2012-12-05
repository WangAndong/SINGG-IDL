PRO singg_hicats_match
   ;
   ; Match SINGG sample selection to various HICAT catalogs
   ;
   ; G. Meurer Feb 2004
   ;
   ; set some parameters
   sdir     = '~/singg/sample/version3/'  ; directory for singg sample
   fil      =  'sample3_ravsort.dat'      ; singg sample name
   version  = 3                           ; singg sample version
   sfil     = sdir+fil
   ;
   dbnams   = ['hicat_may02_optconv', 'hicat_feb04']
   ndb      = n_elements(dbnams)
   srad     = 15.0                        ; search radius arcminutes
   dvtest   = 500.0
   dwtest   = 100.0
   fmtstr   = ['("| ",I2,f7.3,1x,a20,f8.4,f6.1,f7.1,f8.1)', '("| ",I2,f7.3,1x,a10,f8.4,f6.1,f7.1,f8.1)']
   fmtflg   = '("| ",I4,I4,I4)'
   filo     = 'singg_hicats_match.dat'
   filo2    = 'singg_hicats_mismatch.dat'
   ;
   ; read singg sample
   ; create arrays to hold matching objects, number of matches, angular sep
   ; convert ra,dec to double prec. degrees
   singg_readsamp, sfil, objname, cat, rastr, decstr, nednam, peak, sdv, w50, vhel, vshap, $
    obstr, logmhi, l, b, d, ra, dec, obs, version=version
   ra       = double(15.0*temporary(ra))
   dec      = double(temporary(dec))
   ns       = n_elements(objname)
   matchnam = make_array(ndb, ns, /string, value='-')
   nmatch   = make_array(ndb, ns, /int, value=0)
   sep      = make_array(ndb, ns, /float, value=-1.0)
   wflag    = make_array(ndb, ns, /byte, value=0b)
   vflag    = wflag
   spdb     = sep
   sintdb   = sep
   w50db    = sep
   vheldb   = sep
   ;
   ; loop through databases.  determine matches
   FOR k = 0, ndb-1 DO BEGIN
      dbopen, dbnams[k]
      ;
      ; match singg sample, element by element
      FOR j = 0, ns-1 DO BEGIN 
         list = dbcircled(ra[j], dec[j], srad, dis)
         IF list[0] GT -1 THEN BEGIN 
            nmatch[k,j]   = n_elements(list)
            sep[k,j]      = dis[0]
            dbext, list, 'HIPASS_NAME,RA,DEC,SP,SINT,VEL_50MAX,WIDTH_50MAX', $
             name, radb, decdb, sp, sint, vh, width
            matchnam[k,j] = name[0]
            spdb[k,j]     = sp[0]
            sintdb[k,j]   = sint[0]
            vheldb[k,j]   = vh[0]
            w50db[k,j]    = width[0]
            IF abs(vhel[j] - vheldb[k,j]) GT dvtest THEN vflag[k,j] = 1b 
            IF abs(w50[j] - w50db[k,j]) GT dwtest THEN wflag[k,j] = 1b 
         ENDIF 
      ENDFOR 
      dbclose
   ENDFOR 
   ;
   ; print results
   openw, lu, filo, /get_lun
   openw, lu2, filo2, /get_lun
   FOR i = 0, ns-1 DO BEGIN 
      str = string(format='(a12,2x,a11,2x,a11,f8.4,f6.1,f7.1,f8.1,f6.1)', $
                   objname[i],rastr[i],decstr[i],0.001*peak[i],sdv[i],w50[i],vhel[i],d[i])
      pr2 = 0b
      str2 = str
      FOR k = 0, ndb-1 DO BEGIN 
         str = str + '  ' + $
          string(format=fmtstr[k], $
                 nmatch[k,i],sep[k,i],matchnam[k,i],spdb[k,i],sintdb[k,i],w50db[k,i],vheldb[k,i])
         str2 = str2 + '  ' + string(format=fmtflg, nmatch[k,i], vflag[k,i], wflag[k,i])
         IF nmatch[k,i] NE 1 OR vflag[k,i] NE 0b OR wflag[k,i] NE 0b THEN pr2 = 1b
      ENDFOR 
      print, str
      printf, lu, str
      IF pr2 THEN printf, lu2, str2
   ENDFOR 
   free_lun,lu
   free_lun, lu2
END  
