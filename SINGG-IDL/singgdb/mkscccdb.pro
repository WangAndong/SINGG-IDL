PRO mkscccdb
   ;
   ; Make Kilborn et al South Celestial Cap database from AJ machine readable file.
   ;
   ; G. Meurer 08/2004
   ; G. Meurer 12/2004 - adjusted to insert E(B-V) and flag value from 
   ;                     Schlegel et al. (1998, ApJ, 500, 525) into
   ;                     database.  Databse now has E(b-V) in its sort 
   ;                     block
   ;
   hdir     = '/data1/acs7/meurer/SINGG/HIcatalogs/'
   ;hdir     = '/home/meurer/singg/HIcat/'
   fili     = 'kilborn02_table1.dat'
   filo     = 'hipass_sccc'
   febv1    = 'hipass_sccc_ebv1.log'
   febv2    = 'hipass_sccc_ebv2.log'
   skipline = 40
   fmti     = '(6x,a9,1x,a10,1x,a11,1x,i5,1x,i3,1x,i3,1x,f5.3,1x,f7.2,2x,f5.2,'+$
               '1x,a16,1x,a5,1x,a6,1x,a1)'
   h0       = 70.0                  ; hubble constant
   invrad   = 3.141592654/180.      ; radians / degree
   vmin     = -9999
   ;
   ; define things for structure and database
   title    = 'HIPASS South Celestial Cap Catalog (SCCC) - Kilborn et al. (2002)'
   item     = ['name', 'optid', 'ra', 'dec', 'glong', 'glat', 'vhel', 'w50', 'w20', 'sp', 'sint', $
               'logmhi', 'vopt', 'morph', 'flag', 'ebv', 'mask_ebv']
   tout     = ['C', 'C', 'R', 'R', 'R', 'R', 'I', 'I', 'I', 'R', 'R', 'R', 'I', 'C', 'B', 'R', 'B']
   tlen     = [0, 0, 8, 8, 4, 4, 2, 2, 2, 4, 4, 4, 2, 0, 1, 4, 1]
   dstring  = 'a,a,d,d,f,f,i,i,i,f,f,f,i,a,b,f,b'
   descript = ['HIPASS name', 'Optical ID', 'RA (J2000) [deg]', 'Dec (J2000) [deg]', $
               'Galactic longitude [deg]', 'Galactic longitude [deg]', $
               'HI Heliocentric radial velocity [km/s]', $
               'HI FWHM velocity width [km/s]', 'HI width at 20% max intensity [km/s]', $
               'HI peak flux density [Jy]', 'HI integrated flux [Jy km/s]', $
               'Logarithmic HI mass [Log(M_sun)]', $
               'Optical Heliocentric radial velocity [km/s]', $
               'Optical morphology', $
               'Flag: 0b - none, 1b - opt. confusion, 2b - narowband conf.', $
               'Galactic foreground E(B-V) (Schlegel et al) [mag]', $
               'Byte mask from Schlegel et al']
   indblk   = ['ra','dec','glong', 'glat', 'vhel','vopt', 'logmhi', 'sp', 'sint', 'ebv']
   ;
   ; work in directory containing sccc
   cd, hdir, current=cwd
   ;
   ; read in catalog
   readfmt, fili, fmti, name, rastr, decstr, vhel, w20, w50, sp, sint, logmhi, $
            optid, voptstr, morph, f_morph, skipline=skipline
   nsc      = n_elements(name)
   ;
   ; convert ra, dec to degrees
   rastr  = strtrim(temporary(rastr),2)
   decstr = strtrim(temporary(decstr),2)
   ra     = 15.0d0*sexideg(rastr, delim=' ')
   dec    = sexideg(decstr, delim=' ')
   ;
   ; convert ra,dec to glong, glat
   glactc, ra, dec, 2000.0, glong, glat, 1, /degree
   ;
   ; get ebv from Schlegel maps
   ebv      = dust_getval(glong, glat, /interp, outfile=febv1)
   mask_ebv = dust_getval(glong, glat, map='mask', outfile=febv2)
   ;
   ; compress optical ID, reset optical IDs that have 'new'
   optid  = strcompress(temporary(optid),/remove_all)
   ;
   ; convert optical velocity to integer
   vopt   = make_array(nsc, /int)
   FOR i = 0, nsc-1 DO BEGIN 
      str     = voptstr[i]
      reads,str,v,format='(i5)'
      vopt[i] = v
   ENDFOR 
   k      = where(strtrim(voptstr,2) EQ '', nk)
   IF nk GT 0 THEN vopt[k] = vmin 
   ;
   ; convert f_morph to a binary flag
   ; ' ' -> 0b  = no flag
   ; 'd' -> 1b  = confused optical detection
   ; '*' -> 2b  = narrowband confirmed
   flag   = make_array(nsc,/byte,value=0b)
   k      = where(f_morph EQ 'd',nk)
   IF nk GT 0 THEN flag[k] = 1b
   k      = where(f_morph EQ '*',nk)
   IF nk GT 0 THEN flag[k] = 2b
   ;
   ; Go to directory for databases
   zdbase   = getenv('ZDBASE')
   cd, zdbase
   ;
   ; set lengths of character items
   k       = where(tlen EQ 0,nk)
   IF nk GT 0 THEN FOR i = 0,nk-1 DO result = execute('tlen[k[i]] = max(strlen('+item[k[i]]+'))') $
              ELSE stop
   ;
   ; create structure
   create_struct2, sccc, 'SCCCC', item, dstring, dimen=nsc
   ;
   ; open dbd file, write top of the file
   fildbd  = filo + '.dbd'
   openw, lu, fildbd, /get_lun
   printf, lu, '#title'
   printf, lu, title
   printf, lu, ''
   printf, lu, '#maxentries'
   printf, lu, strtrim(string(nsc),2)
   printf, lu, ''
   printf, lu, '#items'
   ;
   ; Fill sccc structure, write item lines of dbd file
   mc     = max(strlen(item))
   pritem = ljust(strupcase(item), mc)
   ni     = n_elements(item)
   FOR i = 0, ni-1 DO BEGIN 
      cmd = "sccc."+strtrim(item[i],2)+" = "+strtrim(item[i],2)
      print,cmd
      result = execute(cmd)
      typstr = ljust(tout[i]+'*'+strtrim(string(tlen[i]),2),5)
      printf,lu,pritem[i]+'   '+typstr+'   "'+descript[i]+'"'
   ENDFOR 
   ;
   ; write index blocks of dbd file and close
   printf,lu,'  '
   printf,lu,'#index'
   ni = n_elements(indblk)
   FOR i = 0, ni-1 DO BEGIN 
      printf,lu,ljust(strupcase(strtrim(indblk[i],2)),mc)+'   sort'
   ENDFOR 
   free_lun,lu   
   ;
   ; write database
   !PRIV = 2
   dbcreate, filo, 1, 1, /external
   dbopen, filo, 1
   dbbuildstruct, sccc
   dbclose,dummy
   ;
   cd, cwd
END 
