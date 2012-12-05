PRO mkremeasuredb, kludgedist=kludgedist, calcflow=calcflow
   ;
   ; Create database for Koribalski's remeasurements.
   ;
   ; G. Meurer 08/2004
   ; G. Meurer 12/2004 - adjusted to insert E(B-V) and flag value from 
   ;                     Schlegel et al. (1998, ApJ, 500, 525) into
   ;                     database.  Databse now has E(b-V) in its sort 
   ;                     block
   ;
   ; setup
   hdir     = '/data1/acs7/meurer/SINGG/HIcatalogs/'
   ;hdir     = '/home/meurer/singg/HIcat/'
   fili     = 'singg_hipar_ed.dat'
   filo     = 'hipass_remeasure'
   fmti     = 'a,a,a,a,f,f,f,i,i,i,a'
   h0       = 70.0                  ; hubble constant
   invrad   = 3.141592654/180.      ; radians / degree
   distmin  = 1.0                   ; minimum distance [Mpc]
   dmin_bgc = 0.4
   vmin     = -999.9
   ;
   ; set up things for structure, database
   title    = 'Remeasurement of HI parameters by Koribalski (2004)'
   item     = ['name', 'optid', 'ra', 'dec', 'glong', 'glat', 'vhel', 'vlg', $
               'vcmb', 'vtonry', 'vshap', 'w50', 'w20',  'sp', 'esp', 'sint', $
               'logmhi', 'distance', 'ebv', 'mask_ebv', 'kname']
   tout     = ['C', 'C', 'R', 'R', 'R', 'R', 'I', 'R', $
               'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', $
               'R', 'R', 'R', 'B', 'C']
   tlen     = [0,0,8,8,4,4,2,4,$
               4,4,4,4,4,4,4,4,$
               4,4,4,1,0]
   dstring  = 'a,a,d,d,f,f,i,f,f,f,f,f,f,f,f,f,f,f,f,b,a'
   descript = ['HIPASS name', 'Optical ID', 'RA (J2000) [deg]', 'Dec (J2000) [deg]', $
               'Galactic longitude [deg]', 'Galactic longitude [deg]', $
               'HI Heliocentric radial velocity [km/s]', $
               'Local group corrected radial velocity [km/s]', $
               'CMB corrected radial velocity [km/s]', $
               'Tonry flow model corrected radial velocity [km/s]', $
               'Huchra flow model corrected radial velocity [km/s]', $
               'HI FWHM velocity width [km/s]', 'HI width at 20% max intensity [km/s]', $
               'HI peak flux density [Jy]', 'Error in HI peak flux density [Jy]', $
               'HI integrated flux [Jy km/s]', 'logarithm of HI mass [log(M_sun)]', $
               'Distance - multipole attractor model [Mpc]', $
               'Galactic foreground E(B-V) (Schlegel et al) [mag]', $
               'Byte mask from Schlegel et al',$
               'Name from Koribalski']
   indblk   = ['ra','dec','vhel','logmhi','sp','sint','glat','glong','ebv']
   ;
   ; work in directory containing catalog
   cd, hdir, current=cwd
   ;
   readcol, fili, kname, rastr, decstr, name, sp, esp, sint, vhel, w50, w20, optid, format=fmti
   nrem     = n_elements(name)
   esp      = 0.001*esp     ; convert from mJy to Jy
   ra       = 15.0d0*sexideg(rastr, delim=':')
   dec      = sexideg(decstr, delim=':')
   ;
   ; convert ra,dec to glong, glat
   glactc, ra, dec, 2000.0, glong, glat, 1, /degree
   ;
   ; compute local group velocity
   vlg      = float(vhel) - 79.0*cos(invrad*glong)*cos(invrad*glat) + $
              296.0*sin(invrad*glong)*cos(invrad*glat) - 36.0*sin(invrad*glat)
   ;
   IF keyword_set(kludgedist) THEN BEGIN 
      ;
      ; kludge distances (e.g. because jphflow not available).
      vcmb   = vlg
      vshap  = vlg
      vtonry = vlg
   ENDIF ELSE BEGIN 
      ;
      ; compute flow model velocities
      filvo  = 'flow_'+strtrim(filo,2)+'.out'
      filvs  = 'flow_'+strtrim(filo,2)+'.simple'
      IF keyword_set(calcflow) THEN BEGIN 
         vuse   = vhel
         badv   = where(vuse LT vmin, nb)
         IF nb GT 0 THEN vuse[badv] = vmin
         mkjphflowin, name, ra, dec, vuse
         spawn, '/home/meurer/bin/jphflow'
         spawn, 'mv flow.out '+filvo
         spawn, 'mv flow.simple '+filvs
      ENDIF 
      ;
      ; read in derived velocities
      readcol,filvs,dum1,dum2,dum3,vhin,vlgg,vcmb,vvir,vga,vshap,sty,iau,dum4,dum5,vtonry,$
              format='(a12,f,f,f,f,f,f,f,f,f,f,f,f,f)'
   ENDELSE 
   ;
   ; compute distance
   distance = vshap/h0
   k        = where(distance LT distmin, nk)
   IF nk GT 0 THEN distance[k] = distmin
   ;
   ; compute HI masses
   logmhi   = alog10(2.36e5*distance^2*sint)
   ;
   ; get ebv from Schlegel maps
   ebv      = dust_getval(glong, glat, /interp, outfile=febv1)
   mask_ebv = dust_getval(glong, glat, map='mask', outfile=febv2)
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
   create_struct2, remeasure, 'REMEASURE', item, dstring, dimen=nrem
   ;
   ; open dbd file, write top of the file
   fildbd  = filo + '.dbd'
   openw, lu, fildbd, /get_lun
   printf, lu, '#title'
   printf, lu, title
   printf, lu, ''
   printf, lu, '#maxentries'
   printf, lu, strtrim(string(nrem),2)
   printf, lu, ''
   printf, lu, '#items'
   ;
   ; Fill remeasure structure, write item lines of dbd file
   mc     = max(strlen(item))
   pritem = ljust(strupcase(item), mc)
   ni     = n_elements(item)
   FOR i = 0, ni-1 DO BEGIN 
      cmd = "remeasure."+strtrim(item[i],2)+" = "+strtrim(item[i],2)
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
   dbbuildstruct, remeasure
   dbclose,dummy
   ;
   cd, cwd
END 
