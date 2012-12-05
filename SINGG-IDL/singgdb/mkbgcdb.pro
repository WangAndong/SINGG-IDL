PRO mkbgcdb, calcflow=calcflow
   ;
   ; Make BGC database from AJ machine readable file.
   ;
   ; calcflow -> if set then then jphflow is run.  This should only be 
   ;             done on a solaris machine.  If not set then flow model
   ;             results are read in without recalculating.
   ;
   ; G. Meurer 08/04
   ; G. Meurer 12/2004 - adjusted to insert E(B-V) and flag value from 
   ;                     Schlegel et al. (1998, ApJ, 500, 525)
   ;                     also uses E(B-V) in the sort block for the 
   ;                     database.
   ;
   bdir     = '/data1/acs7/meurer/SINGG/HIcatalogs/'
   fili     = 'Koribalski.BGC.machine.table'
   filo     = 'hipass_bgc'
   febv1    = 'hipass_bgc_ebv1.log'
   febv2    = 'hipass_bgc_ebv2.log'
   skipline = 71
   fmti     = '(7x,a9,1x,a8,1x,a9,1x,f5.1,1x,f5.1,1x,a16,a1,1x,'+$
              'f6.3,1x,f5.3,1x,f6.1,1x,f5.1,1x,i4,1x,i2,1x,i3,1x,'+$
              'i3,1x,i4,1x,f5.2,a1,1x,a7)'
   h0       = 70.0                  ; hubble constant
   h0_bgc   = 75.0                  ; Hubble constant used by BGC
   invrad   = 3.141592654/180.      ; radians / degree
   distmin  = 1.0                   ; minimum distance [Mpc]
   dmin_bgc = 0.4
   vmin     = -999.9
   ;
   ; define things for structure and database
   title    = 'HIPASS Bright Galaxy Catalog (BGC) - Koribalski et al. (2004)'
   item     = ['name', 'optid', 'ra', 'dec', 'glong', 'glat', 'vhel', 'evhel', 'vlg_bgc', $
               'vlg', 'vcmb', 'vtonry', 'vshap', 'w50', 'w20', 'sp', 'esp', $
               'sint', 'esint', 'logmhi_bgc', 'flag_logm', 'distance_bgc', 'logmhi','distance', 'ebv', 'mask_ebv', 'notes']
   tout     = ['C', 'C', 'R', 'R', 'R', 'R', 'I', 'I', 'R', $
               'R', 'R', 'R', 'R', 'I', 'I', 'R', 'R', $
               'R', 'R', 'R', 'B', 'R', 'R', 'R', 'R', 'B', 'C']
   tlen     = [0,0,8,8,4,4,2,2,4,$
               4,4,4,4,2,2,4,4,$
               4,4,4,1,4,4,4,4,1,0]
   dstring  = 'a,a,d,d,f,f,i,i,f,f,f,f,f,i,i,f,f,f,f,f,b,f,f,f,f,b,a'
   descript = ['HIPASS name', 'Optical ID', 'RA (J2000) [deg]', 'Dec (J2000) [deg]', $
               'Galactic longitude [deg]', 'Galactic longitude [deg]', $
               'HI Heliocentric radial velocity [km/s]', $
               'Error in heliocentric radial velocity [km/s]', $
               'BGC Local group corrected radial velocity [km/s]', $
               'Local group corrected radial velocity [km/s]', $
               'CMB corrected radial velocity [km/s]', $
               'Tonry flow model corrected radial velocity [km/s]', $
               'Huchra flow model corrected radial velocity [km/s]', $
               'HI FWHM velocity width [km/s]', 'HI width at 20% max intensity [km/s]', $
               'HI peak flux density [Jy]', 'Error in HI peak flux density [Jy]', $
               'HI integrated flux [Jy km/s]', 'Error in HI integrated flux [Jy km/s]', $
               'BGC logarithm of HI mass [log(M_sun)]', $
               'log(mass) flag from BGC: 1 - distance, 2 - Mstream', $
               'Distance adopted by BGC [Mpc]', 'Logarithmic HI mass [Log(M_sun)]', $
               'Distance - multipole attractor model [Mpc]', $
               'Galactic foreground E(B-V) (Schlegel et al) [mag]', $
               'Byte mask from Schlegel et al',$
               'Notes from BGC']
   indblk   = ['ra','dec','vhel','logmhi','sp','sint','glat','glong','ebv']
   ;
   ; work in directory containing bgc
   cd, bdir, current=cwd
   ;
   readfmt, fili, fmti, name, rastr, decstr, $
            glong_bgc, glat_bgc, optid, flagstr_optid, sp, esp, sint, esint, $
            vhel, evhel, w50, w20, vlg_bgc, logmhi_bgc, flag_logm_str, notes, skipline=skipline
   nbgc   = n_elements(name)
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
   ; compress optical ID, reset optical IDs that have 'new'
   optid  = strcompress(temporary(optid),/remove_all)
   k      = where(strpos(optid,'new') EQ 0, nk)
   IF nk GT 0 THEN optid[k] = ''
   ;
   ; also throw out optical IDs where flagged
   k      = where(strtrim(flagstr_optid,2) NE '', nk)
   IF nk GT 0 THEN optid[k] = ''
   ;
   ; calculate BGC adopted distance
   distance_bgc = vlg_bgc / h0_bgc
   ;
   ; compute logm flag in bytes, and fix bgc distances where needed.
   flag_logm    = make_array(nbgc, /byte, value=0b)
   k            = where(strpos(flag_logm_str,'*') GE 0,nk)
   IF nk GT 0 THEN BEGIN 
      flag_logm[k]    = flag_logm[k] + 1b
      distance_bgc[k] = sqrt(10.0^logmhi_bgc[k]/(2.36e5*sint[k]))
      forprint, name[k], distance_bgc[k]
   ENDIF 
   k            = where(strpos(flag_logm_str,'+') GE 0,nk)
   IF nk GT 0 THEN flag_logm[k]    = flag_logm[k] + 2b
   ;
   ; compute local group velocity
   vlg    = float(vhel) - 79.0*cos(invrad*glong)*cos(invrad*glat) + $
            296.0*sin(invrad*glong)*cos(invrad*glat) - 36.0*sin(invrad*glat)
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
   readcol,filvs,dum1,dum2,dum3,vhin,vlg,vcmb,vvir,vga,vshap,sty,iau,dum4,dum5,vtonry,$
       format='(a12,f,f,f,f,f,f,f,f,f,f,f,f,f)'
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
   create_struct2, bgc, 'BGC', item, dstring, dimen=nbgc
   ;
   ; open dbd file, write top of the file
   fildbd  = filo + '.dbd'
   openw, lu, fildbd, /get_lun
   printf, lu, '#title'
   printf, lu, title
   printf, lu, ''
   printf, lu, '#maxentries'
   printf, lu, strtrim(string(nbgc),2)
   printf, lu, ''
   printf, lu, '#items'
   ;
   ; Fill bgc structure, write item lines of dbd file
   mc     = max(strlen(item))
   pritem = ljust(strupcase(item), mc)
   ni     = n_elements(item)
   FOR i = 0, ni-1 DO BEGIN 
      cmd = "bgc."+strtrim(item[i],2)+" = "+strtrim(item[i],2)
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
   dbbuildstruct, bgc
   dbclose,dummy
   ;
   cd, cwd
END 
