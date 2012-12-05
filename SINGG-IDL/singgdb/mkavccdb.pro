PRO mkavccdb
   ;
   ; Make database for annomalous velocity object catalog (AVCC) of 
   ; Putman et al. (2002)
   ;
   ; G. Meurer 08/2004
   ; G. Meurer 12/2004 - adjusted to insert E(B-V) and flag value from 
   ;                     Schlegel et al. (1998, ApJ, 500, 525)
   ;                     also uses E(B-V) in the sort block for the 
   ;                     database.
   ;
   hdir     = '/data1/acs7/meurer/SINGG/HIcatalogs/'
   ;hdir     = '/home/meurer/singg/HIcat/'
   fili     = 'putman_table1.dat'
   filo     = 'hipass_avcc'
   febv1    = 'hipass_avcc_ebv1.log'
   febv2    = 'hipass_avcc_ebv2.log'
   skipline = 61
   fmti     = '(i4,2x,a20,i2,1x,f4.1,2x,a1,i2,1x,i2,3x,i4,2x,i4,2x,i4,4x,i3,3x,'+$
              'f4.1,3x,f4.1,2x,i3,1x,f6.2,3x,f5.2,2x,f5.2,2x,f8.1,2x,a30)'
   h0       = 70.0                  ; hubble constant
   invrad   = 3.141592654/180.      ; radians / degree
   vmin     = -9999
   ;
   ; define things for structure and database
   title    = 'HIPASS Anomalous Velocity Object Catalog (AVCC) - Putman et al. (2002)'
   item     = ['avccid', 'name', 'optid', 'ra', 'dec', 'glong', 'glat', 'vlsr', 'vgsr', 'vlgsr', 'w50', $
               'rada', 'radb', 'pa', 'area', 'tpeak', 'nhi', 'sint', 'ebv', 'mask_ebv']
   tout     = ['I', 'C', 'C', 'R', 'R', 'R', 'R', 'I', 'I', 'I', 'I', 'R', 'R', 'I', 'R', 'R', 'R', 'R', 'R', 'B']
   tlen     = [2,0,0,8,8,4,4,2,2,2,2,4,4,2,4,4,4,4,4,1]
   dstring  = 'i,a,a,d,d,f,f,i,i,i,i,f,f,i,f,f,f,f,f,b'
   descript = ['AVCC ID number', 'AVCC Name', 'Optical ID', $
               'RA (J2000) [deg]', 'Dec (J2000) [deg]', $
               'Galactic longitude [deg]', 'Galactic longitude [deg]', $
               'Average Local Standard of Rest velocity [km/s]', $
               'Average Galactic Standard of Rest velocity [km/s]', $
               'Average Local Group Standard of Rest velocity [km/s]', $
               'HI FWHM velocity width [km/s]', $
               'Semi-major axis length [deg]', 'Semi-minor axis length [deg]', $
               'Position angle [deg]', 'Area [deg^2]', 'Peak brightness temp [K]', $
               'Max HI column density [10^{20} cm^2]', 'Total flux [Jy km/s]', $
               'Galactic foreground E(B-V) (Schlegel et al) [mag]', $
               'Byte mask from Schlegel et al']
   indblk   = ['ra', 'dec', 'glong', 'glat', 'vlsr', 'vgsr', 'vlgsr', 'sint', 'rada', 'area', 'nhi', 'ebv']
   ;
   ; work in directory containing catalog
   cd, hdir, current=cwd
   ;
   ; read in catalog
   readfmt, fili, fmti, avccid, name, rah, ram, decs, decd, decm, vlsr, vgsr, vlgsr, $
            w50, rada, radb, pa, area, tpeak, nhi, sint, optid, skipline=skipline
   nhvc     = n_elements(avccid)
   ;
   ; convert ra, dec to decimal degrees
   ra       = (float(rah) + ram/60.0)*15.0
   ds       = make_array(nhvc, /float, value=1.0)
   k        = where(decs EQ '-', nk)
   IF nk GT 0 THEN ds[k] = -1.0
   dec      = ds*(float(decd) + float(decm)/60.0)
   ;
   ; convert ra,dec to glong, glat
   glactc, ra, dec, 2000.0, glong, glat, 1, /degree
   ;
   ; get ebv from Schlegel maps
   ebv      = dust_getval(glong, glat, /interp, outfile=febv1)
   mask_ebv = dust_getval(glong, glat, map='mask', outfile=febv2)
   ;
   ; compress optical ID & name
   optid    = strcompress(temporary(optid),/remove_all)
   name     = strcompress(temporary(name),/remove_all)
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
   create_struct2, avcc, 'AVCC', item, dstring, dimen=nhvc
   ;
   ; open dbd file, write top of the file
   fildbd  = filo + '.dbd'
   openw, lu, fildbd, /get_lun
   printf, lu, '#title'
   printf, lu, title
   printf, lu, ''
   printf, lu, '#maxentries'
   printf, lu, strtrim(string(nhvc),2)
   printf, lu, ''
   printf, lu, '#items'
   ;
   ; Fill bgc structure, write item lines of dbd file
   mc     = max(strlen(item))
   pritem = ljust(strupcase(item), mc)
   ni     = n_elements(item)
   FOR i = 0, ni-1 DO BEGIN 
      cmd = "avcc."+strtrim(item[i],2)+" = "+strtrim(item[i],2)
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
   dbbuildstruct, avcc
   dbclose,dummy
   ;
   cd, cwd
END 
