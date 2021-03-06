PRO mkgalexgr5db
   ;
   ; Make database of GALEX GR5 fields
   ;
   ; G. Meurer 06/2008
   ; ----------------------------------------------------------------
   ; setup stuff
   wd      = '/home/meurer/Teltime/Galex/Cy05/'      ; directory to start work in
   ;ffile   = 'GALEX5-targets-2008-04-28.fits'  ; file containing GR5
   tfile   = 'GALEX5-targets-2008-04-28.txt'    ; file containing GR5
   fexten  = 1                                  ; extension
   filo    = 'galex_gr5'                        ; output database name
   logstr  = 'MKGALEXGR5DB: '                   ; start of all log messages
   fmti    = '(l,a,a,a,d,d,l,i,l,l)'
   ;
   ; set up things for database
   title    = 'GALEX GR5 tile characteristics database'
   item     = ['planid', 'tile', 'survey', 'ow', 'ra', 'dec', 'glat', 'glong', $
               'ebv', 'maskebv', 'texpmin', 'nvisits', 'texpnuv', 'texpfuv']
   tout     = ['I', 'C', 'C', 'B', 'R', 'R', 'R', 'R', 'R', 'B', 'I', 'I', 'I', 'I']
   tlen     = [4,0,0,1,8,8,8,8,4,1,4,2,4,4]
   descript = ['Mission planning system plan identification number', $
               'Tile name', 'GALEX survey', 'Optical Wheel setting, 0=direct imaging, 1=grism', $
               'RA (J2000) [deg]', 'Dec (J2000) [deg]', $
               'Galactic longitude [deg]', 'Galactic latitude [deg]', $
               'Galactic foreground E(B-V) (Schlegel et al) [mag]', $
               'Byte mask from Schlegel et al', 'planned total exposure [s]', $
               'Number of observations (some may have t_exp=0)', $
               'total NUV exposure [s]', 'total FUV exposure [s]']
   indblk   = ['ra','dec','glat','glong','texpfuv','texpnuv','ebv', 'ow']
   ;
   ; Go to working directory
   cd, wd, current=cwd
   ;
   ; read in data from fits file
   print, logstr+'Reading gr5 text file. '
   readcol, tfile, planid, tile, survey, cow, ra, dec, texpmin, nvisits, texpnuv, texpfuv, format=fmti
   nn      = n_elements(ra)
   print, logstr+'Number of entries read in: ', nn
   ;
   ; find where coordinates are legal
   kk      = where(ra GE 0.0 AND ra LE 360.0 AND dec GE -90.0 AND dec LE 90.0, ngood) 
   print, logstr+'Number of rows with good coordinates: ', ngood
   IF ngood LE 0 THEN BEGIN 
      print, logstr+'No objects with good coords, exiting routine. '
      return 
   ENDIF 
   ;
   ; break out arrays for good objects
   print, logstr+'Saving good entries'
   planid   = planid[kk]
   tile     = tile[kk]
   survey   = survey[kk]
   cow      = cow[kk]
   ra       = ra[kk]
   dec      = dec[kk]
   texpmin  = texpmin[kk]
   nvisits  = nvisits[kk]
   texpnuv  = texpnuv[kk]
   texpfuv  = texpfuv[kk]
   ;
   ; convert optical wheel position to a logical variable:
   ;   0 - direct imaging
   ;   1 - grism in place
   ow       = make_array(ngood, /byte, value=0b)
   kk       = where(cow EQ 'g' OR cow EQ 'G', nkk)
   ow[kk]   = 1b
   ;
   ; calculate extra quantities
   ; - galactic latitude, longitude (euler quicker than glactic)
   ; - extinction and mask at center of field of view
   print, logstr+'Calculating galactic coordinates and reddening'
   euler, ra, dec, glong, glat, 1
   ebv      = dust_getval(glong, glat, /interp)
   maskebv  = byte(dust_getval(glong, glat, map='mask'))
   ;
   ; Go to directory for databases
   zdbase   = getenv('ZDBASE')
   cd, zdbase
   ;
   ; set lengths of character items
   kk      = where(tlen EQ 0,nkk)
   IF nkk GT 0 THEN FOR ii = 0,nkk-1 DO result = execute('tlen[kk[ii]] = max(strlen('+item[kk[ii]]+'))') $
               ELSE stop
   ;
   ; open dbd file, write top of the file
   print, logstr+'Writing DBD file'
   fildbd  = filo + '.dbd'
   openw, lu, fildbd, /get_lun
   printf, lu, '#title'
   printf, lu, title
   printf, lu, ''
   printf, lu, '#maxentries'
   printf, lu, strtrim(string(ngood),2)
   printf, lu, ''
   printf, lu, '#items'
   ;
   ; write item lines of dbd file
   mc     = max(strlen(item))
   pritem = ljust(strupcase(item), mc)
   ni     = n_elements(item)
   FOR ii = 0, ni-1 DO BEGIN 
      typstr = ljust(tout[ii]+'*'+strtrim(string(tlen[ii]),2),5)
      printf,lu,pritem[ii]+'   '+typstr+'   "'+descript[ii]+'"'
   ENDFOR 
   ;
   ; write index blocks of dbd file and close
   printf,lu,'  '
   printf,lu,'#index'
   ni = n_elements(indblk)
   FOR ii = 0, ni-1 DO BEGIN 
      printf,lu,ljust(strupcase(strtrim(indblk[ii],2)),mc)+'   sort'
   ENDFOR 
   free_lun,lu   
   ;
   ; write database
   print, logstr+'Writing database'
   !PRIV = 2
   dbcreate, filo, 1, 1, /external
   dbopen, filo, 1
   dbbuild, planid, tile, survey, ow, ra, dec, glong, glat, ebv, maskebv, $
            texpmin, nvisits, texpnuv, texpfuv
   dbclose,dummy
   ;
   ; go to starting directory
   cd, cwd   
END 
