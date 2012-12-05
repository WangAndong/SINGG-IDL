PRO mkgalexstatdb
   ;
   ; Make database of GALEX GR5 fields
   ;
   ; G. Meurer 06/2008
   ; G. Meurer 05/2009 update string added to title
   ;                   change so that files not put into ZDBASE
   ;                   until checking if there are already files 
   ;                   there, if so update_idldb is used.
   ; G. Meurer 11/2009 copy and modified from mkgalexgr5alldb
   ; ----------------------------------------------------------------
   ; setup stuff
   wd        = '/home/meurer/ZDBASE/Ingest/'      ; directory to start work in
   ;updatestr = 'Date: 2008 June(?).'           ; **** make sure to update for each new ingest!!!
   ;ffile     = 'galex_obs_status_2008.fits'    ; fits file for entire database
   updatestr = 'Date: 2009 Oct 22.'           ; **** make sure to update for each new ingest!!!
   ffile     = 'mission_status_20091022.fits'    ; fits file for entire database
   fexten    = 1                               ; extension
   filo      = 'galex_status'                  ; output database name
   logstr    = 'MKGALEXSTATDB: '             ; start of all log messages
   fmti      = '(l,a,a,a,d,d,l,i,l,l)'
   ;
   ; set up things for database
   title    = 'GALEX Mission Status database. '+updatestr
   item     = ['eclipse', 'obsname', 'subvisit', 'survey', 'ow', 'ra', 'dec', 'glong', 'glat', $
               'ebv', 'maskebv', 'texpplan', 'texpnuv', 'texpfuv']
   tout     = ['I', 'C', 'I', 'C', 'B', 'R', 'R', 'R', 'R', 'R', 'B', 'R', 'R', 'R']
   tlen     = [4, 0, 4, 0, 1, 8, 8, 8, 8, 4, 1, 8, 8, 8]
   descript = ['Eclipse', 'Observation Name', 'Sub Visit Number', $ 
               'Survey type', 'Optical Wheel setting, 0=direct imaging, 1=grism', $
               'RA (J2000) [deg]', 'Dec (J2000) [deg]', $
               'Galactic longitude [deg]', 'Galactic latitude [deg]', $
               'Galactic foreground E(B-V) (Schlegel et al) [mag]', $
               'Byte mask from Schlegel et al', $
               'planned total exposure [s]', $
               'total NUV exposure [s]', 'total FUV exposure [s]']
   indblk   = ['ra','dec', 'eclipse', 'glat','glong','texpfuv','texpnuv','ebv', 'ow']
   ;
   ; Go to working directory
   cd, wd, current=cwd
   ;
   ; read in data from fits file
   print, logstr+'Reading fits file: '+ffile
   result    = mrdfits(ffile, 1)
   ;
   ; break out arrays
   print, logstr+'breaking out arrays. '
   eclipse    = result.eclipse
   ;obsname    = result.obs_name
   obsname    = result.tile
   subvisit   = result.sub_visit
   ;survey     = result.survey
   survey     = result.surv_type
   cow        = result.ow
   ;ra         = result.ra
   ;dec        = result.dec
   ra         = result.ra0
   dec        = result.dec0
   ;nvisitsf   = result.fuv_visits
   ;nvisitsn   = result.nuv_visits
   ;texpplan   = result.plan_exp
   ;texpfuv    = result.fuv_exp
   ;texpnuv    = result.nuv_exp
   texpplan   = result.exptime_pred
   texpfuv    = result.fuv_exptime
   texpnuv    = result.nuv_exptime
   ;texpfgr4   = result.fuv_gr4_exp
   ;texpngr4   = result.nuv_gr4_exp
   ;texpfgr4   = 0.0*texpfuv
   ;texpngr4   = 0.0*texpnuv
   result     = 0.0
   nn         = n_elements(ra)
   print, logstr+'Number of entries read in: ', nn
   ;
   ; find where coordinates are legal
   kk      = where(ra GE 0.0 AND ra LE 360.0 AND dec GE -90.0 AND dec LE 90.0, ngood) 
   ;
   ; kludge to make short db
   ;ngood   = 100
   ;kk      = kk[lindgen(100)]
   ;
   print, logstr+'Number of rows with good coordinates: ', ngood
   IF ngood LE 0 THEN BEGIN 
      print, logstr+'No objects with good coords, exiting routine. '
      return 
   ENDIF 
   ;
   ; break out arrays for good objects
   print, logstr+'Saving good entries'

   eclipse    = eclipse[kk]
   obsname    = obsname[kk]
   subvisit   = subvisit[kk]
   survey     = survey[kk]
   cow        = cow[kk]
   ra         = ra[kk]
   dec        = dec[kk]
   ;nvisitsf   = nvisitsf[kk]
   ;nvisitsn   = nvisitsn[kk]
   texpplan   = texpplan[kk]
   texpfuv    = texpfuv[kk]
   texpnuv    = texpnuv[kk]
   ;texpfgr4   = texpfgr4[kk]
   ;texpngr4   = texpngr4[kk]
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
   ; **** don't do this it will overwrite!!!
   ; zdbase   = getenv('ZDBASE')
   ; cd, zdbase
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
   dbbuild, eclipse, obsname, subvisit, survey, ow, ra, dec, glong, glat, $
            ebv, maskebv, texpplan, texpnuv, texpfuv
   dbclose,dummy
   ;
   ; check if the database (the dbd file) exists already at $ZDBASE
   ; * if so use update_idldb
   ; * otherwise do a straight copy into $ZDBASE
   zdbase   = getenv('ZDBASE')
   cd, zdbase
   stat     = file_info(filo+'.dbd')
   IF stat.exists THEN BEGIN 
      update_idldb, filo, dbdir=wd
   ENDIF ELSE BEGIN 
      spawn,'/bin/cp -f '+wd+filo+'db? .'
   ENDELSE 
   ;
   ; go to starting directory
   cd, cwd   
END 
