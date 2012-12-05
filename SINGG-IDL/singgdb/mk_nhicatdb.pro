PRO mk_nhicatdb, ingest=ingest
  ;
  ; make Northern HIPASS catalogue NHICAT as an IDL
  ; Database.
  ;
  ; G. Meurer (ICRAR/UWA) 11/2011
  ;
  logfile   = 'mk_nhicatdb.log'
  logstr    = 'MK_NHICATDB: '
  openw, llog, logfile, /get_lun
  plog, llog, logstr, 'starting... '
  ;
  ; setup stuff
  ;   zdbase  : where IDL databases are stored
  ;   wd      : working directory, where the NHICAT catalogue is stored
  ;             as a binary fits file
  ;   fcatin  : file name of the binary fits file input catalogue
  ;   extin   : extension of binary fits file input catalogue
  zdbase    = getenv('ZDBASE')
  if strmid(zdbase,strlen(zdbase)-1) NE '/' then zdbase = zdbase+'/'
  wd        = zdbase+'Ingest/'    
  fcatin    = 'nhicat.fits'
  extin     = 1
  dbout     = 'nhicat'
  ;
  ; define stuff needed to read in binary fits file and
  ; convert to an IDL database
  ;  tnam     : column names in binary fits file corresponding to
  ;             "item" below.  Where this = "null" means there is no
  ;             corresponding item
  ;  title    : database title
  ;  item     : item names of the quantities (columns) stored in the
  ;             database
  ;  type     : datatype for the items
  ;  tlen     : bytes for each item.  For strings set this to 0 for now
  ;             and work this out later.
  ;  descript : descriptions of each item
  ;
  tnam      = ['hipass', '_raj2000', '_dej2000', 'null', 'null', 'null', 'null', 'rv50max', 'rv50min', $
               'rv20max', 'rv20min', 'rvmom', 'rvsp', 'rvgsr', $
               'rvlg', 'rvcmb', 'rv1', 'rv2', 'rvsp1', 'rvsp2', $
               'rvmask', 'w50max', 'w50min', 'w20max', 'w20min', $
               'speak', 'sint', 'rms', 'rmsclip', 'rmscube', 'cube', 'sigma', 'boxsize', $
               'q', 'nb', 'cf', 'ext']
  title     = 'Northern HI Parkes All Sky Survey Catalogue (NHICAT) (Wong+, 2006)'
  item      = ['hipass_name', 'ra', 'dec', 'glat', 'glong', 'ebv', 'mask_ebv', 'vel_50max', 'vel_50min', $
               'vel_20max', 'vel_20min', 'vel_mom', 'vel_sp', 'vel_gsr', $
               'vel_lg', 'vel_cmb', 'vel_lo', 'vel_hi', 'vel_speclo', 'vel_spechi', $
               'vel_mask', 'width_50max', 'width_50min', 'width_20max', 'width_20min', $
               'sp', 'sint', 'rms', 'rms_clip', 'rms_cube', 'cube', 'sigma', 'boxsize', $
               'comment', 'nb_flg', 'cfsd_flg', 'ext_flag']
  type      = ['C', 'R', 'R', 'R', 'R', 'R', 'B', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R',$
               'R', 'R', 'C', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'I', 'I', 'I', 'I',$
               'I', 'I', 'I']
  tlen      = [0, 8, 8, 8, 8, 4, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 8, 4, 4, 4, 4, $
               4, 4, 4, 4, 4, 4, 4]
  descript  = ['HIPASS name', 'RA (J2000) [deg]', 'Dec (J2000) [deg]', $
               'Galactic longitude [deg]', 'Galactic latitude [deg]', $
               'Galactic foreground E(B-V) (Schlegel et al) [mag]', $
               'Byte mask from Schlegel et al', $
                'mean Vhel at 0.5*Sp (using width max proc) [km/s]', $
               'mean Vhel at 0.5*Sp (using width min proc) [km/s]', $
               'mean Vhel at 0.2*Sp (using width max proc) [km/s]', $
               'mean Vhel at 0.2*Sp (using width min proc) [km/s]', $
               'flux weighted Vhel between vel_lo and vel_hi [km/s]', $
               'velocity at which profile peak flux occurs [km/s]', $
               'Gal stand rest vel (de Vaucouleurs 1991) [km/s]', $
               'Local Group vel (Karachentsev 1996) [km/s]', $
               'CMB velocity (Fixsen 1996) [km/s]', $
               'manual minimum profile Vhel [km/s]', $
               'manual maximum profile Vhel [km/s]', $
               'manual minimum Vhel for plots and RMS [km/s]', $
               'manual maximum Vhel for plots and RMS [km/s]', $
               'manual Vhel mask regions over (in pairs) [km/s]', $
               'width at 0.5*Sp (using width max proc) [km/s]', $
               'width at 0.5*Sp (using width min proc) [km/s]', $
               'width at 0.2*Sp (using width max proc) [km/s]', $
               'width at 0.2*Sp (using width min proc) [km/s]', $
               'peak flux density of profile [Jy]', $
               'integrated flux of source  [Jy km/s]', $
               'RMS of masked spectrum [Jy]', 'clipped RMS of masked spectrum  [Jy]', $
               'unsmoothed RMS of cube [Jy]', 'cube number', $
               'stand. dev. of Gaussian used in baseline smoothing [km/s]', $
               'box size used for parameter measurements [arcmin]', $
               'comment (1=real, 2=have concerns)', $
               'narrow band follow-up status flag (1=real)', $
               'confused source flag (1=confused)', $
               'extended source flag (1=extended)']
   indblk   = ['ra','dec','vel_50max','width_50max','sp','sint','glat','glong', 'ebv']
   ;
   ; go to working directory and read in fits file
   plog, llog, logstr, 'going to working directory: '+wd
   cd, wd, current=cwd
   plog, llog, logstr, 'reading input binary fits file: '+fcatin
   cat      = mrdfits(fcatin, extin, hdcat)
   ;
   ; peel off items from structure as named arrays
   ii       = where(tnam ne 'null', nii)
   plog, llog, logstr, 'creating named variables for '+numstr(nii)+' items '
   for jj = 0, nii-1 do begin 
      kk    = ii[jj]
      str   = item[kk]+'=cat.'+tnam[kk]
      res   = execute(str)
      plog, llog, logstr, ' result of executing:   '+str+'   => '+numstr(res)
   endfor 
   ;
   nobj     = n_elements(name)
   ;
   ; calculate galactic coords
   ; note euler is quicker than glactic, but glactic is retained to stay
   ; compatible with mkhicatdb.pro
   plog, llog, logstr, 'calculating Galactic coordinates '
   glactc, ra, dec, 2000.0, glong, glat, 1, /degree
   ;euler, ra, dec, glong2, glat2, 1
   ;
   ; determine foreground dust extinction
   plog, llog, logstr, 'determining foreground dust extinction'
   ebv      = dust_getval(glong, glat, /interp, outfile=febv1)
   mask_ebv = dust_getval(glong, glat, map='mask', outfile=febv2)
   mask_ebv = byte(mask_ebv)
   ;
   ; set lengths of character items
   plog, llog, logstr, 'setting lengths to character items'
   kk      = where(tlen EQ 0,nkk)
   IF nkk GT 0 THEN FOR ii = 0,nkk-1 DO result = execute('tlen[kk[ii]] = max(strlen('+item[kk[ii]]+'))') $
               ELSE plog, logstr,' no chracater items ???? '
   ;
   ; open dbd file, write top of the file
   fildbd  = dbout + '.dbd'
   plog, llog, logstr, 'Writing DBD file: '+fildbd
   openw, lu, fildbd, /get_lun
   printf, lu, '#title'
   printf, lu, title
   printf, lu, ''
   printf, lu, '#maxentries'
   printf, lu, strtrim(string(nobj),2)
   printf, lu, ''
   ;
   ; write item lines of dbd file
   printf, lu, '#items'
   mc     = max(strlen(item))
   pritem = ljust(strupcase(item), mc)
   ni     = n_elements(item)
   FOR ii = 0, ni-1 DO BEGIN 
      typstr = ljust(type[ii]+'*'+strtrim(string(tlen[ii]),2),5)
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
   plog, llog, logstr, 'Writing database: '+dbout
   !PRIV = 2
   dbcreate, dbout, 1, 1, /external
   dbopen, dbout, 1
   dbbuild_new, hipass_name, ra, dec, glat, glong, ebv, mask_ebv, vel_50max, vel_50min, $
                vel_20max, vel_20min, vel_mom, vel_sp, vel_gsr, $
                vel_lg, vel_cmb, vel_lo, vel_hi, vel_speclo, vel_spechi, $
                vel_mask, width_50max, width_50min, width_20max, width_20min, $
                sp, sint, rms, rms_clip, rms_cube, cube, sigma, boxsize, $
                comment, nb_flg, cfsd_flg, ext_flag
   dbclose,dummy
   ;
   ; ingest database (put into $ZDBASE) if requested
   if keyword_set(ingest) then begin 
      ;
      ; check if the database (the dbd file) exists already at $ZDBASE
      ; * if so use update_idldb
      ; * otherwise do a straight copy into $ZDBASE
      cd, zdbase
      stat     = file_info(dbout+'.dbd')
      IF stat.exists THEN BEGIN 
         update_idldb, dbout, dbdir=wd
      ENDIF ELSE BEGIN 
         spawn,'/bin/cp -f '+wd+dbout+'.db? .'
      ENDELSE 
   endif 
   ;
   ;
   ; go to starting directory, close log file and end
   plog, llog, logstr, 'returning to starting diriectory '+cwd
   cd, cwd   
   plog, llog, logstr, 'finished.'
   free_lun, llog
END
