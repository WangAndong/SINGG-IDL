pro mkgalexgr6scoadd, ingest=ingest
  ;
  ; Make database of the GR6 coadd tiles as atbulated in a
  ; bunch of ASCII text files given to me by Mark Seibert
  ;
  ; G. Meurer (ICRAR/UWA) 11/2011
  ;
  logstr    = 'MKGALEXGR6SCOADD: '
  print, logstr+'starts'
  ;
  wd        = '/Volumes/data/meurer/SUNGG/GR6/Seibert/'
  fili      = 'GR6_'+['AIS_'+numstr(indgen(6)+1), $
              'CAI', 'DIS', 'GII', 'MIS', 'NGS', $
              'CSS_'+['CAI', 'DIS', 'GII', 'MIS']]+'_coadd_summary.txt'
  nfi       = n_elements(fili)
  fmti      = '(a,i,i,i,i,i,i,i,a,a,a,f,f,d,d,a,i,i)' 
  updatestr = 'Date: 2011, Nov 04'           ; **** make sure to update for each new ingest!!!
  dbout     = 'galexgr6_seibert_coadd'
  zdbase    = getenv('ZDBASE')
  ;
  ; set up things for database
  title     = 'GALEX GR6 co-add (Seibert) tile database. '+updatestr
  item      = ['obsname', 'ra', 'dec', 'glong', 'glat', 'ebv', 'maskebv', $
               'texpnuv', 'texpfuv', 'mcat', 'nrrhr', 'nskybg', 'nflag', 'ngrel', $
               'frrhr', 'fskybg', 'fflag', 'fgrel', 'qagrade', 'catalog', $
               'ascvisit', 'mscvisit']
  tout      = ['C', 'R', 'R', 'R', 'R', 'R', 'B', 'R', 'R', 'B', 'B', 'B', 'B', 'C', 'B', 'B', 'B', 'C', $
              'C', 'C', 'I', 'I']
  tlen      = [0, 8, 8, 8, 8, 4, 1, 4, 4, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 2, 2]
  descript  = ['Observation (tile) name', 'RA (J2000) [deg]', 'Dec (J2000) [deg]', $
               'Galactic longitude [deg]', 'Galactic latitude [deg]', $
               'Galactic foreground E(B-V) (Schlegel et al) [mag]', $
               'Byte mask from Schlegel et al', $
               'total NUV exposure [s]', 'total FUV exposure [s]', $
               'MCAT??? ', 'NUV RRHR???', 'NUV skybg ???', 'NUV flag ???', 'NUV galex software release ???', $
               'FUV RRHR???', 'FUV skybg ???', 'FUV flag ???', 'FUV galex software release ???', $
               'QA grade', 'Catalog', 'ASC visit?', 'MSC visit?']
  indblk    = ['ra', 'dec', 'glong', 'glat', 'texpfuv', 'texpnuv', 'ebv']
  ;
  ; go to working directory
  print, logstr+'going to working directory: '+wd
  cd, wd, current=cwd
  ;
  ; read in files creating and then concatenating the arrays that 
  ; will make the database
  for ii = 0, nfi-1 do begin 
     print, logstr+'will read in file: '+fili[ii]
     if ii eq 0 then begin 
        readcol, fili[ii], obsname, mcat, nrrhr, nskybg, nflag, frrhr, fskybg, fflag, $
                 ngrel, fgrel, qagrade, ntime, ftime, ra, dec, catalog, ascvisit, $
                 mscvisit, format=fmti
     endif else begin 
        readcol, fili[ii], _obsname, _mcat, _nrrhr, _nskybg, _nflag, _frrhr, _fskybg, _fflag, $
                 _ngrel, _fgrel, _qagrade, _ntime, _ftime, _ra, _dec, _catalog, _ascvisit, $
                 _mscvisit, format=fmti
        obsname  = [obsname, _obsname]
        mcat     = [mcat, _mcat]
        nrrhr    = [nrrhr, _nrrhr]
        nskybg   = [nskybg, _nskybg]
        nflag    = [nflag, _nflag]
        frrhr    = [frrhr, _frrhr]
        fskybg   = [fskybg, _fskybg]
        fflag    = [fflag, _fflag]
        ngrel    = [ngrel, _ngrel]
        fgrel    = [fgrel, _fgrel]
        qagrade  = [qagrade, _qagrade]
        ntime    = [ntime, _ntime]
        ftime    = [ftime, _ftime]
        ra       = [ra, _ra]
        dec      = [dec, _dec]
        catalog  = [catalog, _catalog]
        ascvisit = [ascvisit, _ascvisit]
        mscvisit = [mscvisit, _mscvisit]
     endelse 
  endfor 
  ;
  nt      = n_elements(obsname)
  print, logstr+'Number of records read in from all files: '+numstr(nt)
  ;
  ; wrap RA
  ra      = ra mod 360.0
  ;
  ; find where coordinates are legal
  kk      = where(ra GE 0.0d0 AND ra LE 360.0d0 AND dec GE -90.0d0 AND dec LE 90.0d0 and finite(ra) and finite(dec), ngood) 
  print, logstr+'Number of records with good coordinates: '+numstr(ngood)
  print, logstr+'Number of records with bad coords:       '+numstr(nt-ngood)
  IF ngood LE 0 THEN BEGIN 
     print, logstr+'No objects with good coords, exiting routine. '
     stop
  ENDIF 
  ;
  ; sort by RA and rearrange arrays
  print, logstr+'sorting good records by RA and re-storing sorted arrays'
  jj       = sort(ra[kk])
  kk       = kk[jj]
  obsname  = temporary(obsname[kk])
  mcat     = temporary(byte(mcat[kk]))
  nrrhr    = temporary(byte(nrrhr[kk]))
  nskybg   = temporary(byte(nskybg[kk]))
  nflag    = temporary(byte(nflag[kk]))
  frrhr    = temporary(byte(frrhr[kk]))
  fskybg   = temporary(byte(fskybg[kk]))
  fflag    = temporary(byte(fflag[kk]))
  ngrel    = temporary(ngrel[kk])
  fgrel    = temporary(fgrel[kk])
  qagrade  = temporary(qagrade[kk])
  ntime    = temporary(ntime[kk])
  ftime    = temporary(ftime[kk])
  ra       = temporary(ra[kk])
  dec      = temporary(dec[kk])
  catalog  = temporary(catalog[kk])
  ascvisit = temporary(ascvisit[kk])
  mscvisit = temporary(mscvisit[kk])
  ;
  ; calculate extra quantities
  ; - galactic latitude, longitude (euler quicker than glactic)
  ; - extinction and mask at center of field of view
  print, logstr+'Calculating galactic coordinates and reddening'
  euler, ra, dec, glong, glat, 1
  ebv      = dust_getval(glong, glat, /interp)
  maskebv  = byte(dust_getval(glong, glat, map='mask'))
  ;
  ; set lengths of character items
  kk      = where(tlen EQ 0,nkk)
  IF nkk GT 0 THEN FOR ii = 0,nkk-1 DO result = execute('tlen[kk[ii]] = max(strlen('+item[kk[ii]]+'))') $
              ELSE stop
  ;
  ; open dbd file, write top of the file
  print, logstr+'Writing DBD file'
  fildbd  = dbout + '.dbd'
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
  dbcreate, dbout, 1, 1, /external
  dbopen, dbout, 1
  dbbuild, obsname, ra, dec, glong, glat, ebv, maskebv, ntime, ftime, mcat, $
           nrrhr, nskybg, nflag, ngrel, frrhr, fskybg, fflag, fgrel, qagrade, $
           catalog, ascvisit, mscvisit
  dbclose,dummy
  ;
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
  ; go to starting directory
  print, logstr+'going to initial directory: '
  cd, cwd   
  print, logstr+'finished '
end
