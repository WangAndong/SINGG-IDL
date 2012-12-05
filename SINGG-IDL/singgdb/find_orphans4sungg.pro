PRO find_orphans4sungg
  ;
  ; find galaxies that are not in the sungg database but are in 
  ; galex tiles that are in the database.
  ;
  ; Do this using supersingg_master database
  ;
  ; G. Meurer 7/2012
  ;
  mdb       = 'supersingg_master'
  hdb       = 'hopcat'
  gdb       = 'galexgr6_seibert_coadd'
  filo      = 'sungg_orphans.dat'
  srad1     = 0.5*60.0          ; galex search radius in degrees
  hdr1      = ['# Halpha detections not measured in the FUV', ' ', $
               '#glxtile orphan             RA         Dec      HIPASS     SNAME_a        SNAME_b        n_a n_b']
  hdr2      = ['# HIPASS galaxies not measured in UV', '# ', $
               '#glxtile      fieldname               RA         Dec      nm  HIPASS     HNAME_a    HNAME_b      n_a n_b']
  nhdr1     = n_elements(hdr1)
  nhdr2     = n_elements(hdr2)
  ;
  ; open database
  dbopen, mdb
  ;
  ; find galaxies that have been measured in the UV, 
  ; so that they can be excluded later on 
  ; (i.e. second stage, finding unmeasured HIPASS galaxies in tiles where
  ;  we have measurements)
  ;enouv     = dbfind('entfuvphot < 0')
  ;dbext, enouv, 'hname', hnam_nouv
  ;ss         = sort(hnam_nouv)
  ;uu         = uniq(hnam_nouv[ss])
  ;hnam_nouv  = hnam_nouv[ss[uu]]
  ;
  ; get tiles of entries that have measurements
  ; this is also for use later on
  eyesuv    = dbfind('entfuvphot > 1')
  dbext, eyesuv, 'hname,entgalexa,entgalexb', hnam_yesuv0, egtilea_sungg, egtileb_sungg
  ss         = sort(hnam_yesuv0)
  uu         = uniq(hnam_yesuv0[ss])
  hnam_yesuv = hnam_yesuv0[ss[uu]]
  nyesuv     = n_elements(hnam_yesuv)
  ;
  gtile      = [egtilea_sungg, egtileb_sungg]
  ss         = sort(gtile)
  uu         = uniq(gtile[ss])
  kk         = where(gtile[ss[uu]] GT 0, nkk)
  IF nkk EQ 0 THEN stop, 'No usable tiles???!!!'
  gtile      = gtile[ss[uu[kk]]]
  ngtile     = n_elements(gtile)
  ;
  ; find galaxies with Halpha and entries in SUNGG UV database
  egood      = dbfind('entoptphot > 1, entfuvphot > 1')
  ngood      = n_elements(egood)
  ;
  ; get corresponding entries in galex tile DB
  dbext, egood, 'sname,hname,entgalexa,entgalexb', snameg, hnameg, egooda, egoodb
  tilegood  = [egooda, egoodb]
  ;
  ; find galaxies without entries in SUNGG UV database
  ebad      = dbfind('entfuvphot < 0')
  nbad      = n_elements(ebad)
  ;
  ; get corresponding entries in galex tile DB
  dbext, ebad, 'sname,hname,ra,dec,entgalexa,entgalexb', snameb, hnameb, rab, decb, ebada, ebadb
  ;
  ; trim to cases where there are galex tiles
  pp        = where((ebada GE 1) OR (ebadb GE 1), npp)
  IF npp LE 0 THEN stop, 'Huh, no valid tiles????'
  ebad      = ebad[pp]
  snameb    = snameb[pp]
  hnameb    = hnameb[pp]
  rab       = rab[pp]
  decb      = decb[pp]
  ebada     = ebada[pp]
  ebadb     = ebadb[pp]
  ;
  ; in cases where ebada ne ebadb make a second copy of the arrays
  qq        = where(ebada NE ebadb, nqq)
  IF nqq GT 0 THEN BEGIN 
     tilebad = [ebada, ebadb[qq]]
     snameb  = [snameb, snameb[qq]]
     hnameb  = [hnameb, hnameb[qq]]
     rab     = [rab, rab[qq]]
     decb    = [decb, decb[qq]]
     ebada   = [ebada, ebada[qq]]
     ebadb   = [ebadb, ebadb[qq]]
  ENDIF ELSE BEGIN 
     tilebad = ebada
  ENDELSE 
  ;
  ; get unique entries in tile databases, 
  ; those with SUNGG entries
  ss        = sort(tilegood)
  uu        = uniq(tilegood[ss])
  utilegood = tilegood[ss[uu]]
  nutg      = n_elements(utilegood)
  ;
  ; array for marking orphans to get
  orphan    = make_array(nbad, /byte, value=0b)
  ;
  ; loop through tiles containing valid SUNGG entries
  ; and match to the tiles of sources not in SUNGG
  FOR ii = 0, nutg-1 DO BEGIN 
     jj = where(tilebad EQ utilegood[ii], njj)
     IF njj GT 0 THEN orphan[jj] = 1b
  ENDFOR 
  ;
  ; open output file, write header
  openw, lu, filo, /get_lun
  FOR ii = 0, nhdr1-1 DO BEGIN 
     printf, lu, hdr1[ii], format='(a)'
     printf, -1, hdr1[ii], format='(a)'
  ENDFOR 
  ;
  ; list the orphans
  kk = where(orphan GT 0, nkk)
  print, 'Number of orphans = ', nkk
  FOR ii = 0, nkk-1 DO BEGIN 
     jj     = kk[ii]
     ptra   = where(egooda EQ tilebad[jj], nptra)
     ptrb   = where(egoodb EQ tilebad[jj], nptrb)
     srcb   = '--'
     IF nptra GT 0 THEN srca = snameg[ptra[0]] ELSE srca   = '--'
     IF nptrb GT 0 THEN srcb = snameg[ptrb[0]] ELSE srcb   = '--'
     printf, lu, tilebad[jj],ljust(snameb[jj],15),rab[jj],decb[jj],ljust(hnameb[jj],11)+ljust(srca,15)+ljust(srcb,15),nptra,nptrb,format='(i7,2x,a15,f11.5,f11.5,2x,a41,i3,i3)'
     printf, -1, tilebad[jj],ljust(snameb[jj],15),rab[jj],decb[jj],ljust(hnameb[jj],11)+ljust(srca,15)+ljust(srcb,15),nptra,nptrb,format='(i7,2x,a15,f11.5,f11.5,2x,a41,i3,i3)'
  ENDFOR 
  ;
  ; start second part of search, finding orphan HIPASS galaxies
  ; Use HOPCAT as starting point.
  ; First get properties of all hopcat entries 
  dbopen, hdb
  dbext, -1, 'name_hipass,ra,dec',nameh,rah,dech
  nh   = n_elements(nameh)
  used = make_array(nh, /byte, value=0b)
  ;
  ; exclude galaxies that have been measured in the UV
  ; already
  FOR ii = 0, nyesuv-1 DO BEGIN 
     jj  = where(nameh EQ hnam_yesuv[ii], njj)
     print, hnam_yesuv[ii]+' : numberof entries that will be excluded : '+numstr(njj)
     IF njj GT 0 THEN used[jj] = 1b
  ENDFOR 
  ;
  jj   = where(used EQ 0b, ngals2check)
  IF ngals2check GT 0 THEN BEGIN 
     print, 'Will look at used tiles for matches to '+numstr(ngals2check)+' entries in HOPCAT'
     nameh = nameh[jj]
     rah   = rah[jj]
     dech  = dech[jj]
  ENDIF 
  dbclose
  ; 
  ; only continue if there are galaxies to check
  IF ngals2check GT 0 THEN BEGIN 
     nmatchglx   = make_array(ngals2check, /int, value=0)
     entgalexa   = make_array(ngals2check, /long, value=0)
     entgalexb   = make_array(ngals2check, /long, value=0)
     obsnameglxa = make_array(ngals2check, /string, value='')
     obsnameglxb = make_array(ngals2check, /string, value='')
     sepglxa     = make_array(ngals2check, /float, value=999.9)
     sepglxb     = make_array(ngals2check, /float, value=999.9)
     tfuvglxa    = make_array(ngals2check, /float, value=-1.0)
     tfuvglxb    = make_array(ngals2check, /float, value=-1.0)
     tnuvglxa    = make_array(ngals2check, /float, value=-1.0)
     tnuvglxb    = make_array(ngals2check, /float, value=-1.0)
     nmatchglx   = make_array(ngals2check, /int, value=0)
     umatch      = make_array(ngals2check, /byte, value=0b)
     ;
     ; open galex tile database
     dbopen, gdb
     ;
     ; loop through galaxies to check
     FOR ii = 0, ngals2check-1 DO BEGIN 
        list2  = dbcircled(rah[ii], dech[ii], srad1, sep, gtile,/silent)
        IF list2[0] GT -1 THEN BEGIN 
           nn              = n_elements(list2)
           nmatchglx[ii]   = nn
           dbext, list2, 'entry,obsname,texpfuv,texpnuv', ent, obsname, texpfuv, texpnuv
           jj              = sort(sep)
           entgalexa[ii]   = ent[jj[0]]
           obsnameglxa[ii] = obsname[jj[0]]
           sepglxa[ii]     = sep[jj[0]]
           tfuvglxa[ii]    = texpfuv[jj[0]]
           tnuvglxa[ii]    = texpnuv[jj[0]]
           jj              = reverse(sort(texpfuv))
           entgalexb[ii]   = ent[jj[0]]
           obsnameglxb[ii] = obsname[jj[0]]
           sepglxb[ii]     = sep[jj[0]]
           tfuvglxb[ii]    = texpfuv[jj[0]]
           tnuvglxb[ii]    = texpnuv[jj[0]]
           print,nameh[ii]+'  ',nn
        ENDIF 
     ENDFOR 
  ENDIF 
  ;
  ; determine how many new orphans there are
  pp = where(nmatchglx GT 0, npp)
  print, 'number of new orphans: '+numstr(npp)
  IF npp GT 0 THEN BEGIN
     ;
     ; pare down lists to just the new orphans
     nameh       = nameh[pp]
     rah         = rah[pp]
     dech        = dech[pp]
     nmatchglx   = nmatchglx[pp]
     entgalexa   = entgalexa[pp]
     obsnameglxa = obsnameglxa[pp]
     sepglxa     = sepglxa[pp]
     tfuvglxa    = tfuvglxa[pp]
     tnuvglxa    = tnuvglxa[pp]
     entgalexb   = entgalexb[pp]
     obsnameglxb = obsnameglxb[pp]
     sepglxb     = sepglxb[pp]
     tfuvglxb    = tfuvglxb[pp]
     tnuvglxb    = tnuvglxb[pp]
     ;
     ; print out header for second section
     FOR ii = 0, nhdr2-1 DO BEGIN 
        printf, lu, hdr2[ii], format='(a)'
        printf, -1, hdr2[ii], format='(a)'
     ENDFOR 
     ;
     ; print data on new orphans
     FOR jj = 0, npp-1 DO BEGIN
        ka       = where(egtilea_sungg EQ entgalexb[jj], nka)
        kb       = where(egtileb_sungg EQ entgalexb[jj], nkb)
        IF nka GT 0 THEN srca = hnam_yesuv0[ka[0]] ELSE srca = '--'
        IF nkb GT 0 THEN srcb = hnam_yesuv0[kb[0]] ELSE srcb = '--'
        printf, -1, entgalexb[jj],ljust(obsnameglxb[jj],20),rah[jj],dech[jj],nmatchglx[jj],ljust(nameh[jj],11)+ljust(srca,11)+ljust(srcb,11),nka,nkb,format='(i7,2x,a25,f11.5,f11.5,i4,2x,a33,i4,i4)'
        printf, lu, entgalexb[jj],ljust(obsnameglxb[jj],20),rah[jj],dech[jj],nmatchglx[jj],ljust(nameh[jj],11)+ljust(srca,11)+ljust(srcb,11),nka,nkb,format='(i7,2x,a25,f11.5,f11.5,i4,2x,a33,i4,i4)'
     ENDFOR 
  ENDIF 
  ;
  ; close output file
  free_lun, lu
END
