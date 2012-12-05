PRO biggal_list
  ;
  ;  Find the galaxies in HICAT that have EXT_FLAG=1,
  ;  print out basic properties of these from SINGG
  ;  and SUNGG databases.
  ; 
  ;  G. Meurer (ICRAR/UWA) 4/2012
  ;
  hdb        = 'hicat_feb04'
  odb        = 'singg_derived'
  udb        = 'super_sungg_derived'
  wd         = getenv('SHOME')+'/Sample/'
  lamf       = 1538.6 
  lamn       = 2315.7
  lamv       = 5470.0
  mag0st     = -21.1
  filo1      = 'biggal_list_all.dat'
  filo2      = 'biggal_list_uvo.dat'
  fmto       = '(a47,f8.1,f7.1,f7.1,a2,i3,f8.1,f6.1,f7.2,f8.2,a2,i3,f8.1,f6.1,f7.2,f7.2)'
  hdr1       = '<------------------------ HI properties -----------------------------> <----------- SINGG -------------> <----------- SUNGG ----------->'
  hdr2       = 'HIPASS+    Optid           RA          Dec        V_hel   W_50   Dist | Nop  Radop   a/b  mag_r  lflxha | Nuv  Raduv   a/b  mag_n  mag_f'
  hdr3       = '----------------------------------------------------------------------|---------------------------------|-------------------------------'
  ;
  cd, wd, current=cwd
  ;
  ; open database, and find sources that are "extended"
  dbopen, hdb
  enth       = dbfind('EXT_FLG = 1')
  nbig       = n_elements(enth)
  ;
  ; extract relevant quantities from HIPASS database, and close
  dbext, enth, 'HIPASS_NAME,RA,DEC,VEL_50MAX,WIDTH_50MAX,WIDTH_20MAX,SP,SINT,DISTANCE,LOGMHI', $
                nameh,rah,dech,vhel,w50,w20,sp,sint,dist,lmhi
  dbclose
  ;
  ; create arrays to fill with optical (& UV) data
  nopt        = make_array(nbig, /int, value=0)
  nameo       = make_array(nbig, /string, value='-')
  ido         = make_array(nbig, /string, value='-')
  ento        = make_array(nbig, /int, value=0)
  rao         = make_array(nbig, /float, value=-1.0)
  deco        = make_array(nbig, /float, value=-1.0)
  rado        = make_array(nbig, /float, value=-1.0)
  axrato      = make_array(nbig, /float, value=-1.0)
  magr        = make_array(nbig, /float, value=-1.0)
  lflxha      = make_array(nbig, /float, value=-1.0)
  nameu       = make_array(nbig, /string, value='-')
  idu         = make_array(nbig, /string, value='-')
  nnuv        = make_array(nbig, /int, value=0)
  entun       = make_array(nbig, /int, value=0)
  entuf       = make_array(nbig, /int, value=0)
  rau         = make_array(nbig, /float, value=-1.0)
  decu        = make_array(nbig, /float, value=-1.0)
  radu        = make_array(nbig, /float, value=-1.0)
  axratu      = make_array(nbig, /float, value=-1.0)
  magun       = make_array(nbig, /float, value=-1.0)
  maguf       = make_array(nbig, /float, value=-1.0)
  magun0      = make_array(nbig, /float, value=-1.0)
  maguf0      = make_array(nbig, /float, value=-1.0)
  flxun       = make_array(nbig, /float, value=-1.0)
  flxuf       = make_array(nbig, /float, value=-1.0)
  ;
  ; open optical database, find good entries
  dbopen, odb
  goodo       = good_derived3()
  ;
  ; loop through entries and match
  FOR ii = 0, nbig-1 DO BEGIN 
     sstr     = 'object='+strtrim(nameh[ii],2)
     list     = dbfind(sstr,goodo)
     IF list[0] GT 0 THEN BEGIN 
        nopt[ii]   = n_elements(list)
        ;
        ; extract what we need from all matches
        ; and then put data for the first of these in the arrays
        dbext, list, 'name,optid,ra,dec,rmax_f,axerat,mapp_r_t,logf_ha_t', $
                      name_,optid_,ra_,dec_,rado_,axrat_,magr_,lflxha_
        nameo[ii]  = name_[0]
        ido[ii]    = optid_[0]
        ento[ii]   = list[0]
        rao[ii]    = ra_[0]
        deco[ii]   = dec_[0]
        rado[ii]   = rado_[0]
        axrato[ii] = axrat_[0]
        magr[ii]   = magr_[0]
        lflxha[ii] = lflxha_[0]
     ENDIF 
  ENDFOR
  ;
  ; close database
  dbclose
  ;
  ; open UV database, get nuv entries
  dbopen, udb
  goodn  = dbfind('FILTER=NUV')
  ;
  ; loop through entries and match
  FOR ii = 0, nbig-1 DO BEGIN 
     sstr     = 'hipname='+strtrim(nameh[ii],2)
     list     = dbfind(sstr,goodn)
      IF list[0] GT 0 THEN BEGIN 
        nnuv[ii]   = n_elements(list)
        ;
        ; extract what we need from all matches
        dbext, list, 'sname,optid,ra,dec,fluxrad_brt,axerat,mag_brt,flux_brt,entry_othfilt', $
                      name_,optid_,ra_,dec_,radu_,axrat_,magun_,flxun_,entuf_
        dbext, entuf_, 'mag_brt,flux_brt', maguf_,flxuf_
        ;
        ; decide which entry to look at
        if nnuv[ii] eq 1 then begin 
           ;
           ; only one choice, so that's easy
           jj      = 0
        endif else begin 
           ;
           ; find the 'S1' galaxy
           kk      = where(strpos(name_,':S1') ge 0, nkk)
           if nkk gt 0 then jj = kk[0] else jj = 0
        endelse 
        ;
        ; and then put data for the first of these in the arrays
        nameu[ii]  = name_[jj]
        idu[ii]    = optid_[jj]
        entun[ii]  = list[jj]
        rau[ii]    = ra_[jj]
        decu[ii]   = dec_[jj]
        radu[ii]   = radu_[jj]
        axratu[ii] = axrat_[jj]
        magun[ii]  = magun_[jj]
        maguf[ii]  = maguf_[jj]
        flxun[ii]  = flxun_[jj]
        flxuf[ii]  = flxuf_[jj]
     ENDIF 
  ENDFOR
  ;
  ; convert fluxes to magnitude scale
  kk     = where(flxun gt 0.0, nkk)
  if nkk gt 0 then magun0[kk] = mag0st - 2.5*alog10(flxun[kk]) + 5.0*alog10(lamv/lamn)
  kk     = where(flxuf gt 0.0, nkk)
  if nkk gt 0 then maguf0[kk] = mag0st - 2.5*alog10(flxuf[kk]) + 5.0*alog10(lamv/lamf)
  ;
  kk     = where(ido eq '-' and idu ne '-', nkk)
  if nkk gt 0 then ido[kk] = idu[kk]
  ido    = strtrim(ido,2)
  print, max(strlen(ido))
  rastr  = degsexi(rah,/ra,prec=1)
  decstr = degsexi(dech,prec=0)
  ;
  openw,lu,filo1,/get_lun
  printf,-1,hdr1
  printf,-1,hdr2
  printf,-1,hdr3
  printf,lu,hdr1
  printf,lu,hdr2
  printf,lu,hdr3
  for ii = 0, nbig-1 do begin 
     printf,-1,ljust(nameh[ii],11)+ljust(ido[ii],13)+ljust(rastr[ii],12)+ljust(decstr[ii],11),vhel[ii],w50[ii],dist[ii],' |',$
               nopt[ii],rado[ii],axrato[ii],magr[ii],lflxha[ii],' |',$
               nnuv[ii],radu[ii],axratu[ii],magun0[ii],maguf0[ii],format=fmto
     printf,lu,ljust(nameh[ii],11)+ljust(ido[ii],13)+ljust(rastr[ii],12)+ljust(decstr[ii],11),vhel[ii],w50[ii],dist[ii],' |',$
               nopt[ii],rado[ii],axrato[ii],magr[ii],lflxha[ii],' |',$
               nnuv[ii],radu[ii],axratu[ii],magun0[ii],maguf0[ii],format=fmto
  endfor 
  free_lun,lu
  ;
  kk = where(nopt gt 0 or nnuv gt 0, nkk)
  openw,lu,filo2,/get_lun
  printf,-1,hdr1
  printf,-1,hdr2
  printf,-1,hdr3
  printf,lu,hdr1
  printf,lu,hdr2
  printf,lu,hdr3
  for jj = 0, nkk-1 do begin 
     ii = kk[jj]
     printf,-1,ljust(nameh[ii],11)+ljust(ido[ii],13)+ljust(rastr[ii],12)+ljust(decstr[ii],11),vhel[ii],w50[ii],dist[ii],' |',$
               nopt[ii],rado[ii],axrato[ii],magr[ii],lflxha[ii],' |',$
               nnuv[ii],radu[ii],axratu[ii],magun0[ii],maguf0[ii],format=fmto
     printf,lu,ljust(nameh[ii],11)+ljust(ido[ii],13)+ljust(rastr[ii],12)+ljust(decstr[ii],11),vhel[ii],w50[ii],dist[ii],' |',$
               nopt[ii],rado[ii],axrato[ii],magr[ii],lflxha[ii],' |',$
               nnuv[ii],radu[ii],axratu[ii],magun0[ii],maguf0[ii],format=fmto
  endfor 
  free_lun,lu
  ;
  ; go back to starting directory
  cd, cwd
END 
