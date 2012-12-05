PRO singg_sungg_overlap, full=full
  dbua = 'sungg_derived_2010'
  dbub = 'sungg_derived_zz'
  dbo  = 'singg_derived'
  dbs  = 'singg_sample'
  ;
  IF keyword_set(full) THEN BEGIN 
     filo = 'singg_sungg_overlap_full.dat'
     fmto = '(a32,f10.5,f11.5,f8.1,f8.1,i5,f7.2,f6.1,'+$
             'f7.1,f7.3,f7.1,f7.1,f7.2,f8.2,f7.1,f7.1,f8.2)'
     hdro = '# HIPASS+       OPTID               RA        Dec       S(HI)   V_hel  W50  lgMHI  Dist'+$
            '  Rap_o  a/b_o   PA_o  R90_r   m_R   mabs_R Rap_uv  R90_f  Mabs_f'
  ENDIF ELSE BEGIN 
     fmto = '(a32,f10.5,f11.5,f8.3,f7.1,f7.1,2x,a3)'
     hdro = '# HIPASS+       OPTID               RA        Dec       m_R    Rap_o Rap_uv  UVdb'
     filo = 'singg_sungg_overlap.dat'
  ENDELSE 
  ;
  ;  Get all the sources in the UV databases
  dbopen, dbua
  lista  = dbfind('filter = fuv')
  dbext, lista, 'name,hipname,sname,ra,dec,fluxrad_brt,r90_brt,mag_corr', $
         namea, hnamea, snamea, raa, deca, rapa, r90a, mabsfa
  na     = n_elements(namea)
  dbclose
  dbopen, dbub
  listb  = dbfind('filter = fuv')
  dbext, listb, 'name,hipname,sname,ra,dec,fluxrad_brt,r90_brt,mag_corr', $
         nameb, hnameb, snameb, rab, decb, rapb, r90b, mabsfb
  nb     = n_elements(nameb)
  dbclose
  ;
  ; append
  nameu  = [namea, nameb]
  hnameu = [hnamea, hnameb]
  snameu = [snamea, snameb]
  rau    = [raa, rab]
  decu   = [deca, decb]
  rapu   = [rapa, rapb]
  r90u   = [r90a, r90b]
  mabsfu = [mabsfa, mabsfb]
  udb    = [make_array(na, /string, value='OIW'), make_array(nb, /string, value='ZJZ')]
  ;
  nu     = n_elements(nameu)
  ento   = make_array(nu, /int, value=-1)  ; to store corresponding singg_derived entries
  ;
  ; match to singg_derived entries
  dbopen, dbo
  FOR ii = 0, nu-1 DO BEGIN 
     mrad   = (0.25*rapu[ii] > 10.0)/60.0             ; match radius 1/4 aperture but at least 10.0 arcsec
     list   = dbcircled(rau[ii], decu[ii], mrad, sep)
     jj     = min(sep)
     IF list[jj] GT 0 THEN ento[ii] = list[jj]
  ENDFOR 
  ;
  ; save only the overlap
  jj     = where(ento GT 0, njj)
  IF njj EQ 0 THEN stop, 'No optical matches :-('
  ;
  nameu  = nameu[jj]
  hnameu = hnameu[jj]
  snameu = snameu[jj]
  rau    = rau[jj]
  decu   = decu[jj]
  rapu   = rapu[jj]
  ento   = ento[jj]
  udb    = udb[jj]
  r90u   = r90u[jj]
  mabsfu = mabsfu[jj]
  ;
  ; now get the optical quantities
  dbext, ento, 'name,optid,ra,dec,axerat,pa,mapp_r_t,mabs_r0_t,rmax_f,r90_r_t,ew50_0_t,entry_sample',$
         snameo,optid,rao,deco,abo,pao,magr,mabsr,rapo,r90o,ewo,entso
  dbclose
  ;
  ; find cases where SINGG ID does not match
  kk     = where(ljust(snameu,15) NE ljust(snameo,15), nkk)
  IF nkk GT 0 THEN BEGIN 
     print, 'cases where SINGG names do not match: '
     forprint,ljust(snameu[kk],15)+ljust(snameo[kk],15)+ljust(nameu[kk],20)+' '+ljust(optid[kk],20)+'  '+udb[jj]
     print, ' '
  ENDIF 
  ;
  ; open sample database and get quantities
  dbopen, dbs
  dbext, entso, 'sint,vhel,w50,logmhi,distance',sint,vhel,w50,lmhi,dist
  dbclose
  ;
  ; print out results
  kk   = sort(snameo)
  openw, lu, filo, /get_lun
  printf, -1, hdro
  printf, lu, hdro
  IF keyword_set(full) THEN BEGIN 
     FOR jj = 0, njj-1 DO BEGIN
        ii = kk[jj]
        printf,-1,ljust(snameo[ii],15)+' '+ljust(optid[ii],15)+' ',rao[ii], deco[ii], sint[ii], vhel[ii], w50[ii], lmhi[ii], dist[ii], $
               rapo[ii], abo[ii], pao[ii], r90o[ii], magr[ii], mabsr[ii], rapu[ii], r90u[ii], mabsfu[ii], format=fmto
        printf,lu,ljust(snameo[ii],15)+' '+ljust(optid[ii],15)+' ',rao[ii], deco[ii], sint[ii], vhel[ii], w50[ii], lmhi[ii], dist[ii], $
               rapo[ii], abo[ii], pao[ii], r90o[ii], magr[ii], mabsr[ii], rapu[ii], r90u[ii], mabsfu[ii], format=fmto
     ENDFOR 
  ENDIF ELSE BEGIN 
     FOR jj = 0, njj-1 DO BEGIN
        ii = kk[jj]
        printf,-1,ljust(snameo[ii],15)+' '+ljust(optid[ii],15)+' ',rao[ii], deco[ii], magr[ii], rapo[ii], rapu[ii], udb[ii], format=fmto
        printf,lu,ljust(snameo[ii],15)+' '+ljust(optid[ii],15)+' ',rao[ii], deco[ii], magr[ii], rapo[ii], rapu[ii], udb[ii], format=fmto
     ENDFOR 
  ENDELSE 
  free_lun, lu
END 
