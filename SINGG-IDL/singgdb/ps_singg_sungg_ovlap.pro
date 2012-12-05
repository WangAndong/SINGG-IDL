pro ps_singg_sungg_ovlap
  ;
  ;  Find galaxies in ps1 overlap region (dec > -30) that
  ;  have both SINGG and SUNGG data.
  ;
  ; G. Meurer (ICRAR/UWA) 4/2012
  ; G. Meurer (ICRAR/UWA) 5/2012 : 
  ;    - calculate summed total area within maximum radius
  ;
  hdb        = 'singg_sample'
  odb        = 'singg_derived'
  udb        = 'super_sungg_derived'
  mdb        = 'supersingg_master'
  wd         = getenv('SHOME')+'/Sample/'
  lamf       = 1538.6 
  lamn       = 2315.7
  lamv       = 5470.0
  mag0st     = -21.1
  declim0    = -30.0
  declim1    = 2.5
  ;
  filo1      = 'overlap_list_all.dat'
  filo2      = 'overlap_list_deg.dat'
  fmto       = '(a49,f7.3,i4,a2,f8.1,f7.1,f7.1,a2,f8.1,f6.1,f7.2,f8.2,a2,f8.1,f6.1,f7.2,f7.2)'
  fmto2      = '(a25,f12.5,f12.5,f7.3,i4,a2,f8.1,f7.1,f7.1,a2,f8.1,f6.1,f7.2,f8.2,a2,f8.1,f6.1,f7.2,f7.2)'
  hdr        = ['<----------------------- optical ---------------------------> <--- HI properties ---> <--------- SINGG ------------> <---------- SUNGG --------->',$
                'HIPASS+      Optid           RA          Dec      E(B-V)  kin|   V_hel   W_50   Dist |   Radop   a/b  mag_r  lflxha |   Raduv   a/b  mag_n  mag_f',$
                '-------------------------------------------------------------|-----------------------|------------------------------|----------------------------']
  hdr        = ['<----------------------- optical ---------------------------> <--- HI properties ---> <--------- SINGG ------------> <---------- SUNGG --------->',$
                'HIPASS+      Optid           RA          Dec      E(B-V)  kin|   V_hel   W_50   Dist |   Radop   a/b  mag_r  lflxha |   Raduv   a/b  mag_n  mag_f',$
                '-------------------------------------------------------------|-----------------------|------------------------------|----------------------------']
  nhdr       = n_elements(hdr)
  ;
  ; go to working directory
  cd, wd, current=cwd
  ;
  ; start search with uv database
  dbopen, udb
  goodn      = dbfind('FILTER=NUV')
  entun      = dbfind('dec > '+numstr(declim0)+', dec < '+numstr(declim1),goodn)
  ;
  ; extract what we need from all matches
  dbext, entun, 'sname,optid,ra,dec,fluxrad_brt,axerat,mag_brt,flux_brt,entry_othfilt', $
                 nameu,optidu,rau,decu,radu,axratu,magun,flxun,entuf
  dbext, entuf, 'mag_brt,flux_brt', maguf,flxuf
  dbclose
  ;
  ; create arrays to store matches
  ntmp        = n_elements(entun)
  print, 'Number of entries found in '+udb+' = '+numstr(ntmp)
  ento        = make_array(ntmp, /int, value=-1)
  nfndo       = make_array(ntmp, /int, value=-1)
  ;
  ; open optical db, get good entries
  dbopen, odb
  goodo       = good_derived3()
  ;
  ; Now match by SINGG name to optical database
  ; (probably should match by position...)
  for ii = 0, ntmp-1 do begin
     pp       = dbmatch('name',strtrim(nameu[ii],2),goodo)
     ento[ii] = pp[0]
     if pp[0] gt 0 then nfndo[ii] = n_elements(pp) else nfndo[ii] = 0
  endfor
  stop
  ;
  ; keep just the ones with both Halpha and UV, and sort while we 
  ; are at it
  ii          = sort(nameu)
  jj          = where(nfndo[ii] gt 0, ngal)
  kk          = ii[jj]
  nameu       = nameu[kk]
  optidu      = optidu[kk]
  rau         = rau[kk]
  decu        = decu[kk]
  radu        = radu[kk]
  axratu      = axratu[kk]
  magun       = magun[kk]
  flxun       = flxun[kk]
  entuf       = entuf[kk]
  maguf       = maguf[kk]
  flxuf       = flxuf[kk]
  ento        = ento[kk]
  magun0      = make_array(ngal, /float, value=-1.0)
  maguf0      = make_array(ngal, /float, value=-1.0)
  ;
  ; get remaining required optical quantities
  dbext, ento, 'name,object,optid,ra,dec,rmax_f,axerat,mapp_r_t,logf_ha_t,entry_sample', $
                nameo,hname,optido,rao,deco,rado,axrato,magr,lflxha,esamp
  dbclose
  ;
  ; calculate separations to be safe
  gcircd,2,rao,deco,rau,decu,sepou
  ;
  ; open HI database, get relevant quantities
  dbopen, hdb
  dbext, esamp, 'NAME,RA,DEC,VHEL,W50,SP,SINT,DISTANCE,LOGMHI,EBV', $
                nameh,rah,dech,vhel,w50,sp,sint,dist,lmhi,ebv
  dbclose
  ;
  ; convert fluxes to magnitude scale
  kk     = where(flxun gt 0.0, nkk)
  if nkk gt 0 then magun0[kk] = mag0st - 2.5*alog10(flxun[kk]) + 5.0*alog10(lamv/lamn)
  kk     = where(flxuf gt 0.0, nkk)
  if nkk gt 0 then maguf0[kk] = mag0st - 2.5*alog10(flxuf[kk]) + 5.0*alog10(lamv/lamf)
  ;
  ; save optical positions
  rastr  = degsexi(rao,/ra,prec=1)
  decstr = degsexi(deco,prec=0)
  ;
  ; open master database, match by singg name
  dbopen, mdb
  entm   = dbmatch('sname',nameu)
  ;
  ; get kinematics status
  dbext, entm, 'seloptrc,statoptrc',kinsel,kinstat
  dbclose
  ;
  ; make simple 0,1 indication of whether the source has been observed
  ; for optical kinematics
  kinobs = (strlen(strtrim(kinstat,2)) gt 1)
  ;
  ; determine area in each galaxy
  areao = !pi*rado^2/axrato
  areau = !pi*radu^2/axratu
  aream = areao > areau
  jj    = where(areao gt 0, njj)
  if njj gt 0 then tareao = total(areao[jj]) else tareo = -1.0
  jj    = where(areau gt 0, njj)
  if njj gt 0 then tareau = total(areau[jj]) else tareu = -1.0
  jj    = where(aream gt 0, njj)
  if njj gt 0 then taream = total(aream[jj]) else tarem = -1.0
  ;
  ; print results
  ;
  openw,lu,filo1,/get_lun
  for ii = 0, nhdr-1 do begin
     printf,-1,hdr[ii]
     printf,lu,hdr[ii]
  endfor 
  for ii = 0, ngal-1 do begin 
     printf,-1,ljust(nameo[ii],13)+ljust(optido[ii],11)+'  '+ljust(rastr[ii],12)+ljust(decstr[ii],11),ebv[ii],kinobs[ii],' |',vhel[ii],w50[ii],dist[ii],' |',$
               rado[ii],axrato[ii],magr[ii],lflxha[ii],' |',$
               radu[ii],axratu[ii],magun0[ii],maguf0[ii],format=fmto
     printf,lu,ljust(nameo[ii],13)+ljust(optido[ii],11)+'  '+ljust(rastr[ii],12)+ljust(decstr[ii],11),ebv[ii],kinobs[ii],' |',vhel[ii],w50[ii],dist[ii],' |',$
               rado[ii],axrato[ii],magr[ii],lflxha[ii],' |',$
               radu[ii],axratu[ii],magun0[ii],maguf0[ii],format=fmto
  endfor 
  ;
  ; print total area estimates:
  printf, -1, '# '
  printf, -1, '# total optical area = ',tareao,' arcsec^2 '
  printf, -1, '# total uv area      = ',tareau,' arcsec^2 '
  printf, -1, '# total maximum area = ',taream,' arcsec^2 '
  printf, lu, '# '
  printf, lu, '# total optical area = ',tareao,' arcsec^2 '
  printf, lu, '# total uv area      = ',tareau,' arcsec^2 '
  printf, lu, '# total maximum area = ',taream,' arcsec^2 '
  free_lun,lu
  openw,lu,filo2,/get_lun
  for ii = 0, nhdr-1 do begin
     printf,-1,hdr[ii]
     printf,lu,hdr[ii]
  endfor 
  for ii = 0, ngal-1 do begin 
     printf,-1,ljust(nameo[ii],13)+ljust(optido[ii],11)+' ',rao[ii],deco[ii],ebv[ii],kinobs[ii],' |',vhel[ii],w50[ii],dist[ii],' |',$
               rado[ii],axrato[ii],magr[ii],lflxha[ii],' |',$
               radu[ii],axratu[ii],magun0[ii],maguf0[ii],format=fmto2
     printf,lu,ljust(nameo[ii],13)+ljust(optido[ii],11)+' ',rao[ii],deco[ii],ebv[ii],kinobs[ii],' |',vhel[ii],w50[ii],dist[ii],' |',$
               rado[ii],axrato[ii],magr[ii],lflxha[ii],' |',$
               radu[ii],axratu[ii],magun0[ii],maguf0[ii],format=fmto2
  endfor 
  free_lun,lu
  ;
  ; go back to starting directory
  cd, cwd
end 
