pro galexmatch_targets
  ;
  ; find HIPASS targets in supersingg_master that have galex 
  ; observations of sufficient length to be useful.  Make 
  ; summary lists for distribution to 
  ;
  ; Tsutomu Takeuchi - all galaxies with SUNGG or SINGG data
  ; Ivy Wong - those in original SUNGG target list as well as 
  ;            serindipitous targets
  ; Zheng Zheng  - those with Halpha for SINGG that are not in
  ;                Ivy's list
  ;
  ; G. Meurer (ICRAR/UWA) 11/2011
  ;
  flog      = 'galexmatch_targets.log'
  logstr    = 'GALEXMATCH_TARGETS: '
  openw, ll, flog, /get_lun
  plog, ll, logstr, 'starting'
  ;
  tfmin     = 100.0                    ; minimum FUV exposure times
  wd        = '~/SUNGG/Sample/'
  filo1     = 'galexmatches_all.dat'
  filo2     = 'galexmatches_iwong.dat'
  filo3     = 'galexmatches_zz.dat'
  filo4     = 'galexmatches_tt.dat'
  filo5     = 'galexmatches_orphans.dat'
  dbm       = 'supersingg_master'
  dbg       = 'galexgr6_seibert_coadd'
  hdro      = '#Sname         RA(str)      Dec(str)    RA(deg)   Dec(deg)   Dist lg(MHI) '+$
              'pr iw zz sungg sr2 Ak_FIS   IRAS glxtile tile_name                         '+$
              't_nuv     t_fuv'
  fmto      = '(a38,f10.4,f10.4,f8.2,f7.2,i3,i3,i3,i5,i5,i7,i7,i7,2x,a31,f10.2,f10.2)'
  ;
  ; go to working directory
  plog, ll, logstr, 'going to working directory: '+wd
  cd, wd, current=cwd
  ;
  ; open database, extract stuff
  plog, ll, logstr, 'opening master database, getting all targets with GALEX data'
  dbopen, dbm
  listg      = dbfind('entgalexb > 1')     
  ng         = n_elements(listg) 
  plog, ll, logstr, 'getting relevant quantities from master database' 
  plog, ll, logstr, 'number of entries with GALEX observations: '+numstr(ng)
  dbext, listg, 'sname,hname,ra,dec,dist,lmhi,entuvsamp,entoptphot,entgalexb,entakfis,entiras', $
                 sname,hname,ra,dec,dist,lmhi,entsungg,entsr2,entglx,entakfis,entiras
  dbclose
  ;
  ; open galex tile database get additional info needed
  ; to find the good matches
  plog, ll, logstr, 'opening GALEX tile database: '+dbg
  dbopen, dbg
  dbext, entglx, 'obsname,texpnuv,texpfuv,qagrade,nflag,fflag',$
         tile,tnuv,tfuv,qagrade,nflag,fflag
  dbclose
  ;
  ; apply tfuv cutoff and repack arrays
  plog, ll, logstr, 'applying FUV exptime cutoff of : '+numstr(tfmin)
  good       = where(tfuv ge tfmin, ngood)
  if ngood le 0 then begin 
     plog, ll, logstr, '**** No good entries found, halting program.'
     stop
  endif 
  ;
  plog, ll, logstr, 'repacking arrays after finding number of good entries = '+numstr(ngood)
  listg    = temporary(listg[good])
  sname    = temporary(sname[good])
  hname    = temporary(hname[good])
  ra       = temporary(ra[good])
  dec      = temporary(dec[good])
  dist     = temporary(dist[good])
  lmhi     = temporary(lmhi[good])
  entsungg = temporary(entsungg[good])
  entsr2   = temporary(entsr2[good])
  entglx   = temporary(entglx[good])
  entakfis = temporary(entakfis[good])
  entiras  = temporary(entiras[good])
  tile     = temporary(tile[good])
  tnuv     = temporary(tnuv[good])
  tfuv     = temporary(tfuv[good])
  qagrade  = temporary(qagrade[good])
  nflag    = temporary(nflag[good])
  fflag    = temporary(fflag[good])
  ;
  ; mark the galaxies that ivy gets
  plog, ll, logstr, 'divvying up the sample '
  ivygal       = make_array(ngood, /byte, value=0b)
  jivy         = where(entsungg ge 1, njivy)
  plog, ll, logstr, 'Number of targets in sungg sample: '+numstr(njivy)
  ivygal[jivy] = 1b
  ;
  kk           = sort(entglx[jivy])
  uu           = uniq(entglx[jivy[kk]])
  entglx_ivy   = entglx[jivy[kk[uu]]]
  ntivy        = n_elements(entglx_ivy)
  plog, ll, logstr, 'number of tiles covered by sungg_sample: '+numstr(ntivy)
  ;
  ; search for extra galaxies in sungg_sample tiles
  plog, ll, logstr, 'searching for extra galaxies in sungg_sample tiles'
  for ii = 0, ntivy-1 do begin 
     kk        = where(entglx eq entglx_ivy[ii], nkk)
     if nkk gt 0 then ivygal[kk] = ivygal[kk] + 1b
  endfor 
  jj           = where(ivygal eq 1b, njj)
  kk           = where(ivygal eq 2b, nkk)
  plog, ll, logstr, 'Number of SUNGG primary target: '+numstr(nkk)
  plog, ll, logstr, 'Number of extra HIPASS targets in SUNGG tiles: '+numstr(njj)
  ;
  ; find targets for Zheng
  plog, ll, logstr, 'Searching for non-SUNGG galaxies with SINGG observations'
  zhenggal       = make_array(ngood, /byte, value=1b)    ; start by taking everything
  jivy           = where(ivygal ge 1b, njivy)            ; these are Ivy's
  zhenggal[jivy] = 0b                                    ; chuck them out
  jj             = where(entsr2 le 0, nnosr2)            ; galaxies not in SR2
  zhenggal[jj]   = 0b                                    ; chuck them out (for now)
  jzz            = where(zhenggal gt 0, njzz)            ; these are the ones for zheng, so far
  if njzz le 0 then stop, 'Nothing for Zheng? :-( '
  ;
  kk            = sort(entglx[jzz])
  uu            = uniq(entglx[jzz[kk]])
  entglx_zz     = entglx[jzz[kk[uu]]]
  ntzz          = n_elements(entglx_zz)
  plog, ll, logstr, 'number of tiles covered by SR2 galaxies not in SUNGG : '+numstr(ntzz)
  ;
  ; search for extra galaxies in SR2 but non-SUNGG tiles
  plog, ll, logstr, 'searching for extra galaxies in SR2 but non-SUNGG tiles'
  for ii = 0, ntzz-1 do begin 
     kk        = where(entglx eq entglx_zz[ii], nkk)
     if nkk gt 0 then zhenggal[kk] = zhenggal[kk] + 1b
  endfor 
  jj           = where(zhenggal eq 1b, njj)
  kk           = where(zhenggal eq 2b, nkk)
  plog, ll, logstr, 'Number of SR2 but non-SUNGG primary target: '+numstr(nkk)
  plog, ll, logstr, 'Number of extra HIPASS targets in these tiles: '+numstr(njj)
  ;
  ; get RA, DEC in sexigessimal
  rastr        = degsexi(ra,/ra,prec=1)
  decstr       = degsexi(dec,prec=0)
  ;
  ; make priority code
  ; 3 - Halpha data
  ; 2 - in sungg_sample
  ; 1 - everything else
  priority     = make_array(ngood,/int,value=1)
  jj           = where(entsungg gt 0, njj)
  if njj gt 0 then priority[jj] = 2
  jj           = where(entsr2 gt 0, njj)
  if njj gt 0 then priority[jj] = 3
  ;
  ; print results
  ; 1. all matches
  plog, ll, logstr, 'writing file for all matches: '+filo1
  openw, lu, filo1, /get_lun
  printf, -1, hdro
  printf, lu, hdro
  plog, ll, logstr, 'will write 1 header line and # records = '+numstr(ngood)
  for ii = 0, ngood-1 do begin
     str       = ljust(sname[ii],15)+ljust(rastr[ii],13)+ljust(decstr[ii],12)
     printf,-1,str,ra[ii],dec[ii],dist[ii],lmhi[ii],$
               priority[ii],ivygal[ii],zhenggal[ii],entsungg[ii],entsr2[ii],$
               entakfis[ii],entiras[ii],entglx[ii],ljust(tile[ii],31),tnuv[ii],tfuv[ii],$
               format=fmto
     printf,lu,str,ra[ii],dec[ii],dist[ii],lmhi[ii],$
               priority[ii],ivygal[ii],zhenggal[ii],entsungg[ii],entsr2[ii],$
               entakfis[ii],entiras[ii],entglx[ii],ljust(tile[ii],31),tnuv[ii],tfuv[ii],$
               format=fmto
  endfor
  free_lun, lu
  ;
  ; 2 galaxies for Ivy
  plog, ll, logstr, 'writing file for SUNGG sample matches and extras: '+filo2
  openw, lu, filo2, /get_lun
  printf, -1, hdro
  printf, lu, hdro
  jj           = where(ivygal gt 0, njj)
  plog, ll, logstr, 'will write 1 header line and # records = '+numstr(njj)
  if njj le 0 then stop, 'No galaxies for Ivy??? :-( '
  for kk = 0, njj-1 do begin
     ii        = jj[kk]
     str       = ljust(sname[ii],15)+ljust(rastr[ii],13)+ljust(decstr[ii],12)
     printf,-1,str,ra[ii],dec[ii],dist[ii],lmhi[ii],$
               priority[ii],ivygal[ii],zhenggal[ii],entsungg[ii],entsr2[ii],$
               entakfis[ii],entiras[ii],entglx[ii],ljust(tile[ii],31),tnuv[ii],tfuv[ii],$
               format=fmto
     printf,lu,str,ra[ii],dec[ii],dist[ii],lmhi[ii],$
               priority[ii],ivygal[ii],zhenggal[ii],entsungg[ii],entsr2[ii],$
               entakfis[ii],entiras[ii],entglx[ii],ljust(tile[ii],31),tnuv[ii],tfuv[ii],$
               format=fmto
  endfor
  free_lun, lu
  ;
  ; 3 galaxies for Zheng
  plog, ll, logstr, 'writing file for SR2 galaxies but not in SUNGG sample, and extras: '+filo3
  openw, lu, filo3, /get_lun
  printf, -1, hdro
  printf, lu, hdro
  jj           = where(zhenggal gt 0, njj)
  plog, ll, logstr, 'will write 1 header line and # records = '+numstr(njj)
  if njj le 0 then stop, 'No galaxies for Zheng??? :-( '
  for kk = 0, njj-1 do begin
     ii        = jj[kk]
     str       = ljust(sname[ii],15)+ljust(rastr[ii],13)+ljust(decstr[ii],12)
     printf,-1,str,ra[ii],dec[ii],dist[ii],lmhi[ii],$
               priority[ii],ivygal[ii],zhenggal[ii],entsungg[ii],entsr2[ii],$
               entakfis[ii],entiras[ii],entglx[ii],ljust(tile[ii],31),tnuv[ii],tfuv[ii],$
               format=fmto
     printf,lu,str,ra[ii],dec[ii],dist[ii],lmhi[ii],$
               priority[ii],ivygal[ii],zhenggal[ii],entsungg[ii],entsr2[ii],$
               entakfis[ii],entiras[ii],entglx[ii],ljust(tile[ii],31),tnuv[ii],tfuv[ii],$
               format=fmto
  endfor
  free_lun, lu
  ;
  ; 4 galaxies for Tsutomu
  plog, ll, logstr,  'writing file for SUNGG or SR2 galaxies with good GALEX tiles (and extras): '+filo4
  openw, lu, filo4, /get_lun
  printf, -1, hdro
  printf, lu, hdro
  jj           = where(priority gt 0, njj)
  plog, ll, logstr, 'will write 1 header line and # records = '+numstr(njj)
  if njj le 0 then stop, 'No galaxies for Tsutomu??? :-( '
  for kk = 0, njj-1 do begin
     ii        = jj[kk]
     str       = ljust(sname[ii],15)+ljust(rastr[ii],13)+ljust(decstr[ii],12)
     printf,-1,str,ra[ii],dec[ii],dist[ii],lmhi[ii],$
               priority[ii],ivygal[ii],zhenggal[ii],entsungg[ii],entsr2[ii],$
               entakfis[ii],entiras[ii],entglx[ii],ljust(tile[ii],31),tnuv[ii],tfuv[ii],$
               format=fmto
     printf,lu,str,ra[ii],dec[ii],dist[ii],lmhi[ii],$
               priority[ii],ivygal[ii],zhenggal[ii],entsungg[ii],entsr2[ii],$
               entakfis[ii],entiras[ii],entglx[ii],ljust(tile[ii],31),tnuv[ii],tfuv[ii],$
               format=fmto
  endfor
  free_lun, lu
  ;
  ; 5 orphan galaxies
  plog, ll, logstr, 'writing file for SINGG sample galaxies not yet observed with good GALEX tiles: '+filo5
  openw, lu, filo5, /get_lun
  printf, -1, hdro
  printf, lu, hdro
  jj           = where(zhenggal eq 0 AND ivygal EQ 0, njj)
  plog, ll, logstr, 'will write 1 header line and # records = '+numstr(njj)
  if njj le 0 then stop, 'No orphan galaxies??? :-( '
  for kk = 0, njj-1 do begin
     ii        = jj[kk]
     str       = ljust(sname[ii],15)+ljust(rastr[ii],13)+ljust(decstr[ii],12)
     printf,-1,str,ra[ii],dec[ii],dist[ii],lmhi[ii],$
               priority[ii],ivygal[ii],zhenggal[ii],entsungg[ii],entsr2[ii],$
               entakfis[ii],entiras[ii],entglx[ii],ljust(tile[ii],31),tnuv[ii],tfuv[ii],$
               format=fmto
     printf,lu,str,ra[ii],dec[ii],dist[ii],lmhi[ii],$
               priority[ii],ivygal[ii],zhenggal[ii],entsungg[ii],entsr2[ii],$
               entakfis[ii],entiras[ii],entglx[ii],ljust(tile[ii],31),tnuv[ii],tfuv[ii],$
               format=fmto
  endfor
  free_lun, lu
  ;
  plog, ll, logstr, 'returning to starting directory: '+cwd
  cd, cwd
  plog, ll, logstr, 'finished.'
  free_lun, ll
end


