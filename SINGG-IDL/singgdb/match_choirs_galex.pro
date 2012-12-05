pro match_choirs_galex
   ;
   ; G. Meurer  (ICRAR/UWA) 10/2011
   wd        = getenv('SHOME')+'Choirs'
   fili      = 'choirs_sys_ed.dat' 
   fmti      = '(a,i,f,f,i,a,a,a)'
   filo      = 'choirs_sys_ed.out'
   fmto      = '(a11,i4,f8.3,f8.3,i5,2x,a8,a10,a9,i5,i4,i7,2x,a27,i4,i4,i6,i6)'
   sdb       = 'singg_sample'
   mdb       = 'supersingg_master'
   gdb       = 'galex_all201105'
   noname    = '<null>'
   hdro      = ['#  1 HIPASS : HIPASS name', '#  2 NELG   : number of emission line galaxies in field', $
                '#  3 lg_MHI : system log(Mass in HI/M_sun) ', $
                '#  4 esamp  : entry in singg_sample database', $
                '#  5 runid  : SINGG observing run for best data', $
                '#  6 Cfilt  : filter used for continuum subtraction', $
                '#  7 NBfilt : narrow-band filter used for Halpha', $
                '#  8 s4g_id : ID number for S4G (spitzer) survey', $
                '#  9 uvf    : UVflag - -1: not in galex tile database, 0: AIS length exposure, 1: longer exposure, 2,3: SUNGG galaxy w/o,w FUV data', $
                '# 10 texpn  : total exposure time NUV [s]', $
                '# 11 texpf  : total exposure time FUV [s]', '# ', $
                '# HIPASS   NELG  lg_MHI   Dist esamp  runid   Cfilt     NBfilt  s4g_id uvf entglx  gname                       nvn nvf texpn texpf']
   ;
   ; go to working directory
   cd, wd, current=cwd
   ;
   ; read input file
   readcol, fili, cname, nelg, lmhi, dist, esamp, runid, cfilt, nbfilt, format=fmti
   nc        = n_elements(cname)
   ;
   ; adjust name to that of the S1 galaxy
   s1nam     = strtrim(cname,2)+':S1'
   ;
   ; match to master database
   dbopen, mdb
   list      = dbmatch('sname',s1nam)
   ;
   ; extract relevant quantities
   dbext, list, 'sname,uvflag,s4g,entgalexb',sname,uvflag,s4g,eglxb
   dbclose
   ;
   ; extract quantities from galex tile database
   dbopen, gdb
   dbext, eglxb, 'obsname,nvisitsn,nvisitsf,texpnuv,texpfuv',gname,nvn,nvf,tnuv,tfuv
   ;
   ; give gname = noname (= '<null>') when uvflag = -1
   jj        = where(uvflag lt 0, njj)
   if njj gt 0 then gname[jj] = noname
   ;
   ; print results to screen and file
   openw, lu, filo, /get_lun
   ;
   nh       = n_elements(hdro)
   for ii = 0, nh-1 do begin
      printf,-1,hdro[ii],format='(a)'
      printf,lu,hdro[ii],format='(a)'
   endfor
   for ii = 0, nc-1 do begin 
      printf,-1,ljust(cname[ii],11),nelg[ii],lmhi[ii],dist[ii],esamp[ii],$
                ljust(runid[ii],8),ljust(cfilt[ii],10),ljust(nbfilt[ii],10), $
                s4g[ii],uvflag[ii],eglxb[ii],ljust(gname[ii],27),$
                nvn[ii],nvf[ii],tnuv[ii],tfuv[ii],format=fmto
      printf,lu,ljust(cname[ii],11),nelg[ii],lmhi[ii],dist[ii],esamp[ii],$
                ljust(runid[ii],8),ljust(cfilt[ii],10),ljust(nbfilt[ii],10), $
                s4g[ii],uvflag[ii],eglxb[ii],ljust(gname[ii],27),$
                nvn[ii],nvf[ii],tnuv[ii],tfuv[ii],format=fmto
   endfor 
   free_lun, lu
   cd, cwd
end 
