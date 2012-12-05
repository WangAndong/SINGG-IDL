pro mk_ps1_match
   ;
   ; Find galaxies that are in SINGG, have reasonable length UV 
   ; exposures, and are in the PanSTARRS coverage area
   ;
   ; G. Meurer  10/2011
   ;
   wd        = getenv('SHOME')+'Sample'
   dbs       = 'supersingg_master'
   dbg       = 'galex_all201105'
   dbd       = 'singg_derived'
   declim    = -30.0                   ; lowest declination for PanSTARRS1
   filo      = 'singg_ps1.out'
   fmto      = '(a14,f10.4,f10.4,f8.1,f7.2,f7.2,f7.2,f7.1,i7,i7,f7.2,i3,i5,i5,i5,i5,i3,i3,i3,i3,i3)'
   hdro      = '#sname             RA       Dec       Ropt   m_R   mue_R lg_Fha EW(Ha) t(FUV) t(NUV) lg_MHI uv euvs eopt efuv enuv lv s4 sg mg rc'
   ;
   ; go to working directory
   cd, wd, current=cwd
   ;
   ; find singg galaxies in PS1 area
   dbopen, dbs
   list      = dbfind('dec > '+strtrim(string(declim),2)+', entoptphot > 0, uvflag > 1')
   nl        = n_elements(list)
   if nl eq 0 and list[0] eq -1 then nl = 0
   ;
   ; get important quantities from dbs
   dbext, list, 'sname,hname,ra,dec,lmhi,uvflag,s4g,idsigrid,lvhis', $ 
          sname,hname,ra,dec,lmhi,uvflg,s4g,idsigrid,lvhis
   dbext, list, 'entuvsamp,entoptphot,entfuvphot,entnuvphot,entgalexb,mhongoose,statoptrc', $
          entuvsamp,entopt,entf,entn,entglx,mgoose,statoptrc
   dbclose
   ;
   jj        = sort(hname)
   uu        = uniq(hname[jj])
   nuu       = n_elements(uu)
   print, 'Unique number of HIPASS targets: ', nuu
   ;
   ; get sizes, and optical photometry
   dbopen, dbd
   dbext, entopt, 'rmax_f,mapp_r_t,logf_ha_t,ew50_0_t,mu_e_r0_t', $
          rmax,mapr,lfha,ew500,mue0
   dbclose
   ;
   ; get galex exposure times
   dbopen, dbg
   dbext, entglx, 'texpfuv,texpnuv',texpf,texpn
   dbclose
   ;
   ; set some matching flags
   ls4g       = (s4g gt 0)
   lsigrid    = (idsigrid gt 0)
   loptrc     = (strlen(statoptrc) gt 0)
   ;
   ; print out information on the overlap galaxies
   openw, lu, filo, /get_lun
   printf, -1, hdro
   printf, lu, hdro
   for ii = 0, nl-1 do begin 
      printf, -1, ljust(sname[ii],14),ra[ii],dec[ii],rmax[ii],mapr[ii],mue0[ii],lfha[ii],ew500[ii],texpf[ii],texpn[ii],lmhi[ii],$
             uvflg[ii],entuvsamp[ii],entopt[ii],entf[ii],entn[ii],lvhis[ii],ls4g[ii],lsigrid[ii],mgoose[ii],loptrc[ii],format=fmto
      printf, lu, ljust(sname[ii],14),ra[ii],dec[ii],rmax[ii],mapr[ii],mue0[ii],lfha[ii],ew500[ii],texpf[ii],texpn[ii],lmhi[ii],$
             uvflg[ii],entuvsamp[ii],entopt[ii],entf[ii],entn[ii],lvhis[ii],ls4g[ii],lsigrid[ii],mgoose[ii],loptrc[ii],format=fmto
   endfor 
   ;
   ; close and return to initial directory
   free_lun, lu
   cd, cwd
end
