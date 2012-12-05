PRO choirs_tables
  ;
  ; Make data tables for the choirs galaxies
  ;
  ; G. Meurer 11/2009
  ;
  sdb     = 'singg_derived'       ; name of Halpha/R derived databse
  udb     = 'sungg_derived'       ; name of UV derived database
  cmin    = 4                     ; minimum number of members for choirs
  tabsys  = 'choirs_sys.dat'      ; table for data on the HIPASS systems
  tab1    = 'choirs_elg_opt.dat'  ; optical data for elgs
  tab2    = 'choirs_elg_uv.dat'   ; uv-optical data for elgs
  tol     = 0.0001
  ;
  ; calculate things
  sstr    = ':S'+strtrim(string(cmin),2)   ; search string to find choir
  ;
  ; open database find choirs sample
  dbopen, 'singg_derived'
  list0   = good_derived3()                  ; all good entries
  list1   = dbfind('name = '+sstr, list0)   ; sources with the right multiplicity
  nsys    = n_elements(list1)               ; number of systems
  print, 'Number of HIPASS choirs systems : ', nsys
  ;
  ; get hipass level data
  dbext, list1, 'runid,object,logmhi,distance,entry_sample,filter_r,filter_n', $
         runid,hname,logmhi,dist,esamp,filtr,filtn
  nelg    = make_array(nsys,/int,value=0)
  ps      = sort(hname)
  ;
  ; cycle through hipasss targets
  FOR ii = 0, nsys-1 DO BEGIN
     jj  = ps[ii]
     ee  = dbfind('entry_sample = '+strtrim(string(esamp[jj]),2), list0)
     nelg[ii] = n_elements(ee)
     IF ii EQ 0 THEN ente = ee $
                ELSE ente = [ente, ee]
  ENDFOR 
  nelgt   = n_elements(ente)
  print, 'Total number of ELGs : ', nelgt, fix(total(nelg))
  ;
  ; write systems level data
  fmts    = '(a9,i4,f8.3,f8.2,i5,a29)'
  hds     = '# HIPASS+ NELG lg(MHI)  Dist  esamp RunID   cont_filt nb_filt'
  openw, lu, tabsys, /get_lun
  printf, -1, hds
  printf, lu, hds
  FOR ii = 0, nsys-1 DO BEGIN 
     jj  = ps[ii]
     str = '  '+ljust(runid[jj],8)+ljust(filtr[jj],10)+ljust(filtn[jj],9)
     printf, -1, ljust(hname[jj],11), nelg[jj], logmhi[jj], dist[jj], esamp[jj], $
                 str, format=fmts
     printf, lu, ljust(hname[jj],11), nelg[jj], logmhi[jj], dist[jj], esamp[jj], $
                 str, format=fmts
  ENDFOR 
  free_lun, lu
  ;
  ; Get ELG level data
  dbext, ente, 'name,optid,ra,dec,axerat,inclination,pa,a_r_gal,a_n_gal,frac_ha0_t,mabs_r0_t,err_mag_r_t', $
         sname,optid,ra,dec,arat,incl,pa,ar,an,fracha,mabsr,emabsr
  dbext, ente, 'rmax_f,radius_f,re_r_t,err_re_r_t,re_ha_t,err_re_ha_t,err_re_ha_t_cont,err_re_ha_t_sky', $
         rmax, radius, rer, erer, reha, ereha, erehac, erehas
  dbext, ente, 'r90_r_t,err_r90_r_t,r90_ha_t,err_r90_ha_t,err_r90_ha_t_cont,err_r90_ha_t_sky', $
         r90r, er90r, r90ha, er90ha, er90hac, er90has
  dbext, ente, 'logf_ha0_t,logl_ha0_t,err_logf_ha_t,err_logf_ha_t_cont,err_logf_ha_t_sky', $
         lfha0, lha0, elha0, elha0c, elha0s
  dbext, ente, 'logse_ha0_t,err_logse_ha_t,err_logse_ha_t_cont,err_logse_ha_t_sky', $
         lseha, elseha, elsehac, elsehas 
  dbext, ente, 'ew50_0_t,err_ew50_0_t,mu_e_r0_t,err_mu_e_r_t', $
         ew50,eew50,muer,emuer
  ;
  ; sort by singg name
  p1     = sort(sname)
  hdr1a  = '# SINGG+      OptID              RA        Dec      a/b    PA     incl  A_R    A_NB  fracHa   Mabs_R  errR  lg(LHa)  errHa    rmax  Rmax   r_90R     err    r_50R    err   r_90Ha      err     r_50Ha    err  lg(SeHa) err   EW50     err '
  hdr1b  = '#                               (deg)      (deg)          (deg)  (deg)  (mag)  (mag)         (ABmag)                           (")  (Kpc)     (")     (")       (")    (")    (")      (")      (")      (")    (cgs)    (cgs)  (AA)    (AA)'
  fmt1   = '(a28,f11.5,f11.5,f6.2,f7.1,f7.1,f7.3,f7.3,f7.3,f9.3,f7.3,f8.3,f7.3,f8.1,f7.2,f9.2,f8.2,f9.2,f8.2,f9.2,f8.2,f9.2,f8.2,f9.3,f9.3,f8.1,f7.1)'
  ;
  ; write out optical data on Choirs ELGs
  openw, lu, tab1, /get_lun
  printf, -1, hdr1a
  printf, -1, hdr1b
  printf, lu, hdr1a
  printf, lu, hdr1b
  FOR ii = 0, nelgt-1 DO BEGIN 
     jj = p1[ii]
     str  = ljust(sname[jj],14)+ljust(optid[jj],12)+'  '
     printf, -1, str, ra[jj], dec[jj], arat[jj], pa[jj], incl[jj], ar[jj], an[jj], fracha[jj], mabsr[jj], emabsr[jj], lha0[jj], elha0[jj], rmax[jj], radius[jj], r90r[jj], er90r[jj], rer[jj], erer[jj], r90ha[jj], er90ha[jj], reha[jj], ereha[jj], lseha[jj], elseha[jj], ew50[jj], eew50[jj], format=fmt1
     printf, lu, str, ra[jj], dec[jj], arat[jj], pa[jj], incl[jj], ar[jj], an[jj], fracha[jj], mabsr[jj], emabsr[jj], lha0[jj], elha0[jj], rmax[jj], radius[jj], r90r[jj], er90r[jj], rer[jj], erer[jj], r90ha[jj], er90ha[jj], reha[jj], ereha[jj], lseha[jj], elseha[jj], ew50[jj], eew50[jj], format=fmt1
  ENDFOR 
  free_lun, lu
  ;
  ; close optical database
  dbclose
  ;
  ; open UV database, get near and far UV lists
  dbopen, udb
  listf = dbfind('filter = fuv')    ; all the far uv entries
  listn = dbfind('filter = nuv')    ; all the near uv entries
  ;
  ; match by entry in singg_derived
  entf  = dbmatch('entry_singg_derivd', ente, listf)   ; fuv matches
  entn  = dbmatch('entry_singg_derivd', ente, listn)   ; nuv matches
  ;
  ; save only cases where either a far or near uv observation is 
  ; available (or both are)
  pu    = where(entf GT 0 OR entn GT 0, npu)
  entf  = entf[pu]
  entn  = entn[pu]
  ;
  ; keep a few optical quantities
  sname  = sname[pu]
  ra     = ra[pu]
  dec    = dec[pu]
  rmax   = rmax[pu]
  lfha0  = lfha0[pu]
  elha0  = elha0[pu]
  arat   = arat[pu]
  ;
  ; get fuv & nuv quantities
  dbext, entf, 'sname,name,ra,dec,axerat,pa,ebvgal,flux_corr,mag_brt,mag_corr,mag_rms', $
         snamef, onamef, raf, decf, aratf, paf, ebvf, fluxf, mf, m0f, emf
  dbext, entf, 'fluxrad_brt,r90_brt,r50_brt,semag,entry_othfilt', $
         rmaxf, r90f, r50f, muef, entnf
  dbext, entn, 'sname,name,ra,dec,axerat,pa,ebvgal,flux_corr,mag_brt,mag_corr,mag_rms', $
         snamen, onamen, ran, decn, aratn, pan, ebvn, fluxn, mn, m0n, emn
  dbext, entn, 'fluxrad_brt,r90_brt,r50_brt,semag,entry_othfilt', $
         rmaxn, r90n, r50n, muen, entfn
  ;
  ; do some sanity checks
  ; a. look for entry_othfilt mismatches
  k1    = where(entf NE entfn, nk1)
  k2    = where(entn NE entnf, nk2)
  print, 'number of fuv entry_othfilt mismatches: ', nk1
  IF nk1 GT 0 THEN BEGIN 
     print, '#entf   entfn'
     forprint, entf, entfn
  ENDIF 
  print, 'number of nuv entry_othfilt mismatches: ', nk2
  IF nk1 GT 0 THEN BEGIN 
     print, '#entn   entnf'
     forprint, entn, entnf
  ENDIF 
  ;
  ; b. get ratios of max radii, area, and axerat, as well as 
  ;    differences in angles
  rat_rmax   = rmaxf / rmaxn
  rat_area   = (rmaxf*rmaxf*aratn)/(rmaxn*rmaxn*aratf)
  rat_arat   = aratf/aratn
  dif_ra     = raf - ran
  dif_dec    = decf - decn
  dif_pa     = paf - pan
  ;
  ; find mismatching cases
  kk         = where(abs(rat_rmax-1.0) GT tol OR abs(rat_area-1.0) GT tol OR abs(rat_arat-1.0) GT tol $
                     OR abs(dif_ra) GT tol OR abs(dif_dec) GT tol OR abs(dif_pa) GT tol, nkk)
  IF nkk GT 0 THEN BEGIN
     print, 'warning fuv,nuv apertures do not match: '
     forprint, sname+'  '+snamef+'  '+snamen+'  ', rat_rmax, rat_area, rat_arat
     forprint, sname+'  '+snamef+'  '+snamen+'  ', dif_ra, dif_dec, dif_pa
  ENDIF 
  ;
  ; calculate multi-wavelength results
  cfn        = mf - mn
  ecfn       = sqrt(emf*emf+emn*emn)
  lhafuv     = lfha0 - alog10(fluxf)
  elhafuv    = sqrt(elha0^2+(0.4*emf)^2)
  arearat    = (rmaxf/rmax)^2*(arat/aratf)
  gcircd, 2, ra, dec, raf, decf, angsep
  angsep     = 3600.0*angsep
  ;
  ; sort
  p2    = sort(sname)
  ;
  ; write out UV results
  hdr2a = '# SINGG+          RA         Dec     EBVgal  a/b     PA   rmax    r90f   r50f   Mf      M0f    errMf  muef     r90n   r50n   Mn      M0b    errMn  muen      f-n    err   lg(Ha/f)   err  ratarea angsep'
  hdr2b = '#                (deg)      (deg)     (mag)        (deg)   (")    (")    (")  (ABmag) (ABmag)  (mag) (ABmag)    (")    (")  (ABmag) (ABmag)  (mag) (ABmag)  (ABmag) (mag)   (lgAA)  (lg)             (")'
  fmt2  = '(a14,f11.5,f11.5,f7.3,f6.2,f7.1,f7.2,f7.2,f7.2,f8.3,f8.3,f7.3,f8.3,f7.2,f7.2,f8.3,f8.3,f7.3,f8.3,f9.3,f7.3,f9.3,f7.3,f7.2,f8.2)'
  openw, lu, tab2, /get_lun
  printf, -1, hdr2a
  printf, -1, hdr2b
  printf, lu, hdr2a
  printf, lu, hdr2b
  FOR ii = 0, npu-1 DO BEGIN 
     jj = p2[ii]
     str = ljust(sname[jj],14)
     printf, -1, str, raf[jj], decf[jj], ebvf[jj], aratf[jj], paf[jj], $
             rmaxf[jj], r90f[jj], r50f[jj], mf[jj], m0f[jj], emf[jj], muef[jj], $
             r90n[jj], r50n[jj], mn[jj], m0n[jj], emn[jj], muen[jj], $
             cfn[jj], ecfn[jj], lhafuv[jj], elhafuv[jj], arearat[jj], $
             angsep[jj], format=fmt2
     printf, lu, str, raf[jj], decf[jj], ebvf[jj], aratf[jj], paf[jj], $
             rmaxf[jj], r90f[jj], r50f[jj], mf[jj], m0f[jj], emf[jj], muef[jj], $
             r90n[jj], r50n[jj], mn[jj], m0n[jj], emn[jj], muen[jj], $
             cfn[jj], ecfn[jj], lhafuv[jj], elhafuv[jj], arearat[jj], $
             angsep[jj], format=fmt2
  ENDFOR 
  free_lun, lu
  ;
  dbclose
END 
