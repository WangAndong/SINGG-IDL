PRO ssoup_dbhafuvsb, ll, sname, nfound, sha, esha, sr, esr, hafuv, ehafuv, raw=raw, $
                     odb=odb, udb=udb
  ;
  ; derive quantities used in Halpha/FUV versus surf.bright 
  ; (Halpha and R) from optical and UV databases.
  ;
  ;   ll      -> logical unit of log file
  ;   sname   -> singg source name
  ;   nfound  <- number of sources found (and returned)
  ;   sha     <- log Halpha surface brightness
  ;   esbha   <- error in sha
  ;   sr      <- log r band surface brightness
  ;   esr     <- error in sr
  ;   hafuv   <- log Halpha/FUV
  ;   ehafuv  <- error in hafuv
  ;   raw     -> if set raw quantities are returned,
  ;              otherwise dust corrected values are returned.
  ;   odb     -> if set, the name of the optical database,
  ;              the default is 'singg_derived'
  ;   udb     -> if set, the name of the ultraviolet database,
  ;              the default is 'sungg_derived'
  ;
  ; G. Meurer (ICRAR/UWA) 07/2010
  prog      = 'SSOUP_DBHAFUVSB: '
  mrad      = 0.25             ; matchradius in arcmin
  snlimit   = 2.0              ; H-alpha s/n limit
  sbconv1   = 4.0d0*!pi*(0.206265*3.085678d27)^2      ; (erg/s/kpc^2)/(erg/s/cm^2/arcsec^2)
  sbconv2   = sbconv1*1.0d-7                          ; (W/kpc^2)/(erg/s/cm^2/arcsec^2)
  lsbconv2  = alog10(sbconv2)
  ;
  plog,ll,prog,'--------------------------- starting '+prog+' -------------------------------------'
  IF NOT keyword_set(odb) THEN odb = 'singg_derived'
  IF NOT keyword_set(udb) THEN udb = 'sungg_derived'
  ;
  ; open optical database, find source by singg name
  plog,ll,prog,'opening optical database: '+odb
  dbopen, odb
  IF strupcase(odb) EQ 'SINGG_DERIVED' THEN BEGIN
     list    = good_derived3(snlimit=snlimit, cmdin='err_logf_ha_t > -0.1')
  ENDIF ELSE BEGIN 
     list    = good_derived(exclopt=2)
  ENDELSE 
  ;
  ;  Have to be careful doing optical matching for cases of 
  ;  of multiple snames and multiples within sname
  ns         = n_elements(sname)
  ;
  FOR jj = 0, ns-1 DO BEGIN 
     listo   = dbfind('name = '+sname[jj],list)
     IF jj EQ 0 THEN listp = listo ELSE listp = [listp, listo]
  ENDFOR 
  list       = listp
  pp         = where(list GE 0,count)
  plog,ll,prog,'found '+numstr(count)+' matches in the optical database '
  ;
  ; return if nothing found
  IF count LE 0 THEN BEGIN 
     nfound  = 0 
     dbclose
     plog,ll,prog,'no matches in the optical db, closing it and returning'
     return
  ENDIF 
  list       = list[pp]
  ;
  ; extract quantities from database
  IF NOT keyword_set(raw) THEN BEGIN 
     plog,ll,prog,'extracting dust corrected quantities from optical database'
     dbext,list,'name,ra,dec,logf_ha0_t,err_logf_ha_t,logse_ha0_t,err_logse_ha_t,mu_e_r0_t,err_mu_e_r_t',$
                 snameo,rao,deco,fha,efha,sha,esha,sr,esr
  ENDIF ELSE BEGIN 
     plog,ll,prog,'extracting raw quantities from optical database'
     dbext,list,'name,ra,dec,logf_ha_t,err_logf_ha_t,logse_ha_t,err_logse_ha_t,mu_e_r_t,err_mu_e_r_t',$
                 snameo,rao,deco,fha,efha,sha,esha,sr,esr
  ENDELSE 
  ;
  ; convert optical quantities
  sha         = lsbconv2 + sha
  sr          = 16.433 - 0.4*sr
  esr         = 0.4*esr
  ;
  ; close optical database
  plog,ll,prog,'closing optical database'
  dbclose
  ;
  ; get ready for UV matching
  listu       = make_array(count,/long,value=-1l)
  ;
  plog,ll,prog,'opening uv database: '+udb
  dbopen, udb
  listf       = dbfind('filter = fuv')
  ;
  ; match sources by ra,dec
  plog,ll,prog,'matching sources UV sources by optical ra,dec '
  FOR ii = 0, count-1 DO BEGIN 
     ra   = rao[ii]
     dec  = deco[ii]
     list = dbcircled(ra, dec, mrad, dis, listf)
     nl   = n_elements(list)
     IF nl EQ 1 AND list[0] LT 0 THEN nl = 0
     IF nl GE 1 THEN BEGIN 
        IF nl EQ 1 THEN BEGIN 
           ; store matching entry
           listu[ii] = list[0]
        ENDIF ELSE BEGIN 
           ; find closest match if there are multiple
           jj = sort(dis)
           listu[ii] = list[jj[0]]
        ENDELSE 
      ENDIF 
  ENDFOR 
  ;
  pp      = where(listu GE 0, nfound)
  plog,ll,prog,'found '+numstr(nfound)+' matching UV entries '
  IF nfound EQ 0 THEN BEGIN 
     dbclose
     plog,ll,prog,'no matches in the UV db, closing it and returning'
     return
  ENDIF 
  ;
  ; save optical quantities ofd matches
  snameo   = snameo[pp]
  rao      = rao[pp]
  deco     = deco[pp]
  fha      = fha[pp]
  efha     = efha[pp]
  sha      = sha[pp]
  esha     = esha[pp]
  sr       = sr[pp]
  esr      = esr[pp]
  ;
  ; get FUV quantities
  IF NOT keyword_set(raw) THEN BEGIN 
     plog,ll,prog,'extracting dust corrected quantities from UV database'
     dbext,listu,'sname,flux_corr,flux_rms',snameu,ffuv,effuv
  ENDIF ELSE BEGIN 
     plog,ll,prog,'extracting raw quantities from UV database'
     dbext,listu,'sname,flux_brt,flux_rms',snameu,ffuv,effuv
  ENDELSE 
  hafuv    = fha - alog10(ffuv)
  elffuv   = alog10(1.0+effuv/ffuv)
  ehafuv   = sqrt(elffuv^2 + efha^2)
  ;
  ; report names of matching sources
  plog,ll,prog,'will return the database derived quantities for these optical, uv sources: '
  FOR ii = 0, nfound-1 DO plog,ll,' ',ljust(snameo[ii],15)+ljust(snameu[ii],15)
END 
