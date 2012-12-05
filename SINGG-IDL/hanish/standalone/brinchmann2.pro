PRO brinchmann2,PS=ps,BW=bw,HIMF=himf,NUM=num
; Expanded list of plots, generated from the SINGG_DERIVED database.
; OPTIONAL INPUTS:
;   /ps        Postscript output
;   /bw        Black-and-white output
;   himf       Use different HI mass function
;   /num       Include number density in plots

  psflag = KEYWORD_SET(ps)
  bwflag = KEYWORD_SET(bw)
  numflag = KEYWORD_SET(num)
  dummy = FLTARR(3)
  imf = 1.26D41 ; the conversion factor from erg/s to solar masses per year
  cm_Mpc = 3.0857D24; cm per Mpc
  s_yr = 86400.0*365.25
  q0 = 0.20
  G = (1.0/232.47) ; (km/s)^2 pc Msolar^-1

  dbopen,"singg_derived",0
  derind = good_derived2()+1
  dbext,derind,"NAME,OBJECT,LOGMHI,DISTANCE,LOGL_HA0_T,MABS_R0_T,EW50_0_T", $
            NAME,OBJECT,LOGMHI,DISTANCE,LOGL_HA0_T,MABS_R0_T,EW50_0_T
  dbext,derind,"LOGSE_HA0_T,TGAS0_T,MU_E_R0_T,RADIUS_F,AXERAT,RE_R_T,RE_HA_T", $
            LOGSE_HA0_T,TGAS0_T,MU_E_R0_T,RADIUS_F,AXERAT,RE_R_T,RE_HA_T
  dbext,derind,"FLAG_O,LOGL_HA0_O,LOGSE_HA0_O,LOGL_R0_T,R90_R_T,R90_HA_T,A_N_INT,RMAX_F", $
            FLAG_O,LOGL_HA0_O,LOGSE_HA0_O,LOGL_R0_T,R90_R_T,R90_HA_T,A_N_INT,RMAX_F
  dbclose
  object = update_name(object)

; Store these so we can overwrite invalid values later
  logl_ha0 = logl_ha0_t
  logse_ha0 = logse_ha0_t
  re_r = re_r_t
  ind = WHERE(re_r LE 0.0,count)
  IF count GT 0 THEN re_r[ind] = rmax_f[ind]
  r90_r = r90_r_t
  ind = WHERE(r90_r LE 0.0,count)
  IF count GT 0 THEN r90_r[ind] = rmax_f[ind]
  re_ha = re_ha_t
  ind = WHERE(re_ha LE 0.0,count)
  IF count GT 0 THEN re_ha[ind] = rmax_f[ind]
  r90_ha = r90_ha_t
  ind = WHERE(r90_ha LE 0.0,count)
  IF count GT 0 THEN r90_ha[ind] = rmax_f[ind]

  ind = WHERE(STRTRIM(flag_o,2) EQ "T",count)
  IF count GT 0 THEN BEGIN
; If flag_O is set for this galaxy, replace each _T value with the _O value
    logl_ha0[ind] = logl_ha0_o[ind]
    logse_ha0[ind] = logse_ha0_o[ind]
    re_ha[ind] = rmax_f[ind]
  ENDIF

  num_objs = N_ELEMENTS(name)

  dbopen,"singg_sample",0
  dbext,-1,"NAME,    W50", $
            sampname,W50
  dbclose
  sampname = update_name(sampname)

; Dynamical mass is defined by the individual galaxy.
  dyn = BYTARR(num_objs)
  sini = SQRT((1.0 - (1.0/axerat)^2)/(1.0 - q0^2))

  Mdyn = FLTARR(num_objs)
  Mdyn_90 = FLTARR(num_objs)
  Mdyn_3e = FLTARR(num_objs)
  v50 = FLTARR(num_objs)

  FOR ii = 0,num_objs-1 DO BEGIN
    ind = WHERE(STRTRIM(sampname,2) EQ STRTRIM(object[ii],2),count)
    IF count GT 0 THEN BEGIN
      v50[ii] = 0.5 * W50[ind[0]] / MIN([sini[ii],1.0]) ; in km/s
      Mdyn[ii] = ALOG10(v50[ii]^2 * radius_f[ii] * 1E3 / G) ; in solar masses
      Mdyn_90[ii] = ALOG10(v50[ii]^2 * (r90_ha[ii]*!dtor/3600.0*distance[ii]*1E6) / G) ; in solar masses
      Mdyn_3e[ii] = ALOG10(v50[ii]^2 * (3.0*re_ha[ii]*!dtor/3600.0*distance[ii]*1E6) / G) ; in solar masses
    ENDIF
  ENDFOR

  IF NOT KEYWORD_SET(himf) THEN himf = -1
  setup_himf,theta0,refmass,alpha,HIMF=himf

; All we want out of sfrd.dat is the bin sizes.
  datafile = !singgdir+"/sfrd.dat"
  readcol_new,datafile,mmin,mmax,num_in_bin,meansfr,meantgas,meanrho,sigrho, $
                       meanlr,siglr,SKIPLINE=numskip,COMMENT='#', $
                       /SILENT,FORMAT='(F,F,I,F,F,F,F,F,F)'

  scale = DBLARR(num_objs)

  rho_R = 0.d0
  rho_Ha = 0.d0
  frac_R = DBLARR(num_objs)
  frac_Ha = DBLARR(num_objs)
  logtheta = DBLARR(num_objs)
  ngals = INTARR(num_objs)
  nbin = INTARR(num_objs)
  FOR ii=0,num_objs-1 DO BEGIN
    junk = MIN(SQRT(logmhi[ii]-mmin),temp,/NAN)
    nbin[ii] = num_in_bin[temp]
    scale[ii] = (mmax[temp]-mmin[temp])/num_in_bin[temp]
    logtheta[ii] = ALOG10(calc_theta(logmhi[ii],theta0, $
                   refmass,alpha,sigma))
  ENDFOR

;;  logl_r = logl_r0_t ;; Before using this, find the solar conversion
  logl_r = (mabs_r0_t-4.64)/(-2.5) ; in solar units

  frac_N = 10.d0^logtheta*scale
  rho_N = TOTAL(frac_N)
  frac_N = frac_N/rho_N

  frac_R = 10.d0^(logtheta + logl_r)*scale
  rho_R = TOTAL(frac_R)
  frac_R = frac_R/rho_R

  frac_Ha = 10.d0^(logtheta + logl_ha0)*scale
  rho_Ha = TOTAL(frac_Ha)
  frac_Ha = frac_Ha/rho_Ha

;forprint,name,frac_N,frac_R,frac_Ha
;stop

; Some variables (like logMHI) are defined only for HI pointings, so
; multiple-galaxy images need to be handled differently.

  single = WHERE(STRPOS(name,':S') LT 0,scount)
  m1 = WHERE(STRPOS(name,':S1') GE 0,mcount)
  numS = scount + mcount

  PRINT,"R flux density [solar] = ",rho_R
  PRINT,"Ha flux density = ",rho_Ha
  PRINT,"Number density = ",rho_N

  dyn[single] = 1b ; We don't want to use multiple-source images.
;  dyn = dyn AND (sini GE 0.5)
  dyn = dyn AND (axerat GE 1.25)
  dynindex = WHERE(dyn EQ 1b,goodcount)
  dynindex2 = WHERE(dyn[single] EQ 0b, badsingcount)

  IF NOT psflag THEN BEGIN
    set_plot,'X'
    setplotcolors
    setbgfg,!white,!black
    DEVICE, RETAIN=2, DECOMPOSED=0
    WINDOW,XSIZE=1550,YSIZE=1100
    !P.MULTI=[0,2,3,0,0]

    charsz = 3.0
    symsz = 1.5
    thick = 1.0
  ENDIF ELSE BEGIN
; We'll write .eps files
    set_plot,'PS'
    setplotcolors ; PS mode changes you to 256 colors
    xs = 6.5
    ys = 10.0
    xoff = 1.0
    yoff = 3.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
           bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,2,3,0,0]

; these are duplicated inside singg_density_plot
    charsz = 2.0
    symsz = 1.2
    thick = 2.0
  ENDELSE

  ymin = 0.0
  ymax = 3.0
  xmin = 37.0
  xmax = 43.0

  slope_l = LADFIT(logl_r[dynindex],Mdyn[dynindex])
  slope_l90 = LADFIT(logl_r[dynindex],Mdyn_90[dynindex])
  slope_l3e = LADFIT(logl_r[dynindex],Mdyn_3e[dynindex])

; Patch the dynamical values
  FOR ii = 0,num_objs-1 DO BEGIN
    IF NOT dyn[ii] THEN BEGIN
      Mdyn[ii] = slope_l[1]*logl_r[ii]+slope_l[0]
      Mdyn_90[ii] = slope_l90[1]*logl_r[ii]+slope_l90[0]
      Mdyn_3e[ii] = slope_l3e[1]*logl_r[ii]+slope_l3e[0]
    ENDIF
  ENDFOR

  rho_Mdyn = TOTAL(10.d0^(logtheta + Mdyn)*scale)
  rho_Mdyn_90 = TOTAL(10.d0^(logtheta + Mdyn_90)*scale)
  rho_Mdyn_3e = TOTAL(10.d0^(logtheta + Mdyn_3e)*scale)

  PRINT,"Dynamical Mass densities: "
  PRINT,"  r_max: ",rho_Mdyn
  PRINT,"  r_90:  ",rho_Mdyn_90
  PRINT,"  3*r_e: ",rho_Mdyn_3e

  IF numflag THEN frac = [[frac_R],[frac_Ha],[frac_N]] $
             ELSE frac = [[frac_R],[frac_Ha]]

  bpath = !singgdir+"/brinchmann/"
  bstyle = [1,1,1]

; Fraction of luminosities in relation to HI mass
;  xmax = 11.5
;  xmin = 7.0
;  binsize = 0.5
;  singg_density_plot,logMHI,[xmin,xmax],binsize,frac, $
;                     "!6log(M!DHI!N [M!dsolar!n])", $
;                     psflag,bwflag,label="(a)"
;st = SORT(lmhi)
;for ii = 0,N_ELEMENTS(st)-1 DO BEGIN
; print,lmhi[st[ii]],TOTAL(frac_HI[st[0:ii]])
;endfor

; Fraction of luminosities in relation to dynamical mass
;  xmax = 12.5
;  xmin = 8.5
;  binsize = 0.5
;  singg_density_plot,Mdyn,[xmin,xmax],binsize,frac, $
;                     "!6log(M!Ddyn!N [M!dsolar!n])", $
;                     psflag,bwflag,label="(b)"

; Fraction of SFRD due to R-band magnitude
; (apparent is 5-17)
  xmax = -24.0
  xmin = -14.0
  binsize = -1.0
  singg_density_plot,mabs_r0_t,[xmin,xmax],binsize,frac, $
                     "!6M!DR!N [ABmag]", $
                     psflag,bwflag,/NOBIN,nbin=nbin

  readcol_new,bpath+"mstar.dat",xval,num,mass,sfr, $
              FORMAT='(F,F,F,F)',COMMENT='#',/SILENT
  nfrac = num / TOTAL(num)
  mfrac = mass / TOTAL(mass)
  sfrac = sfr / TOTAL(sfr)
  IF numflag THEN bfrac = [[mfrac],[sfrac],[nfrac]] $
             ELSE bfrac = [[mfrac],[sfrac]]

  xpos = (xval*(-2.5) + 4.64) + ALOG10(2.0); convert to solar masses assuming M/L=2

  singg_density_plot,xpos,[xmin,xmax],binsize,bfrac, $
                     "",/NOBIN,/NOSTEP, $
                     psflag,bwflag,/OVER,STYLE=bstyle

; Fraction of SFRD due to b (birthrate parameter)
  xmax = 2.0
  xmin = -3.0
  binsize = 0.5
  b_singg = ew_imf(ew50_0_t,/silent)
  singg_density_plot,ALOG10(b_singg),[xmin,xmax],binsize,frac, $
                     "!6log(SFR/<SFR>)", $
                     psflag,bwflag,/NOBIN

  readcol_new,bpath+"log_b.dat",xval,num,mass,sfr, $
              FORMAT='(F,F,F,F)',COMMENT='#',/SILENT
  nfrac = num / TOTAL(num)
  mfrac = mass / TOTAL(mass)
  sfrac = sfr / TOTAL(sfr)
  IF numflag THEN bfrac = [[mfrac],[sfrac],[nfrac]] $
             ELSE bfrac = [[mfrac],[sfrac]]
  singg_density_plot,xval,[xmin,xmax],binsize,bfrac, $
                     "",/NOBIN,/NOSTEP, $
                     psflag,bwflag,/OVER,STYLE=bstyle


; Fraction of luminosities in relation to A_Ha
  xmax = 3.0
  xmin = 0.0
  binsize = 0.2
  singg_density_plot,a_n_int,[xmin,xmax],binsize,frac, $
                     "A(H!7a!6) [ABmag]",/NOBIN, $
                     psflag,bwflag

  readcol_new,bpath+"TauV.dat",xval,num,mass,sfr, $
              FORMAT='(F,F,F,F)',COMMENT='#',/SILENT
  nfrac = num / TOTAL(num)
  mfrac = mass / TOTAL(mass)
  sfrac = sfr / TOTAL(sfr)
  IF numflag THEN bfrac = [[mfrac],[sfrac],[nfrac]] $
             ELSE bfrac = [[mfrac],[sfrac]]
  singg_density_plot,xval,[xmin,xmax],binsize,bfrac, $
                     "",/NOBIN,/NOSTEP, $
                     psflag,bwflag,/OVER,STYLE=bstyle

; Fraction of luminosities in relation to RE_R
  xmax = 4.5
  xmin = 2.5
  binsize = 0.25
  rdist = re_r*!dtor/3600.0 * (distance*1E6)
  singg_density_plot,ALOG10(rdist),[xmin,xmax],binsize,frac, $
                     "!6log(r!De!N(!8R!6) [pc])",/NOBIN, $
                     psflag,bwflag

  readcol_new,bpath+"R50.dat",xval,num,mass,sfr, $
              FORMAT='(F,F,F,F)',COMMENT='#',/SILENT
  nfrac = num / TOTAL(num)
  mfrac = mass / TOTAL(mass)
  sfrac = sfr / TOTAL(sfr)
  IF numflag THEN bfrac = [[mfrac],[sfrac],[nfrac]] $
             ELSE bfrac = [[mfrac],[sfrac]]
  singg_density_plot,ALOG10(xval)+3.0,[xmin,xmax],binsize,bfrac, $
                     "",/NOBIN,/NOSTEP, $
                     psflag,bwflag,/OVER,STYLE=bstyle

; Fraction of luminosities in relation to r90/r50
  xmax = 5.0
  xmin = 1.5
  binsize = 0.5
  hdist = r90_r/re_r
  singg_density_plot,hdist,[xmin,xmax],binsize,frac, $
                     "r!D90!N(R)/r!De!N(R)",/NOBIN, $
                     psflag,bwflag

  readcol_new,bpath+"concentration.dat",xval,num,mass,sfr, $
              FORMAT='(F,F,F,F)',COMMENT='#',/SILENT
  nfrac = num / TOTAL(num)
  mfrac = mass / TOTAL(mass)
  sfrac = sfr / TOTAL(sfr)
  IF numflag THEN bfrac = [[mfrac],[sfrac],[nfrac]] $
             ELSE bfrac = [[mfrac],[sfrac]]
  singg_density_plot,xval,[xmin,xmax],binsize,bfrac, $
                     "",/NOBIN,/NOSTEP, $
                     psflag,bwflag,/OVER,STYLE=bstyle

; Fraction of luminosities in relation to EW
;  xmax = 3.0
;  xmin = 0.0
;  binsize = 0.25
;  singg_density_plot,ALOG10(ew50_0_t),[xmin,xmax],binsize,frac, $
;                     "!6log(EW(H!7a!6) [!3"+angstsym()+"!6])", $
;                     psflag,bwflag,label="(j)"

; Fraction of luminosities in relation to mu_e_r0

;; mu = mabs + 5.0*ALOG10(distance+5) + 2.5*ALOG10(2.0*!pi*re^2) - a_gal - a_int

  xmax = 10.0
  xmin = 6.5
  binsize = 0.4
  xpos = logl_r - ALOG10(2.0*!pi*(rdist/1E3)^2)

  singg_density_plot,xpos,[xmin,xmax],binsize,frac, $
                     "log(!7l!6!d*!N(!8R!6) [M!Dsolar!N kpc!E-2!N])", $
                     psflag,bwflag,/NOBIN

  readcol_new,bpath+"mustar.dat",xval,num,mass,sfr, $
              FORMAT='(F,F,F,F)',COMMENT='#',/SILENT
  nfrac = num / TOTAL(num)
  mfrac = mass / TOTAL(mass)
  sfrac = sfr / TOTAL(sfr)
  IF numflag THEN bfrac = [[mfrac],[sfrac],[nfrac]] $
             ELSE bfrac = [[mfrac],[sfrac]]
  singg_density_plot,xval,[xmin,xmax],binsize,bfrac, $
                     "",/NOBIN,/NOSTEP, $
                     psflag,bwflag,/OVER,STYLE=bstyle

  IF psflag THEN BEGIN
    psend,!outdir+"/brinchmann.eps",/noprint,/clobber
  ENDIF

  RETURN
END
