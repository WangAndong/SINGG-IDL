PRO make_sfrd_plots,PS=ps,BW=bw,HIMF=himf,SR1=SR1,BRINCHMANN=brinchmann
; Expanded list of plots, generated from the SINGG_DERIVED database.
; OPTIONAL INPUTS:
;   /ps        Postscript output
;   /bw        Black-and-white output
;   himf       Use different HI mass function

  psflag = KEYWORD_SET(ps)
  bwflag = KEYWORD_SET(bw)
  sr1flag = KEYWORD_SET(sr1)
  brflag = KEYWORD_SET(brinchmann)
  dummy = FLTARR(3)
  imf = 1.26D41 ; the conversion factor from erg/s to solar masses per year
  cm_Mpc = 3.0857D24; cm per Mpc
  s_yr = 86400.0*365.25
  q0 = 0.20
  G = (1.0/232.47) ; (km/s)^2 pc Msolar^-1

  IF sr1flag THEN dbopen,"sr1",0 ELSE dbopen,"singg_derived",0
  derind = good_derived2(/phot)+1
;;  IF sr1flag THEN derind = good_derived2()+1 ELSE derind = good_derived(exclopt=1)

  dbext,derind,"NAME,OBJECT,LOGMHI,DISTANCE,LOGL_HA0_T,MABS_R0_T,EW50_0_T,EW50_T", $
                NAME,OBJECT,LOGMHI,DISTANCE,LOGL_HA0_T,MABS_R0_T,EW50_0_T,EW50_T
  dbext,derind,"LOGSE_HA0_T,TGAS0_T,MU_E_R0_T,RADIUS_F,AXERAT,RE_R_T,RE_HA_T", $
                LOGSE_HA0_T,TGAS0_T,MU_E_R0_T,RADIUS_F,AXERAT,RE_R_T,RE_HA_T
  dbext,derind,"FLAG_O,LOGL_HA0_O,LOGSE_HA0_O,LOGL_R0_T,R90_R_T,R90_HA_T,A_N_INT,RMAX_F", $
                FLAG_O,LOGL_HA0_O,LOGSE_HA0_O,LOGL_R0_T,R90_R_T,R90_HA_T,A_N_INT,RMAX_F
  dbext,derind,'RA,DEC,RUNID', $
                DRA,DDEC,RUNID
  dbclose
  object = update_name(object)

  bpath = !singgdir+"/brinchmann/"
  bstyle = [1,1]

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
  sb = (logse_ha0 GT ALOG10(3.9E-16))

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
  t_orb = FLTARR(num_objs) ; in Gyr
  t_orb_90 = FLTARR(num_objs)
  t_orb_3e = FLTARR(num_objs)
  v50 = FLTARR(num_objs)

  FOR ii = 0,num_objs-1 DO BEGIN
    ind = WHERE(STRTRIM(sampname,2) EQ STRTRIM(object[ii],2),count)
    IF count GT 0 THEN BEGIN
      v50[ii] = 0.5 * W50[ind[0]] / MIN([sini[ii],1.0]) ; in km/s
; the r90 and re used WERE the Halpha ones, but we replaced with the R values.
      Mdyn[ii] = ALOG10(v50[ii]^2 * radius_f[ii] * 1E3 / G) ; in solar masses
      Mdyn_90[ii] = ALOG10(v50[ii]^2 * (r90_r[ii]*!dtor/3600.0*distance[ii]*1E6) / G) ; in solar masses
      Mdyn_3e[ii] = ALOG10(v50[ii]^2 * (3.0*re_r[ii]*!dtor/3600.0*distance[ii]*1E6) / G) ; in solar masses
      t_orb[ii] = 2.0 * !pi * (radius_f[ii]/1E17*cm_Mpc) / v50[ii] / s_yr
      t_orb_90[ii] = 2.0 * !pi * (r90_r[ii] * !dtor/3600.0 * distance[ii]/1E14*cm_Mpc) / v50[ii] / s_yr
      t_orb_3e[ii] = 2.0 * !pi * (3.0*re_r[ii] * !dtor/3600.0 * distance[ii]/1E14*cm_Mpc) / v50[ii] / s_yr
    ENDIF
  ENDFOR

  IF NOT KEYWORD_SET(himf) THEN himf = -1
  setup_himf,theta0,refmass,alpha,HIMF=himf

; All we want out of sfrd.dat is the bin sizes.
  datafile = !singgdir+"/sfrd.dat"
  readcol_new,datafile,mmin,mmax,num_in_bin,meansfr,meantgas,meandtgas,meanEW,$
                       meanrho,sigrho,meanlr,siglr,SKIPLINE=numskip, $
                       COMMENT='#',/SILENT,FORMAT='(F,F,I,F,F,F,F,F,F,F,F)'
  num_bins = N_ELEMENTS(mmin)

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

;;ind = SORT(logmhi)
;ind = SORT(frac_R)
;forprint,logmhi[ind],' '+object[ind]+' ',runid[ind],frac_R[ind],frac_Ha[ind]

;cont_R = frac_R / scale
;cont_Ha = frac_Ha / scale
;ind = REVERSE(SORT(cont_R))
;forprint,logmhi[ind],' '+name[ind]+' ',runid[ind],ALOG10(cont_R[ind]),ALOG10(cont_Ha[ind])
;stop

; Some variables (like logMHI) are defined only for HI pointings, so
; multiple-galaxy images need to be handled differently.

  single = WHERE(STRPOS(name,':S') LT 0,scount)
  m1 = WHERE(STRPOS(name,':S1') GE 0,mcount)
  numS = scount + mcount

  PRINT,'SINGLE: ',scount
  PRINT,'MULTIPLE: ',mcount
  PRINT,'TOTAL: ',num_objs

  obj = [object[single],object[m1]]
  lMHI = [logMHI[single],logMHI[m1]]
  ltheta = [logtheta[single],logtheta[m1]]
  sc = [scale[single],scale[m1]]
  ew50 = [ew50_0_t[single],FLTARR(mcount)]
  ew50_un = [ew50_t[single],FLTARR(mcount)]
  tg = [tgas0_t[single],FLTARR(mcount)]
  fr_R = [frac_R[single],DBLARR(mcount)]
  fr_Ha = [frac_Ha[single],DBLARR(mcount)]
  md = [Mdyn[single],FLTARR(mcount)]
  FOR ii = 0,mcount-1 DO BEGIN
    gals = WHERE(STRTRIM(object,2) EQ STRTRIM(object[m1[ii]],2),galcount)
    ew50[scount+ii] = MEAN(ew50_0_t[gals])
    ew50_un[scount+ii] = MEAN(ew50_t[gals])
    tg[scount+ii] = 1.0/TOTAL(1.0/tgas0_t[gals])
    fr_R[scount+ii] = TOTAL(frac_R[gals])
    fr_Ha[scount+ii] = TOTAL(frac_Ha[gals])
  ENDFOR

  frac_HI = 10.d0^(ltheta + lMHI)*sc
  rho_HI = TOTAL(frac_HI)
  frac_HI = frac_HI/rho_HI

  frac = [[frac_R],[frac_Ha]]
  fr = [[fr_R],[fr_Ha]]

  PRINT,"HI mass density = ",rho_HI
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
    !P.MULTI=[0,4,5,0,0]

    charsz = 3.0
    symsz = 1.5
    thick = 1.0
  ENDIF ELSE BEGIN
; We'll write .eps files
    set_plot,'PS'
    setplotcolors ; PS mode changes you to 256 colors
    xs = 10.5
    ys = 4.5
    xoff = 1.2
    yoff = 3.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
           bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,2,1,0,0]

; these are duplicated inside singg_density_plot
    charsz = 1.5
    symsz = 1.2
    thick = 2.0
  ENDELSE
  IF bwflag THEN dynclr = !black ELSE dynclr = !dmagenta

  ymin = 8.0
  ymax = 12.5
  xmin = 7.0
  xmax = 11.0
  title=""
  clip = [xmin,ymin,xmax,ymax]

  PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],COLOR=!black, $
       YTITLE="!6log(M!Ddyn!N [M!dsolar!n])",CHARSIZE=charsz, $
       XTITLE="!6log(M!DHI!N [M!dsolar!n])",XSTYLE=1,YSTYLE=1, $
       TITLE=title,CLIP=clip,THICK=thick,NOCLIP=0, $
       XMARGIN=[6,1.5],YMARGIN=[3,1.1]

  IF goodcount GT 0 THEN BEGIN
    PLOTS,logmhi[dynindex],Mdyn[dynindex],THICK=thick,NOCLIP=0, $
        PSYM=SYM(1),COLOR=!ddgray,CLIP=clip,SYMSIZE=symsz
    PLOTS,logmhi[dynindex],Mdyn[dynindex],THICK=thick,NOCLIP=0, $
        PSYM=SYM(1),COLOR=dynclr,CLIP=clip,SYMSIZE=symsz*0.75
  ENDIF
  IF NOT psflag THEN BEGIN
    IF badsingcount GT 0 THEN PLOTS,logmhi[single[dynindex2]],Mdyn[single[dynindex2]],$
          THICK=thick,NOCLIP=0, $
          PSYM=SYM(6),COLOR=!black,CLIP=clip,SYMSIZE=symsz
    mult = WHERE(STRPOS(name,':S') GE 0,count)
    PLOTS,logmhi[mult],Mdyn[mult],THICK=thick,NOCLIP=0, $
          PSYM=SYM(22),COLOR=!black,CLIP=clip,SYMSIZE=symsz
    PLOTS,logmhi[m1],Mdyn[m1],THICK=thick,NOCLIP=0, $
          PSYM=SYM(23),COLOR=!ddgray,CLIP=clip,SYMSIZE=symsz
  ENDIF
  PLOTS,[0.0,20.0],[0.0,20.0],COLOR=!black,CLIP=clip,NOCLIP=0

  slope_r1 = LADFIT(ALOG10(rmax_f[dynindex]),ALOG10(v50[dynindex]))
  PRINT,"log(rmax) = ",slope_r1[1],"log(v50) + ",slope_r1[0]
  rtemp = r90_r[dynindex]*!dtor/3600.0 * (distance[dynindex]*1E3)
  slope_r2 = LADFIT(ALOG10(rtemp),ALOG10(v50[dynindex]))
  PRINT,"log(r90) = ",slope_r2[1],"log(v50) + ",slope_r2[0]

  delY = MEAN(Mdyn[dynindex]-logmhi[dynindex])
  slope_m = LADFIT(logmhi[dynindex],Mdyn[dynindex])
  sig = STDDEV(Mdyn[dynindex] - (slope_m[1]*logmhi[dynindex]+slope_m[0]))
  PRINT,"Mean of ALOG(Mdyn/Mhi): ",delY,"; log(Mdyn) = ",slope_m[1],"log(Mhi) + ",slope_m[0]," +/- ",sig
  PRINT," Fractions of luminosity density: (R,Ha)"
  PRINT,"  Good: ",TOTAL(frac_R[dynindex]),TOTAL(frac_Ha[dynindex])
  PRINT,"  Inclination: ",TOTAL(frac_R[single[dynindex2]]),TOTAL(frac_Ha[single[dynindex2]])
  PRINT,"  Multiple: ",TOTAL(fr_R[scount:numS-1]),TOTAL(fr_Ha[scount:numS-1])
  PLOTS,[xmin,xmax],[xmin*slope_m[1]+slope_m[0],xmax*slope_m[1]+slope_m[0]], $
        LINESTYLE=2,COLOR=!black,CLIP=clip,NOCLIP=0

  PLOTS,[xmin,xmax],[xmin*slope_m[1]+slope_m[0]+sig, $
                     xmax*slope_m[1]+slope_m[0]+sig], $
        LINESTYLE=1,COLOR=!black,CLIP=clip,NOCLIP=0
  PLOTS,[xmin,xmax],[xmin*slope_m[1]+slope_m[0]-sig, $
                     xmax*slope_m[1]+slope_m[0]-sig], $
        LINESTYLE=1,COLOR=!black,CLIP=clip,NOCLIP=0
  XYOUTS,(0.1*(xmax-xmin)+xmin),(0.88*(ymax-ymin)+ymin), $
         STRTRIM('(a)',2),CHARSIZE=charsz,COLOR=!black, $
         ALIGNMENT=0.5,CHARTHICK=thick

  xmin = 6.0
  xmax = 11.5
  title=""
  clip = [xmin,ymin,xmax,ymax]

  PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],COLOR=!black, $
       YTITLE="!6log(M!Ddyn!N [M!dsolar!n])",CHARSIZE=charsz, $
       XTITLE="!6log(L!DR!N [L!dsolar!n])",XSTYLE=1,YSTYLE=1, $
       TITLE=title,CLIP=clip,THICK=thick,NOCLIP=0, $
       XMARGIN=[6.1,1.5],YMARGIN=[3,1.1]

  IF goodcount GT 0 THEN BEGIN
    PLOTS,logl_r[dynindex],Mdyn[dynindex],THICK=thick,NOCLIP=0, $
        PSYM=SYM(1),COLOR=!ddgray,CLIP=clip,SYMSIZE=symsz
    PLOTS,logl_r[dynindex],Mdyn[dynindex],THICK=thick,NOCLIP=0, $
        PSYM=SYM(1),COLOR=dynclr,CLIP=clip,SYMSIZE=symsz*0.75
  ENDIF
  IF NOT psflag THEN BEGIN
    IF badsingcount GT 0 THEN PLOTS,logl_r[single[dynindex2]],Mdyn[single[dynindex2]],$
          THICK=thick,NOCLIP=0, $
          PSYM=SYM(6),COLOR=!black,CLIP=clip,SYMSIZE=symsz
    mult = WHERE(STRPOS(name,':S') GE 0,count)
    PLOTS,logl_r[mult],Mdyn[mult],THICK=thick,NOCLIP=0, $
          PSYM=SYM(22),COLOR=!black,CLIP=clip,SYMSIZE=symsz
    PLOTS,logl_r[m1],Mdyn[m1],THICK=thick,NOCLIP=0, $
          PSYM=SYM(23),COLOR=!ddgray,CLIP=clip,SYMSIZE=symsz
  ENDIF
  PLOTS,[0.0,20.0],[0.3,20.3],COLOR=!black,CLIP=clip,NOCLIP=0 ; M/L = 2
;  PLOTS,[0.0,20.0],[0.0,20.0],COLOR=!black,CLIP=clip,NOCLIP=0 ; M/L = 1

  delY = MEAN(Mdyn[dynindex]-logl_r[dynindex])
  slope_l = LADFIT(logl_r[dynindex],Mdyn[dynindex])
  sig = STDDEV(Mdyn[dynindex] - (slope_l[1]*logl_r[dynindex]+slope_l[0]))
  PRINT,"Mean of ALOG(Mdyn/L_r): ",delY,"; log(Mdyn) = ",slope_l[1],"log(L_r) + ",slope_l[0]," +/- ",sig
  PLOTS,[xmin,xmax],[xmin*slope_l[1]+slope_l[0],xmax*slope_l[1]+slope_l[0]], $
        LINESTYLE=2,COLOR=!black,CLIP=clip,NOCLIP=0
  PLOTS,[xmin,xmax],[xmin*slope_l[1]+slope_l[0]+sig, $
                     xmax*slope_l[1]+slope_l[0]+sig], $
        LINESTYLE=1,COLOR=!black,CLIP=clip,NOCLIP=0
  PLOTS,[xmin,xmax],[xmin*slope_l[1]+slope_l[0]-sig, $
                     xmax*slope_l[1]+slope_l[0]-sig], $
        LINESTYLE=1,COLOR=!black,CLIP=clip,NOCLIP=0
  XYOUTS,(0.1*(xmax-xmin)+xmin),(0.88*(ymax-ymin)+ymin), $
         STRTRIM('(b)',2),CHARSIZE=charsz,COLOR=!black, $
         ALIGNMENT=0.5,CHARTHICK=thick

  slope_l90 = LADFIT(logl_r[dynindex],Mdyn_90[dynindex])
  slope_l3e = LADFIT(logl_r[dynindex],Mdyn_3e[dynindex])

  slope_t = LADFIT(logl_r[dynindex],ALOG10(t_orb[dynindex]))
  tsig = STDDEV(ALOG10(t_orb[dynindex]) - (slope_t[1]*logl_r[dynindex]+slope_t[0]))
  slope_t90 = LADFIT(logl_r[dynindex],ALOG10(t_orb_90[dynindex]))
  tsig90 = STDDEV(ALOG10(t_orb_90[dynindex]) - (slope_t90[1]*logl_r[dynindex]+slope_t90[0]))
  slope_t3e = LADFIT(logl_r[dynindex],ALOG10(t_orb_3e[dynindex]))
  tsig3e = STDDEV(ALOG10(t_orb_3e[dynindex]) - (slope_t3e[1]*logl_r[dynindex]+slope_t3e[0]))

  IF psflag THEN BEGIN
    psend,!outdir+"/massscatter.eps",/noprint,/clobber
    set_plot,'PS'
    xs = 10.5
    ys = 4.5
    xoff = 1.2
    yoff = 3.0
;xs = 4.5
;ys = 7.0
;;!P.MULTI=[0,1,2,0,0]
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
             bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,2,1,0,0]
  ENDIF

; Now, redo the Mdyn plots with Mdyn_90 and Mdyn_3e
  ymin = 8.0
  ymax = 12.0
  xmin = 7.0
  xmax = 11.0
  title=""
  clip = [xmin,ymin,xmax,ymax]

  PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],COLOR=!black, $
       YTITLE="!6log(M!Ddyn!N(r!D90!N) [M!dsolar!n])",CHARSIZE=charsz, $
       XTITLE="!6log(M!DHI!N [M!dsolar!n])",XSTYLE=1,YSTYLE=1, $
       TITLE=title,CLIP=clip,THICK=thick,NOCLIP=0, $
       XMARGIN=[6,1.5],YMARGIN=[3,1.1]

  IF goodcount GT 0 THEN BEGIN
    PLOTS,logmhi[dynindex],Mdyn_90[dynindex],THICK=thick,NOCLIP=0, $
        PSYM=SYM(1),COLOR=!ddgray,CLIP=clip,SYMSIZE=symsz
    PLOTS,logmhi[dynindex],Mdyn_90[dynindex],THICK=thick,NOCLIP=0, $
        PSYM=SYM(1),COLOR=dynclr,CLIP=clip,SYMSIZE=symsz*0.75
  ENDIF
  IF NOT psflag THEN BEGIN
    IF badsingcount GT 0 THEN PLOTS,logmhi[single[dynindex2]],Mdyn_90[single[dynindex2]],$
          THICK=thick,NOCLIP=0, $
          PSYM=SYM(6),COLOR=!black,CLIP=clip,SYMSIZE=symsz
    mult = WHERE(STRPOS(name,':S') GE 0,count)
    PLOTS,logmhi[mult],Mdyn_90[mult],THICK=thick,NOCLIP=0, $
          PSYM=SYM(22),COLOR=!black,CLIP=clip,SYMSIZE=symsz
    PLOTS,logmhi[m1],Mdyn_90[m1],THICK=thick,NOCLIP=0, $
          PSYM=SYM(23),COLOR=!ddgray,CLIP=clip,SYMSIZE=symsz
  ENDIF
  PLOTS,[0.0,20.0],[0.0,20.0],COLOR=!black,CLIP=clip,NOCLIP=0

  delY = MEAN(Mdyn_90[dynindex]-logmhi[dynindex])
  slope_m = LADFIT(logmhi[dynindex],Mdyn_90[dynindex])
  sig = STDDEV(Mdyn_90[dynindex] - (slope_m[1]*logmhi[dynindex]+slope_m[0]))
  PRINT,"Mean of ALOG(Mdyn_90/Mhi): ",delY,"; log(Mdyn_90) = ",slope_m[1],"log(Mhi) + ",slope_m[0]," +/- ",sig
  PLOTS,[xmin,xmax],[xmin*slope_m[1]+slope_m[0],xmax*slope_m[1]+slope_m[0]], $
        LINESTYLE=2,COLOR=!black,CLIP=clip,NOCLIP=0

  PLOTS,[xmin,xmax],[xmin*slope_m[1]+slope_m[0]+sig, $
                     xmax*slope_m[1]+slope_m[0]+sig], $
        LINESTYLE=1,COLOR=!black,CLIP=clip,NOCLIP=0
  PLOTS,[xmin,xmax],[xmin*slope_m[1]+slope_m[0]-sig, $
                     xmax*slope_m[1]+slope_m[0]-sig], $
        LINESTYLE=1,COLOR=!black,CLIP=clip,NOCLIP=0
;  XYOUTS,(0.1*(xmax-xmin)+xmin),(0.88*(ymax-ymin)+ymin), $
;         STRTRIM('(a)',2),CHARSIZE=charsz,COLOR=!black, $
;         ALIGNMENT=0.5,CHARTHICK=thick

  xmin = 7.0
  xmax = 11.5
  title=""
  clip = [xmin,ymin,xmax,ymax]

  PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],COLOR=!black, $
       YTITLE="!6log(M!Ddyn!N(r!D90!N) [M!dsolar!n])",CHARSIZE=charsz, $
       XTITLE="!6log(L!DR!N [L!dsolar!n])",XSTYLE=1,YSTYLE=1, $
       TITLE=title,CLIP=clip,THICK=thick,NOCLIP=0, $
       XMARGIN=[6.1,1.5],YMARGIN=[3,1.1]

  IF goodcount GT 0 THEN BEGIN
    PLOTS,logl_r[dynindex],Mdyn_90[dynindex],THICK=thick,NOCLIP=0, $
        PSYM=SYM(1),COLOR=!ddgray,CLIP=clip,SYMSIZE=symsz
    PLOTS,logl_r[dynindex],Mdyn_90[dynindex],THICK=thick,NOCLIP=0, $
        PSYM=SYM(1),COLOR=dynclr,CLIP=clip,SYMSIZE=symsz*0.75
  ENDIF
  IF NOT psflag THEN BEGIN
    IF badsingcount GT 0 THEN PLOTS,logl_r[single[dynindex2]],Mdyn_90[single[dynindex2]],$
          THICK=thick,NOCLIP=0, $
          PSYM=SYM(6),COLOR=!black,CLIP=clip,SYMSIZE=symsz
    mult = WHERE(STRPOS(name,':S') GE 0,count)
    PLOTS,logl_r[mult],Mdyn_90[mult],THICK=thick,NOCLIP=0, $
          PSYM=SYM(22),COLOR=!black,CLIP=clip,SYMSIZE=symsz
    PLOTS,logl_r[m1],Mdyn_90[m1],THICK=thick,NOCLIP=0, $
          PSYM=SYM(23),COLOR=!ddgray,CLIP=clip,SYMSIZE=symsz
  ENDIF
  PLOTS,[0.0,20.0],[0.3,20.3],COLOR=!black,CLIP=clip,NOCLIP=0 ; M/L = 2
;  PLOTS,[0.0,20.0],[0.0,20.0],COLOR=!black,CLIP=clip,NOCLIP=0 ; M/L = 1

  delY = MEAN(Mdyn_90[dynindex]-logl_r[dynindex])
  sig = STDDEV(Mdyn_90[dynindex] - (slope_l90[1]*logl_r[dynindex]+slope_l90[0]))
  PRINT,"Mean of ALOG(Mdyn_90/L_r): ",delY,"; log(Mdyn_90) = ",slope_l90[1],"log(L_r) + ",slope_l90[0]," +/- ",sig
  PLOTS,[xmin,xmax],[xmin*slope_l90[1]+slope_l90[0],xmax*slope_l90[1]+slope_l90[0]], $
        LINESTYLE=2,COLOR=!black,CLIP=clip,NOCLIP=0
  PLOTS,[xmin,xmax],[xmin*slope_l90[1]+slope_l90[0]+sig, $
                     xmax*slope_l90[1]+slope_l90[0]+sig], $
        LINESTYLE=1,COLOR=!black,CLIP=clip,NOCLIP=0
  PLOTS,[xmin,xmax],[xmin*slope_l90[1]+slope_l90[0]-sig, $
                     xmax*slope_l90[1]+slope_l90[0]-sig], $
        LINESTYLE=1,COLOR=!black,CLIP=clip,NOCLIP=0
;  XYOUTS,(0.1*(xmax-xmin)+xmin),(0.88*(ymax-ymin)+ymin), $
;         STRTRIM('(b)',2),CHARSIZE=charsz,COLOR=!black, $
;         ALIGNMENT=0.5,CHARTHICK=thick

  IF psflag THEN BEGIN
    psend,!outdir+"/massscatter90.eps",/noprint,/clobber
; We've plotted the four scatter plots, now do the 10 growth plots.
    set_plot,'PS'
    xs = 10.5
    ys = 4.5
    xoff = 1.2
    yoff = 3.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
             bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,2,1,0,0]
  ENDIF

; Now, Mdyn_3e
  ymin = 8.0
  ymax = 12.5
  xmin = 7.0
  xmax = 11.0
  title=""
  clip = [xmin,ymin,xmax,ymax]

  PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],COLOR=!black, $
       YTITLE="!6log(3.0*M!Ddyn!N(r!De!N) [M!dsolar!n])",CHARSIZE=charsz, $
       XTITLE="!6log(M!DHI!N [M!dsolar!n])",XSTYLE=1,YSTYLE=1, $
       TITLE=title,CLIP=clip,THICK=thick,NOCLIP=0, $
       XMARGIN=[6,1.5],YMARGIN=[3,1.1]

  IF goodcount GT 0 THEN BEGIN
    PLOTS,logmhi[dynindex],Mdyn_3e[dynindex],THICK=thick,NOCLIP=0, $
        PSYM=SYM(1),COLOR=!ddgray,CLIP=clip,SYMSIZE=symsz
    PLOTS,logmhi[dynindex],Mdyn_3e[dynindex],THICK=thick,NOCLIP=0, $
        PSYM=SYM(1),COLOR=dynclr,CLIP=clip,SYMSIZE=symsz*0.75
  ENDIF
  IF NOT psflag THEN BEGIN
    IF badsingcount GT 0 THEN PLOTS,logmhi[single[dynindex2]],Mdyn_3e[single[dynindex2]],$
          THICK=thick,NOCLIP=0, $
          PSYM=SYM(6),COLOR=!black,CLIP=clip,SYMSIZE=symsz
    mult = WHERE(STRPOS(name,':S') GE 0,count)
    PLOTS,logmhi[mult],Mdyn_3e[mult],THICK=thick,NOCLIP=0, $
          PSYM=SYM(22),COLOR=!black,CLIP=clip,SYMSIZE=symsz
    PLOTS,logmhi[m1],Mdyn_3e[m1],THICK=thick,NOCLIP=0, $
          PSYM=SYM(23),COLOR=!ddgray,CLIP=clip,SYMSIZE=symsz
  ENDIF
  PLOTS,[0.0,20.0],[0.0,20.0],COLOR=!black,CLIP=clip,NOCLIP=0

  delY = MEAN(Mdyn_3e[dynindex]-logmhi[dynindex])
  slope_m = LADFIT(logmhi[dynindex],Mdyn_3e[dynindex])
  sig = STDDEV(Mdyn_3e[dynindex] - (slope_m[1]*logmhi[dynindex]+slope_m[0]))
  PRINT,"Mean of ALOG(Mdyn_3e/Mhi): ",delY,"; log(Mdyn_3e) = ",slope_m[1],"log(Mhi) + ",slope_m[0]," +/- ",sig
  PLOTS,[xmin,xmax],[xmin*slope_m[1]+slope_m[0],xmax*slope_m[1]+slope_m[0]], $
        LINESTYLE=2,COLOR=!black,CLIP=clip,NOCLIP=0

  PLOTS,[xmin,xmax],[xmin*slope_m[1]+slope_m[0]+sig, $
                     xmax*slope_m[1]+slope_m[0]+sig], $
        LINESTYLE=1,COLOR=!black,CLIP=clip,NOCLIP=0
  PLOTS,[xmin,xmax],[xmin*slope_m[1]+slope_m[0]-sig, $
                     xmax*slope_m[1]+slope_m[0]-sig], $
        LINESTYLE=1,COLOR=!black,CLIP=clip,NOCLIP=0
;  XYOUTS,(0.1*(xmax-xmin)+xmin),(0.88*(ymax-ymin)+ymin), $
;         STRTRIM('(a)',2),CHARSIZE=charsz,COLOR=!black, $
;         ALIGNMENT=0.5,CHARTHICK=thick

  xmin = 6.0
  xmax = 11.5
  title=""
  clip = [xmin,ymin,xmax,ymax]

  PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],COLOR=!black, $
       YTITLE="!6log(3.0*M!Ddyn!N(r!De!N) [M!dsolar!n])",CHARSIZE=charsz, $
       XTITLE="!6log(L!DR!N [L!dsolar!n])",XSTYLE=1,YSTYLE=1, $
       TITLE=title,CLIP=clip,THICK=thick,NOCLIP=0, $
       XMARGIN=[6.1,1.5],YMARGIN=[3,1.1]

  IF goodcount GT 0 THEN BEGIN
    PLOTS,logl_r[dynindex],Mdyn_3e[dynindex],THICK=thick,NOCLIP=0, $
        PSYM=SYM(1),COLOR=!ddgray,CLIP=clip,SYMSIZE=symsz
    PLOTS,logl_r[dynindex],Mdyn_3e[dynindex],THICK=thick,NOCLIP=0, $
        PSYM=SYM(1),COLOR=dynclr,CLIP=clip,SYMSIZE=symsz*0.75
  ENDIF
  IF NOT psflag THEN BEGIN
    IF badsingcount GT 0 THEN PLOTS,logl_r[single[dynindex2]],Mdyn_3e[single[dynindex2]],$
          THICK=thick,NOCLIP=0, $
          PSYM=SYM(6),COLOR=!black,CLIP=clip,SYMSIZE=symsz
    mult = WHERE(STRPOS(name,':S') GE 0,count)
    PLOTS,logl_r[mult],Mdyn_3e[mult],THICK=thick,NOCLIP=0, $
          PSYM=SYM(22),COLOR=!black,CLIP=clip,SYMSIZE=symsz
    PLOTS,logl_r[m1],Mdyn_3e[m1],THICK=thick,NOCLIP=0, $
          PSYM=SYM(23),COLOR=!ddgray,CLIP=clip,SYMSIZE=symsz
  ENDIF
  PLOTS,[0.0,20.0],[0.3,20.3],COLOR=!black,CLIP=clip,NOCLIP=0 ; M/L = 2
;  PLOTS,[0.0,20.0],[0.0,20.0],COLOR=!black,CLIP=clip,NOCLIP=0 ; M/L = 1

  delY = MEAN(Mdyn_3e[dynindex]-logl_r[dynindex])
  sig = STDDEV(Mdyn_3e[dynindex] - (slope_l3e[1]*logl_r[dynindex]+slope_l3e[0]))
  PRINT,"Mean of ALOG(Mdyn_3e/L_r): ",delY,"; log(Mdyn_3e) = ",slope_l3e[1],"log(L_r) + ",slope_l3e[0]," +/- ",sig
  PLOTS,[xmin,xmax],[xmin*slope_l3e[1]+slope_l3e[0],xmax*slope_l3e[1]+slope_l3e[0]], $
        LINESTYLE=2,COLOR=!black,CLIP=clip,NOCLIP=0
  PLOTS,[xmin,xmax],[xmin*slope_l3e[1]+slope_l3e[0]+sig, $
                     xmax*slope_l3e[1]+slope_l3e[0]+sig], $
        LINESTYLE=1,COLOR=!black,CLIP=clip,NOCLIP=0
  PLOTS,[xmin,xmax],[xmin*slope_l3e[1]+slope_l3e[0]-sig, $
                     xmax*slope_l3e[1]+slope_l3e[0]-sig], $
        LINESTYLE=1,COLOR=!black,CLIP=clip,NOCLIP=0
;  XYOUTS,(0.1*(xmax-xmin)+xmin),(0.88*(ymax-ymin)+ymin), $
;         STRTRIM('(b)',2),CHARSIZE=charsz,COLOR=!black, $
;         ALIGNMENT=0.5,CHARTHICK=thick

  IF psflag THEN BEGIN
    psend,!outdir+"/massscatter3e.eps",/noprint,/clobber
    set_plot,'PS'
    xs = 8.0
    ys = 11.0
    xoff = 1.0
    yoff = 3.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
             bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,2,3,0,0]
    charsz = 3.0
   ENDIF

; Patch the dynamical values
  FOR ii = 0,num_objs-1 DO BEGIN
    IF NOT dyn[ii] THEN BEGIN
;;PRINT,"old",Mdyn[ii],Mdyn_90[ii]
      Mdyn[ii] = slope_l[1]*logl_r[ii]+slope_l[0]
      Mdyn_90[ii] = slope_l90[1]*logl_r[ii]+slope_l90[0]
;;PRINT,"new",Mdyn[ii],Mdyn_90[ii],logl_r[ii]
      Mdyn_3e[ii] = slope_l3e[1]*logl_r[ii]+slope_l3e[0]
      t_orb[ii] = 10.0^(slope_t[1]*logl_r[ii]+slope_t[0])
      t_orb_90[ii] = 10.0^(slope_t90[1]*logl_r[ii]+slope_t90[0])
      t_orb_3e[ii] = 10.0^(slope_t3e[1]*logl_r[ii]+slope_t3e[0])
    ENDIF
  ENDFOR

  rho_Mdyn = TOTAL(10.d0^(logtheta + Mdyn)*scale)
  rho_Mdyn_90 = TOTAL(10.d0^(logtheta + Mdyn_90)*scale)
  rho_Mdyn_3e = TOTAL(10.d0^(logtheta + Mdyn_3e)*scale)

  PRINT,"Dynamical Mass densities: "
  PRINT,"  r_max: ",rho_Mdyn
  PRINT,"  r_90:  ",rho_Mdyn_90
  PRINT,"  3*r_e: ",rho_Mdyn_3e

; Start the Hanish Plots!

; First page: the six plots that use Brinchmann data.

; Fraction of SFRD due to R-band magnitude
; (apparent is 5-17)
  xmax = -25.0
  xmin = -14.0
  Rbinsize = -1.0
  singg_density_plot,mabs_r0_t,[xmin,xmax],Rbinsize,frac, $
                     "!6M!DR!N [ABmag]", $
                     psflag,bwflag,label="(a)",charsz=charsz
  IF brflag THEN BEGIN
    readcol_new,bpath+"mstar.dat",xval,num,mass,sfr, $
                FORMAT='(F,F,F,F)',COMMENT='#',/SILENT
    mfrac = mass / TOTAL(mass)
    sfrac = sfr / TOTAL(sfr)
    xpos = (xval*(-2.5) + 4.64) + ALOG10(2.0); convert to solar masses assuming M/L=2
    singg_density_plot,xpos,[xmin,xmax],Rbinsize,[[mfrac],[sfrac]], $
                       "",/NOBIN,/NOSTEP, $
                       psflag,bwflag,/OVER,STYLE=bstyle
  ENDIF

; Fraction of luminosities in relation to EW
  xmax = 3.0
  xmin = -0.5
  ewbinsize = 0.25
  singg_density_plot,ALOG10(ew50_0_t),[xmin,xmax],ewbinsize,frac, $
                     "!6log(EW(H!7a!6) [!3"+angstsym()+"!6])", $
                     psflag,bwflag,label="(b)",charsz=charsz
  IF brflag THEN BEGIN
    readcol_new,bpath+"log_b.dat",xval,num,mass,sfr, $
                FORMAT='(F,F,F,F)',COMMENT='#',/SILENT
    xval2 = ALOG10(ew_imf(10.0^xval,/SILENT,/REVERSE))
    mfrac = mass / TOTAL(mass)
    sfrac = sfr / TOTAL(sfr)
    bfrac = [[mfrac],[sfrac]]
    singg_density_plot,xval2,[xmin,xmax],ewbinsize,bfrac, $
                       "",/NOBIN,/NOSTEP, $
                       psflag,bwflag,/OVER,STYLE=bstyle
  ENDIF

; Fraction of luminosities in relation to RE_R
  xmax = 4.5
  xmin = 2.0
  binsize = 0.25
  rdist = re_r*!dtor/3600.0 * (distance*1E6)
  singg_density_plot,ALOG10(rdist),[xmin,xmax],binsize,frac, $
                     "!6log(r!De!N(!8R!6) [pc])", $
                     psflag,bwflag,label="(c)",charsz=charsz
  IF brflag THEN BEGIN
    readcol_new,bpath+"R50.dat",xval,num,mass,sfr, $
                FORMAT='(F,F,F,F)',COMMENT='#',/SILENT
    mfrac = mass / TOTAL(mass)
    sfrac = sfr / TOTAL(sfr)
    bfrac = [[mfrac],[sfrac]]
    singg_density_plot,ALOG10(xval)+3.0,[xmin,xmax],binsize,bfrac, $
                       "",/NOBIN,/NOSTEP, $
                       psflag,bwflag,/OVER,STYLE=bstyle
  ENDIF

; Fraction of luminosities in relation to r90/r50
  xmax = 4.0
  xmin = 1.0
  binsize = 0.25
  hdist = r90_r/re_r
  singg_density_plot,hdist,[xmin,xmax],binsize,frac, $
                     "r!D90!N(R)/r!De!N(R)", $
                     psflag,bwflag,label="(d)",charsz=charsz
  IF brflag THEN BEGIN
    readcol_new,bpath+"concentration.dat",xval,num,mass,sfr, $
                FORMAT='(F,F,F,F)',COMMENT='#',/SILENT
    mfrac = mass / TOTAL(mass)
    sfrac = sfr / TOTAL(sfr)
    bfrac = [[mfrac],[sfrac]]
    singg_density_plot,xval,[xmin,xmax],binsize,bfrac, $
                       "",/NOBIN,/NOSTEP, $
                       psflag,bwflag,/OVER,STYLE=bstyle
  ENDIF
  
; Fraction of luminosities in relation to A_Ha
  xmax = 3.0
  xmin = 0.0
  binsize = 0.2
  singg_density_plot,a_n_int,[xmin,xmax],binsize,frac, $
                     "A(H!7a!6) [ABmag]", $
                     psflag,bwflag,label="(e)",charsz=charsz
  IF brflag THEN BEGIN
    readcol_new,bpath+"TauV.dat",xval,num,mass,sfr, $
                FORMAT='(F,F,F,F)',COMMENT='#',/SILENT
    mfrac = mass / TOTAL(mass)
    sfrac = sfr / TOTAL(sfr)
    bfrac = [[mfrac],[sfrac]]
    singg_density_plot,xval,[xmin,xmax],binsize,bfrac, $
                       "",/NOBIN,/NOSTEP, $
                       psflag,bwflag,/OVER,STYLE=bstyle
  ENDIF

; Fraction of luminosities in relation to mu_e_r0
  xmax = 17.0
  xmin = 25.0
  mubinsize = -0.5
  singg_density_plot,mu_e_r0_t,[xmin,xmax],mubinsize,frac, $
                     "!7l!6!de!N(!8R!6) [ABmag arcsec!E-2!N]", $
                     psflag,bwflag,label="(f)",charsz=charsz
  IF brflag THEN BEGIN
    readcol_new,bpath+"mustar.dat",xval,num,mass,sfr, $
                FORMAT='(F,F,F,F)',COMMENT='#',/SILENT
; Convert:
    xval2 = -2.5*(xval) + 4.64 - 5.0*alog10(!dtor/3600.0)+10
    mfrac = mass / TOTAL(mass)
    sfrac = sfr / TOTAL(sfr)
    bfrac = [[mfrac],[sfrac]]
    singg_density_plot,xval2,[xmin,xmax],mubinsize,bfrac, $
                       "",/NOBIN,/NOSTEP, $
                       psflag,bwflag,/OVER,STYLE=bstyle
  ENDIF

  IF psflag THEN BEGIN
    psend,!outdir+"/hanishplot1.eps",/noprint,/clobber
    set_plot,'PS'
    xs = 8.5
    ys = 8.0
    xoff = 1.0
    yoff = 3.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
             bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,2,2,0,0]
    charsz = 1.5
  ENDIF

; Next up: 4 plots that basically deal with SFR or tgas

; Fraction of luminosities in relation to HI mass
  xmax = 11.5
  xmin = 7.0
  massbinsize = 0.5
  singg_density_plot,logMHI,[xmin,xmax],massbinsize,frac, $
                     "!6log(M!DHI!N [M!dsolar!n])", $
                     psflag,bwflag,label="(a)",charsz=charsz

; Fraction of SFRD due to SFR (Halpha luminosity density)
  xmax = 2.0
  xmin = -3.0
  Habinsize = 0.5
  sfr = logl_ha0 - ALOG10(imf)
; Was "!6log(L!DH!7a!6!N [erg s!E-1!N])"
  singg_density_plot,sfr,[xmin,xmax],Habinsize,frac, $
                     "!6log(SFR [M!Dsolar!N yr!E-1!N])", $
                     psflag,bwflag,label="(b)",charsz=charsz

; Fraction of luminosities in relation to logse_ha0
  xmax = 1.0
  xmin = -4.0
  sebinsize = 0.25
  sfrdens = logse_ha0 - ALOG10(imf) + ALOG10(4.0 * !pi * (206265.)^2) + 2.0*ALOG10(cm_Mpc*1E-3)
;!dtor/3600.0 * (distance*1E3))
;"!6log(S!De!N(H!7a!6) [erg cm!E-2!N s!E-1!N arcsec!E-2!N])"
  singg_density_plot,sfrdens,[xmin,xmax],sebinsize,frac, $
                     "!6log(!7R!6!De!N(H!7a!6) [M!Dsolar!N yr!E-1!N kpc!E-2!N])", $
                     psflag,bwflag,label="(c)",charsz=charsz

; Fraction of luminosities in relation to tgas
  xmax = 11.25
  xmin = 7.75
  tgbinsize = 0.25
  singg_density_plot,ALOG10(tg)+9.0,[xmin,xmax],tgbinsize,fr, $
                     "!6log(t!Dgas!N [yr])", $
                     psflag,bwflag,label="(d)",charsz=charsz

  IF psflag THEN BEGIN
    psend,!outdir+"/hanishplot2.eps",/noprint,/clobber
    set_plot,'PS'
    xs = 8.5
    ys = 8.0
    xoff = 1.0
    yoff = 3.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
             bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,2,2,0,0]
  ENDIF

; Then, 4 more plots, mainly dealing with r90 stuff.

; Fraction of luminosities in relation to r_90/r_max
  xmax = 1.00
  xmin = 0.30
  binsize=0.05
  hplot = r90_r/rmax_f
  singg_density_plot,hplot,[xmin,xmax],binsize,frac, $
                     "!6r!D90!N(R) / r!Dmax!N", $
                     psflag,bwflag,label="(a)",charsz=charsz

; Fraction of luminosities in relation to t_orb_90
;  xmax = 1.0
;  xmin = 0.0
;  tobinsize=0.1
  xmax = 9.2
  xmin = 8.0
  tobinsize=0.1
  singg_density_plot,ALOG10(t_orb_90)+9.0,[xmin,xmax],tobinsize,frac, $
                     "!6log(t!Dorb!N(r!D90!N) [yr])", $
                     psflag,bwflag,label="(b)",charsz=charsz

; Fraction of luminosities in relation to r90 dynamical mass
  xmax = 12.5
  xmin = 7.5
  massbinsize = 0.5
  singg_density_plot,Mdyn_90,[xmin,xmax],massbinsize,frac, $
                     "!6log(M!Ddyn!N(r!D90!N) [M!Dsolar!N])", $
                     psflag,bwflag,label="(c)",charsz=charsz

; Fraction of luminosities in relation to (RE_Ha/RE_R)
  xmax = 0.8
  xmin = -0.8
  binsize = 0.1
  hdist = (re_ha/re_r)
  singg_density_plot,ALOG10(hdist),[xmin,xmax],binsize,frac, $
                     "!6log(r!De!N(H!7a!6)/r!De!N(!8R!6))", $
                     psflag,bwflag,label="(d)",charsz=charsz,xtick=0.4

  IF psflag THEN BEGIN
    psend,!outdir+"/hanishplot3.eps",/noprint,/clobber
    set_plot,'PS'
    xs = 11.0
    ys = 7.5
    xoff = 1.0
    yoff = 3.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
             bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,4,3,0,0]
  ENDIF
; We've filled up the first page of plots, so let the user look at them.
  IF NOT psflag THEN key = GET_KBRD(1)

; And then, the rest of the plots, the ones we don't intend to use.

; Fraction of luminosities in relation to dynamical mass
  xmax = 12.5
  xmin = 8.5
  massbinsize = 0.5
  singg_density_plot,Mdyn,[xmin,xmax],massbinsize,frac, $
                     "!6log(M!Ddyn!N [M!dsolar!n])", $
                     psflag,bwflag

; Fraction of luminosities in relation to RE_Ha
  xmax = 4.75
  xmin = 2.0
  binsize = 0.25
  hdist = re_ha*!dtor/3600.0 * (distance*1E6)
  singg_density_plot,ALOG10(hdist),[xmin,xmax],binsize,frac, $
                     "!6log(r!De!N(H!7a!6) [pc])", $
                     psflag,bwflag

; Fraction of luminosities in relation to t_orb
  xmax = 2.0
  xmin = 0.0
  tobinsize = 0.1
  singg_density_plot,t_orb,[xmin,xmax],tobinsize,frac, $
                     "!6t!Dorb!N(r!Dmax!N) [Gyr]", $
                     psflag,bwflag

; Fraction of luminosities in relation to t_orb_3e
  xmax = 2.0
  xmin = 0.0
  tobinsize=0.1
  singg_density_plot,t_orb_3e,[xmin,xmax],tobinsize,frac, $
                     "!6t!Dorb!N(3r!De!N) [Gyr]", $
                     psflag,bwflag

; Fraction of luminosities in relation to r_50/r_max
  xmax = 0.6
  xmin = 0.0
  binsize=0.05
  hplot = re_r/rmax_f
  singg_density_plot,hplot,[xmin,xmax],binsize,frac, $
                     "!6r!De!N(R) / r!Dmax!N", $
                     psflag,bwflag

; Fraction of luminosities in relation to r90(Ha)/r90(R)
  xmax = 0.5
  xmin = -0.9
  binsize = 0.1
  hplot = ALOG10(r90_ha/r90_r)
  singg_density_plot,hplot,[xmin,xmax],binsize,frac, $
                     "log(!6r!D90!N(H!7a!6) / r!D90!N(R))", $
                     psflag,bwflag

;################################
if psflag then charsz = 2.0

  PRINT,"t_orb = 10.0^(",slope_t[1],"*logl_r+",slope_t[0]," +/- ",tsig,")"
  PRINT,"t_orb_90 = 10.0^(",slope_t90[1],"*logl_r+",slope_t90[0]," +/- ",tsig90,")"
  PRINT,"t_orb_3e = 10.0^(",slope_t3e[1],"*logl_r+",slope_t3e[0]," +/- ",tsig3e,")"

  rr = r90_r*!dtor/3600.0 * (distance*1E3)
  rha = r90_ha*!dtor/3600.0 * (distance*1E3)
  rmax = rmax_f*!dtor/3600.0 * (distance*1E3)
  r3e = 3.0*re_r*!dtor/3600.0 * (distance*1E3)

; linear
  xmin = 1.0
  xmax = 60.0
  ymin = 0.0
  ymax = 1.05
  clip = [xmin,ymin,xmax,ymax]
  PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],COLOR=!black, $
       YTITLE="!6r!D90!N / r!Dmax!N",CHARSIZE=charsz, $
       XTITLE="!6r!Dmax!N [kpc]",XSTYLE=1,YSTYLE=1, $
       TITLE=title,CLIP=clip,THICK=thick,NOCLIP=0, $
       XMARGIN=[6,1.5],YMARGIN=[3,1.1]

  PLOTS,rmax,rr/rmax,THICK=thick,NOCLIP=0, $
        PSYM=SYM(6),COLOR=!blue,CLIP=clip,SYMSIZE=symsz
  PLOTS,rmax,rha/rmax,THICK=thick,NOCLIP=0, $
        PSYM=SYM(6),COLOR=!red,CLIP=clip,SYMSIZE=symsz
  PLOTS,[xmin,xmax],[1.0,1.0],COLOR=!black,CLIP=clip,NOCLIP=0

  slope_r = LADFIT(rmax,rr/rmax)
  slope_ha = LADFIT(rmax,rha/rmax)
  sigr = STDDEV(rr/rmax - (slope_r[1]*rmax+slope_r[0]))
  sigha = STDDEV(rha/rmax - (slope_ha[1]*rmax+slope_ha[0]))
  PRINT,"R:  r90/rmax = ",slope_r[1],"*rmax + (",slope_r[0]," +/- ",sigr,")",FORMAT='(A,F6.3,A,F6.3,A,F5.3,A)'
  PRINT,"Ha: r90/rmax = ",slope_ha[1],"*rmax + (",slope_ha[0]," +/- ",sigha,")",FORMAT='(A,F6.3,A,F6.3,A,F5.3,A)'

  PLOTS,[xmin,xmax],[xmin*slope_r[1]+slope_r[0],xmax*slope_r[1]+slope_r[0]], $
        LINESTYLE=2,COLOR=!blue,CLIP=clip,NOCLIP=0
  PLOTS,[xmin,xmax],[xmin*slope_r[1]+slope_r[0]+sigr, $
                     xmax*slope_r[1]+slope_r[0]+sigr], $
        LINESTYLE=1,COLOR=!blue,CLIP=clip,NOCLIP=0
  PLOTS,[xmin,xmax],[xmin*slope_r[1]+slope_r[0]-sigr, $
                     xmax*slope_r[1]+slope_r[0]-sigr], $
        LINESTYLE=1,COLOR=!blue,CLIP=clip,NOCLIP=0

  PLOTS,[xmin,xmax],[xmin*slope_ha[1]+slope_ha[0],xmax*slope_ha[1]+slope_ha[0]], $
        LINESTYLE=2,COLOR=!red,CLIP=clip,NOCLIP=0
  PLOTS,[xmin,xmax],[xmin*slope_ha[1]+slope_ha[0]+sigha, $
                     xmax*slope_ha[1]+slope_ha[0]+sigha], $
        LINESTYLE=1,COLOR=!red,CLIP=clip,NOCLIP=0
  PLOTS,[xmin,xmax],[xmin*slope_ha[1]+slope_ha[0]-sigha, $
                     xmax*slope_ha[1]+slope_ha[0]-sigha], $
        LINESTYLE=1,COLOR=!red,CLIP=clip,NOCLIP=0

; log
  rr = ALOG10(rr)+3.0
  rha = ALOG10(rha)+3.0
  rmax = ALOG10(rmax)+3.0
  r3e = ALOG10(r3e)+3.0

  xmin = 3.0
  xmax = 5.0
  ymin = xmin
  ymax = xmax
  clip = [xmin,ymin,xmax,ymax]
  PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],COLOR=!black, $
       YTITLE="!6log(r!D90!N) [pc]",CHARSIZE=charsz, $
       XTITLE="!6log(r!Dmax!N [pc])",XSTYLE=1,YSTYLE=1, $
       TITLE=title,CLIP=clip,THICK=thick,NOCLIP=0, $
       XMARGIN=[6,1.5],YMARGIN=[3,1.1]

  PLOTS,rmax,rr,THICK=thick,NOCLIP=0, $
        PSYM=SYM(6),COLOR=!blue,CLIP=clip,SYMSIZE=symsz
  PLOTS,rmax,rha,THICK=thick,NOCLIP=0, $
        PSYM=SYM(6),COLOR=!red,CLIP=clip,SYMSIZE=symsz
  PLOTS,[xmin,xmax],[xmin,xmax],COLOR=!black,CLIP=clip,NOCLIP=0

  slope_r = LADFIT(rmax,rr)
  slope_ha = LADFIT(rmax,rha)
  sigr = STDDEV(rr - (slope_r[1]*rmax+slope_r[0]))
  sigha = STDDEV(rha - (slope_ha[1]*rmax+slope_ha[0]))
  PRINT,"R:  log(r90) = ",slope_r[1],"*log(rmax) + (",slope_r[0]," +/- ",sigr,")",FORMAT='(A,F5.3,A,F6.3,A,F5.3,A)'
  PRINT,"Ha: log(r90) = ",slope_ha[1],"*log(rmax) + (",slope_ha[0]," +/- ",sigha,")",FORMAT='(A,F5.3,A,F6.3,A,F5.3,A)'

  PLOTS,[xmin,xmax],[xmin*slope_r[1]+slope_r[0],xmax*slope_r[1]+slope_r[0]], $
        LINESTYLE=2,COLOR=!blue,CLIP=clip,NOCLIP=0
  PLOTS,[xmin,xmax],[xmin*slope_r[1]+slope_r[0]+sigr, $
                     xmax*slope_r[1]+slope_r[0]+sigr], $
        LINESTYLE=1,COLOR=!blue,CLIP=clip,NOCLIP=0
  PLOTS,[xmin,xmax],[xmin*slope_r[1]+slope_r[0]-sigr, $
                     xmax*slope_r[1]+slope_r[0]-sigr], $
        LINESTYLE=1,COLOR=!blue,CLIP=clip,NOCLIP=0

  PLOTS,[xmin,xmax],[xmin*slope_ha[1]+slope_ha[0],xmax*slope_ha[1]+slope_ha[0]], $
        LINESTYLE=2,COLOR=!red,CLIP=clip,NOCLIP=0
  PLOTS,[xmin,xmax],[xmin*slope_ha[1]+slope_ha[0]+sigha, $
                     xmax*slope_ha[1]+slope_ha[0]+sigha], $
        LINESTYLE=1,COLOR=!red,CLIP=clip,NOCLIP=0
  PLOTS,[xmin,xmax],[xmin*slope_ha[1]+slope_ha[0]-sigha, $
                     xmax*slope_ha[1]+slope_ha[0]-sigha], $
        LINESTYLE=1,COLOR=!red,CLIP=clip,NOCLIP=0

; MHI vs L_R
  ymin = 37.0
  ymax = 43.0
  xmin = 36.0
  xmax = 42.0
  title=""
  clip = [xmin,ymin,xmax,ymax]
  xval = logl_r0_t[single]
  yval = logl_ha0[single]

  singg_scatter_plot,xval,yval,clip, $
          "!6log(L!DR!N [erg s!E-1!N !3"+angstsym()+"!6!E-1!N])","!6log(L!DH!7a!6!N [erg s!E-1!N])", $
          charsz,symsz, $
          XMARGIN=[6.1,1.5],YMARGIN=[3,1.1],TITLE=title,THICK=thick

; Mdyn_90 vs re(Ha)/re(R)
  xmin = 7.5
  xmax = 12.0
  ymin = -0.7
  ymax = 0.6
  title=""
  clip = [xmin,ymin,xmax,ymax]
  xval = mdyn_90[dynindex]
  yval = ALOG10(re_ha[dynindex]/re_r[dynindex])

  singg_scatter_plot,xval,yval,clip,"!6log(M!Ddyn!N(r!D90!N) [M!Dsolar!N])", $
                     "!6log(r!De!N(H!7a!6)/r!De!N(!8R!6))",charsz,symsz, $
                     XMARGIN=[7,1.5],YMARGIN=[3,1.1],TITLE=title,THICK=thick

; L_Ha vs EW
  xmin = 37.0
  xmax = 43.0
  ymin = 0.0
  ymax = 3.0
  title=""
  clip = [xmin,ymin,xmax,ymax]
  xval = logl_ha0
  yval = ALOG10(ew50_0_t)

  singg_scatter_plot,xval,yval,clip,"!6log(L!DH!7a!6!N [erg s!E-1!N])", $
                     "!6log(EW(H!7a!6) [!3"+angstsym()+"!6])",charsz,symsz,SB=sb, $
                     XMARGIN=[7,1.5],YMARGIN=[3,1.1],TITLE=title,THICK=thick

; M_R vs EW
  xmin = -12.0
  xmax = -24.0
  ymin = 0.0
  ymax = 3.0
  title=""
  clip = [xmin,ymin,xmax,ymax]
  xval = mabs_r0_t
  yval = ALOG10(ew50_0_t)

  singg_scatter_plot,xval,yval,clip,"!6M!DR!N [ABmag]", $
                     "!6log(EW(H!7a!6) [!3"+angstsym()+"!6])",charsz,symsz,SB=sb, $
                     XMARGIN=[7,1.5],YMARGIN=[3,1.1],TITLE=title,THICK=thick

; tgas vs EW
  xmin = 8.5
  xmax = 11.25
  ymin = 0.0
  ymax = 3.0
  title=""
  clip = [xmin,ymin,xmax,ymax]
  xval = ALOG10(tg)+9.0
  yval = ALOG10(ew50)

  singg_scatter_plot,xval,yval,clip,"!6log(t!Dgas!N [yr])", $
                     "!6log(EW(H!7a!6) [!3"+angstsym()+"!6])",charsz,symsz,SB=sb, $
                     XMARGIN=[7,1.5],YMARGIN=[3,1.1],TITLE=title,THICK=thick

  IF psflag THEN BEGIN
    psend,!outdir+"/junkplots.eps",/noprint,/clobber
    set_plot,'PS'
    xs = 5.5
    ys = 4.0
    xoff = 1.2
    yoff = 3.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
             bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,0,1,0,0]
    charsz = 1.5
    symsz = 0.8
    thick = 2.0
  ENDIF

  xmin = MIN(mmin)
  xmax = MAX(mmax)
  ymin = -0.5
  ymax = 3.0

  PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],COLOR=!black, $
           XTITLE="!6log(M!DHI!N [M!Dsolar!N])", $
           YTITLE="!6log(EW(H!7a!6) [!3"+angstsym()+"!6])",CHARSIZE=charsz,XSTYLE=1, $
           XMARGIN=[8,2],YMARGIN=[3,2], $
           TITLE="",CLIP=[xmin,ymin,xmax,ymax],THICK=thick,YSTYLE=1
  PLOTS,lMHI,ALOG10(ew50_un),PSYM=SYM(1),COLOR=!dmagenta,SYMSIZE=symsz,THICK=thick
  PLOTS,lMHI,ALOG10(ew50),PSYM=SYM(1),COLOR=!dgreen,SYMSIZE=symsz,THICK=thick

print,MIN(ALOG10(ew50)),MIN(ALOG10(ew50_un)),MAX(ALOG10(ew50)),MAX(ALOG10(ew50_un))

  avemass = (mmax + mmin)/2.0
  meanew = FLTARR(num_bins)
  meanew_un = FLTARR(num_bins)
  FOR ii = 0,num_bins-1 DO BEGIN
    ind = WHERE(lMHI GE mmin[ii] AND lMHI LT mmax[ii],count)
    avemass[ii] = MEAN(lMHI[ind])
    meanew[ii] = MEAN(ALOG10(ew50[ind]))
    meanew_un[ii] = MEAN(ALOG10(ew50_un[ind]))
    IF count GT 1 THEN BEGIN
      val1 = STDDEV(ALOG10(ew50))*[1.0,1.0]
      val2 = STDDEV(ALOG10(ew50_un))*[1.0,1.0]
      ERRPLOT,avemass[ii],meanew_un[ii]-val2[1], $
                          meanew_un[ii]+val2[0],COLOR=!magenta,THICK=thick
      ERRPLOT,avemass[ii],meanew[ii]-val1[1], $
                          meanew[ii]+val1[0],COLOR=!dgreen,THICK=thick
    ENDIF ELSE BEGIN
      val1 = 0.0
      val2 = 0.0
    ENDELSE
    PLOTS,[mmin[ii],mmin[ii]],[ymin,ymax],COLOR=!black,THICK=thick,LINESTYLE=1
  ENDFOR
  OPLOT,avemass,meanew_un,PSYM=SYM(4),COLOR=!ddgray, $
        LINESTYLE=1,CLIP=[xmin,ymin,xmax,ymax],SYMSIZE=symsz*2.5,THICK=thick
  OPLOT,avemass,meanew,PSYM=SYM(4),COLOR=!ddgray, $
        LINESTYLE=1,CLIP=[xmin,ymin,xmax,ymax],SYMSIZE=symsz*2.5,THICK=thick
  OPLOT,avemass,meanew_un,PSYM=-SYM(4),COLOR=!magenta, $
        LINESTYLE=1,CLIP=[xmin,ymin,xmax,ymax],SYMSIZE=symsz*1.5,THICK=thick
  OPLOT,avemass,meanew,PSYM=-SYM(4),COLOR=!green, $
        LINESTYLE=1,CLIP=[xmin,ymin,xmax,ymax],SYMSIZE=symsz*1.5,THICK=thick

  sbval = ALOG10(50.0)
  PLOTS,[xmin,xmax],[sbval,sbval],COLOR=!black,LINESTYLE=2,THICK=thick

  IF psflag THEN BEGIN
    psend,!outdir+'ewbin.eps',/noprint,/clobber
    set_plot,'PS'
    xs = 6.5
    ys = 8.0
    xoff = 0.0
    yoff = 0.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
             bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,0,2,0,0]
    charsz = 2.0
    symsz = 1.2
    thick = 2.0
  ENDIF

; Next up, the aitoff projection sample plots.
  sampsym = 1
  sampsz = 0.75
  samp_clr = !black
  dersym = 1
  dersz = 1.0
  der_clr = !dmagenta
  gal_clr = !dblue

; First, RA/DEC
  ind = WHERE(STRPOS(name,":S") LT 0 OR STRPOS(name,":S1") GT 0,count)
  glactc,dra[ind]/15.0,ddec[ind],2000,dlon,dlat,1

  fili = !singgdir+'sample.dat'
  readcol_new,fili,raS,decS,lon,lat,$
                   format="X,X,A,A,X,X,X,X,X,X,X,X,I,I",comment="#"
  n_objs = N_ELEMENTS(raS)
  Sra = sexideg(raS)
  Sdec = sexideg(decS)
  glactc,Sra,Sdec,2000,lon,lat,1

  aitoff,dra[ind],ddec[ind],dx,dy
  aitoff,(Sra*15.0),Sdec,sx,sy

  IF psflag THEN BEGIN
    PLOT,dummy,dummy,XSTYLE=4,YSTYLE=4,/isotropic,$
         TITLE='',POSITION=[0.0,0.5,1.0,1.0], $
         CHARSIZE=1.5,XRANGE=[-180.0,180.0],YRANGE=[-90.0,90.0],COLOR=!black
  ENDIF ELSE BEGIN
    PLOT,dummy,dummy,XSTYLE=4,YSTYLE=4,/isotropic,$
         TITLE="RA [hr] vs. Declination [deg]", $
         CHARSIZE=1.5,XRANGE=[-180.0,180.0],YRANGE=[-90.0,90.0],COLOR=!black
  ENDELSE
  OPLOT,sx,sy,PSYM=SYM(sampsym),COLOR=samp_clr,SYMSIZE=sampsz
  OPLOT,dx,dy,PSYM=SYM(dersym),COLOR=!ddgray,SYMSIZE=dersz
  OPLOT,dx,dy,PSYM=SYM(dersym),COLOR=der_clr,SYMSIZE=dersz-0.25
  AITOFF_GALACTIC,45,15,gal_clr
  AITOFF_GRID,30,10,LABEL=2
  XYOUTS,-180,75,'(a)',CHARSIZE=1.5,ALIGNMENT=0.5,COLOR=!black

  aitoff,dlon,dlat,dx,dy
  aitoff,lon,lat,sx,sy

; Second, Lat/Lon
  IF NOT psflag THEN title = "Galactic Longitude [deg] vs. Latitude [deg]"

  IF psflag THEN BEGIN
    PLOT,dummy,dummy,XSTYLE=4,YSTYLE=4,/isotropic,$
         TITLE='',POSITION=[0.0,0.0,1.0,0.5], $
         CHARSIZE=1.5,XRANGE=[-180.0,180.0],YRANGE=[-90.0,90.0],COLOR=!black
  ENDIF ELSE BEGIN
    PLOT,dummy,dummy,XSTYLE=4,YSTYLE=4,/isotropic,$
         TITLE="Galactic Longitude [deg] vs. Latitude [deg]", $
         CHARSIZE=1.5,XRANGE=[-180.0,180.0],YRANGE=[-90.0,90.0],COLOR=!black
  ENDELSE
  OPLOT,sx,sy,PSYM=SYM(sampsym),COLOR=samp_clr,SYMSIZE=sampsz
  OPLOT,dx,dy,PSYM=SYM(dersym),COLOR=!ddgray,SYMSIZE=dersz
  OPLOT,dx,dy,PSYM=SYM(dersym),COLOR=der_clr,SYMSIZE=dersz-0.25
  AITOFF_GRID,30,10,/label
  XYOUTS,-180,75,'(b)',CHARSIZE=1.5,ALIGNMENT=0.5,COLOR=!black

  IF psflag THEN BEGIN
    psend,!outdir+"/galcoords.eps",/noprint,/clobber
    set_plot,'PS'
    xs = 11.0
    ys = 10.5
    xoff = 1.0
    yoff = 3.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
             bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,2,2,0,0]
    charsz = 1.5
    symsz = 1.2
    thick = 2.0
  ENDIF

; Fraction of SFRD due to SFR (Halpha luminosity density)
  xmax = 2.0
  xmin = -3.0
  Habinsize = 0.5
  sfr = logl_ha0 - ALOG10(imf)
; Was "!6log(L!DH!7a!6!N [erg s!E-1!N])"
  singg_density_plot,sfr,[xmin,xmax],Habinsize,frac, $
                     "!6log(SFR [M!Dsolar!N yr!E-1!N])", $
                     psflag,bwflag,charsz=charsz

; Fraction of luminosities in relation to logse_ha0
  xmax = 1.0
  xmin = -4.0
  sebinsize = 0.25
  sfrdens = logse_ha0 - ALOG10(imf) + ALOG10(4.0 * !pi * (206265.)^2) + 2.0*ALOG10(cm_Mpc*1E-3)
;!dtor/3600.0 * (distance*1E3))
;"!6log(S!De!N(H!7a!6) [erg cm!E-2!N s!E-1!N arcsec!E-2!N])"
  singg_density_plot,sfrdens,[xmin,xmax],sebinsize,frac, $
                     "!6log(!7R!6!De!N(H!7a!6) [M!Dsolar!N yr!E-1!N kpc!E-2!N])", $
                     psflag,bwflag,charsz=charsz

; Fraction of luminosities in relation to SFR/<SFR>
  xmax = 1.4
  xmin = -2.4
  bbinsize = 0.2
  b = ALOG10(ew_imf(ew50_0_t,/SILENT))
  singg_density_plot,b,[xmin,xmax],bbinsize,frac, $
                     "!6log(SFR/<SFR>)", $
                     psflag,bwflag,charsz=charsz

; Fraction of luminosities in relation to tgas
  xmax = 11.25
  xmin = 7.75
  tgbinsize = 0.25
  singg_density_plot,ALOG10(tg)+9.0,[xmin,xmax],tgbinsize,fr, $
                     "!6log(t!Dgas!N [yr])", $
                     psflag,bwflag,charsz=charsz

  IF psflag THEN BEGIN
    psend,!outdir+"/hanishplots_sb.eps",/noprint,/clobber
  ENDIF

  RETURN
END
