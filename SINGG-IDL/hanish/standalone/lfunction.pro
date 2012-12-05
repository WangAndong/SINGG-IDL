PRO lfunction,SINGLE=single
; Calculates an Halpha luminosity function from the SINGG database.

  dbopen,"singg_derived",0
  dbext,-1,"NAME,OBJECT,LOGMHI,LOGL_HA_T,LOGL_HA_O,FLAG_O,MABS_R0_T,LOGL_R0_T,DISTANCE", $
            name,object,logMHI,logl_ha_t,logl_ha_o,flag_o,mabs_r0_t,logl_r0_t,distance
  derind = good_derived(exclopt=1)-1
  dbclose
  object = update_name(object)

  cm_Mpc = 3.0857D24; cm per Mpc

  logl_ha = DOUBLE(logl_ha_t)
  ind = WHERE(STRTRIM(flag_o,2) EQ "T",count)
  IF count GT 0 THEN BEGIN
; If flag_O is set for this galaxy, replace each _T value with the _O value
    logl_ha[ind] = logl_ha_o[ind]
  ENDIF

;  logl_r0 = (mabs_r0_t - 4.64)/(-2.5)
; Alternatively:
  logl_r0 = logl_r0_t - 29.6358

  MHIpatch = 10.d0^(0.644862*logl_r0 + 3.18060)
; First, figure out the value of theta for each member.

  hmonte = 100
  setup_himf,theta0,refmass,alpha,himf=-1,num_monte=hmonte ; ,/debug
  n_gals = N_ELEMENTS(derind)

  num_in_mbin = FLTARR(n_gals)
  th = FLTARR(n_gals)
  dth = FLTARR(n_gals)
  FOR ii = 0,n_gals-1 DO BEGIN
    theta = calc_theta(logMHI[derind[ii]],theta0[*,0:hmonte], $
                       refmass[*,0:hmonte],alpha[*,0:hmonte],sigtheta)
    th[ii] = ALOG10(theta[0])
    dth[ii] = MEAN(sigtheta) / (ALOG(10)*theta[0])
  ENDFOR
  th_old = th

; Loop over objects
  sing = WHERE(STRPOS(name[derind],':S') LT 0,scount)
  IF KEYWORD_SET(single) THEN BEGIN
    mcount = 0
    lMHI = logMHI[derind[sing]]
    logl = logl_ha[derind[sing]]
    good = BYTARR(scount)+1b
    th = th[sing]
  ENDIF ELSE BEGIN
; Add ELdots
    readcol_new,!singgdir+"hstproplist.txt",elname,elra,eldec,elew,elr25, $
                elHa,elR,COMMENT='#',/SILENT,FORMAT='(A,A,A,F,F,F,F)'
    num_els = N_ELEMENTS(elname)
    elname = update_name(elname)

    lMHI = [logMHI[derind],FLTARR(num_els)]
    logl = [logl_ha[derind],FLTARR(num_els)]
    good = BYTARR(n_gals+num_els)+1b
    obj = [object[derind],STRARR(num_els)]
    Mpatch = [MHIpatch[derind],FLTARR(num_els)]
    th = [th,FLTARR(num_els)]
    dth = [dth,FLTARR(num_els)]
    num_in_mbin = [num_in_mbin,INTARR(num_els)]

; First, assign the eldots
    FOR ii = 0,num_els-1 DO BEGIN
      match = WHERE(STRTRIM(object[derind],2) EQ STRTRIM(elname[ii],2),count)
      IF count EQ 0 THEN BEGIN
        PRINT,"ERROR in lfunction: can't match eldot ",elname[ii]
        RETURN
      ENDIF
      lMHI[n_gals+ii] = logMHI[derind[match[0]]]
      obj[n_gals+ii] = STRTRIM(elname[ii],2)
name[derind[match[0]]] = STRTRIM(name[derind[match[0]]],2)+":S1"
      th[n_gals+ii] = th[match[0]]
      dth[n_gals+ii] = dth[match[0]]

      dist = distance[derind[match[0]]]*cm_Mpc
      logl[n_gals+ii] = ALOG10(elHa[ii] * 4.0*!pi*dist^2.0)
      logl_r0_el = ALOG10(elR[ii] * 4.0*!pi*dist^2.0)
      Mpatch[n_gals+ii] = 10.d0^(0.644862*(logl_r0_el - 29.6358) + 3.18060)

; print,elname[ii],logl[n_gals+ii],logl_r0_el,ALOG10(Mpatch[n_gals+ii]) ; values in the 37-38.5 range
    ENDFOR

    m1 = WHERE(STRPOS(name[derind],':S1') GE 0,mcount)
    FOR ii = 0,mcount-1 DO BEGIN
      gals = WHERE(STRTRIM(obj,2) EQ STRTRIM(object[derind[m1[ii]]],2),galcount)

      HIfrac = Mpatch[gals]/TOTAL(Mpatch[gals])
      lMHI[gals] = logMHI[derind[m1[ii]]]+ALOG10(HIfrac)

check = (10.d0^lMHI[gals] / (1.18E6)) / distance[derind[m1[ii]]]
      good[gals] = (10.d0^lMHI[gals] / (1.18E6)) GE distance[derind[m1[ii]]]

      th[gals] = th[gals]-(1.5*ALOG10(HIfrac))
print,object[derind[m1[ii]]],logMHI[derind[m1[ii]]],distance[derind[m1[ii]]],th[gals[0]]
forprint,lMHI[gals],HIfrac,check,ALOG10(Mpatch[gals]),logl[gals],th[gals],good[gals]
    ENDFOR
  ENDELSE

  valid = WHERE(good)
  invalid = WHERE(good NE 1b,badcount)

  FOR ii = 0,N_ELEMENTS(lMHI)-1 DO BEGIN
    junk = WHERE(FIX(2.0*lMHI[valid]) EQ FIX(2.0*lMHI[ii]),count)
    num_in_mbin[ii]=count
  ENDFOR

; Now that we've got an array of values to work with, bin them.

  minl = 38.0
  maxl = 42.0
  binsize = 0.5
  num_bins = NINT((maxl-minl)/binsize)
  lumbin = minl + binsize*(FINDGEN(num_bins)+0.5)

  set_plot,'X'
  setplotcolors
  setbgfg,!white,!black
  DEVICE, RETAIN=2, DECOMPOSED=0
  WINDOW,XSIZE=800,YSIZE=1100
  !P.MULTI=[0,0,3,0,0]

  charsz = 3.0
  symsz = 1.5
  thick = 1.0

  plothist,logl,xhist,yhist,bin=binsize,XRANGE=[minl,maxl],/FILL, $
           FCOLOR=!ddgray,HALFBIN=0,/NAN,COLOR=!black,CHARSIZE=charsz, $
           THICK=thick

  plot,lMHI,th,COLOR=!black,CHARSIZE=charsz,PSYM=SYM(1), $
       XTITLE='log(MHI)',YTITLE='theta'
  IF badcount GT 0 THEN oplot,lMHI[invalid],th[invalid],COLOR=!red,PSYM=SYM(1)
  mind = SORT(lMHI)

  schechter_log,lMHI[mind],[theta0[0,0],refmass[0,0],alpha[0,0]],yval,dy
  PLOTS,lMHI[mind],yval,COLOR=!dgreen,LINESTYLE=2

    set_plot,'PS'
    setplotcolors ; PS mode changes you to 256 colors
   xs = 6.5
    ys = 5.0
    xoff = 1.0
    yoff = 3.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
          bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,0,1,0,0]
    charsz = 1.5

  plot,logl,th,COLOR=!black,CHARSIZE=charsz,PSYM=SYM(1), $
       XTITLE='!6log(L!DH!7a!6!N [erg s!E-1!N])', $
       YTITLE='!6log(!7h!6 [Mpc!E-3!N])'
  IF badcount GT 0 THEN oplot,logl[invalid],th[invalid],COLOR=!red,PSYM=SYM(1)

; bin it
  num_in_bin = INTARR(num_bins)
  b_th = DBLARR(num_bins)
  b_err = DBLARR(num_bins)
  FOR ii = 0,num_bins-1 DO BEGIN
    ind = WHERE(logl GE minl + binsize*FLOAT(ii) AND $
                logl LT minl + binsize*FLOAT(ii+1),count)
    IF count GT 0 THEN BEGIN
      num_in_bin[ii] = count
      b_th[ii] = MEAN(th[ind])
      b_err[ii] = STDDEV(th[ind])/SQRT(count)
; print,b_th[ii],b_err[ii]
    ENDIF ELSE BEGIN
      PRINT,"empty bin: ",minl + binsize*FLOAT([ii,ii+1])
    ENDELSE
  ENDFOR
;  PLOTS,lumbin,b_th,COLOR=!red,LINESTYLE=1

; Then, do a schechter fit.

  fittol = 1D-12
  Aold = DOUBLE([2.854D-3,41.397,-1.308]) ; single, no dust correction
;  Aold = DOUBLE([2.03D-3,42.03,-1.304]) ; single, dust correction
;  Aold = DOUBLE([4.19D-4,42.830,-1.422]) ; plus multiple
  A = Aold
  lind = SORT(logl)
  schechter_log,logl[lind],Aold,yval,dy
  PLOTS,logl[lind],yval,COLOR=!dgreen,LINESTYLE=2

  bind = WHERE(num_in_bin GT 0,count)

funct = 'schechter_log'

;  lum = lumbin[bind]
;  Y = b_th[bind]
;  err = b_err[bind]

lum = logl[valid]
Y = th[valid]
err = dth[valid] / SQRT(num_in_mbin[valid])

  weights = 1.d0/err^2.d0

;print,N_ELEMENTS(lum),N_ELEMENTS(Y),N_ELEMENTS(weights)
;forprint,lum,Y,weights

  fit=CURVEFIT(lum,Y,weights,A,sigA,FUNCTION_NAME=funct, $
               /double,TOL=fittol,iter=iter,itmax=10000,status=status,chisq=chisq)
;      parms = MPFITFUN('schechter_mp',lum,Y,err,A, $
;              WEIGHTS=weights,PERROR=sigA,/QUIET,STATUS=status)
;      A = parms

  FORPRINT,Aold,A
  PRINT,iter,chisq

;  schechter_log,logl[lind],A,yval,dy
;  PLOTS,logl[lind],yval,COLOR=!blue,LINESTYLE=2

    psend,!outdir+"/lfunction.eps",/noprint,/clobber

  RETURN

END
