PRO setup_himf,theta0,refmass,alpha,HIMF=himf,NUM_MONTE=num_monte, $
               H0=H0,CURVE=curve,DEBUG=debug,LINEAR=linear,PS=ps
; Initialize the HI Mass Function.
; OPTIONAL INPUTS
;   num_monte   Number of Monte Carlo iterations(in/out)
;   himf        Sets what set of data parameters to use.
;                 0: 2005 values with Mould distance model [default]
;                 1: Zwaan 2005
;                 2: Zwaan 2003
;                 3: Kilborn 1999
;                 4: Zwaan 1997
;                 5: Rosenberg 2002 (ADBS)
;                 -1: Data files from Zwaan
;   H0          Hubble constant; default is 70 km/s/Mpc
;   /curve      Use the Schechter function itself instead of
;                 assembling from raw data
;   /debug      Do a bunch of extra stuff.
;   /ps           If we're in debug mode, write to a postscript file
; OUTPUTS
;   theta0[2,M+1] Value and uncertainty
;   refmass[2,M+1]  of the three HI Mass
;   alpha[2,M+1]    Function parameters

  logmode = NOT KEYWORD_SET(linear)
  psflag = KEYWORD_SET(ps)
  IF NOT KEYWORD_SET(himf) THEN himf = 0
  IF NOT KEYWORD_SET(num_monte) THEN num_monte = 0
  IF NOT KEYWORD_SET(H0) THEN H0 = 70.0
  al10 = ALOG(10.d0)

  IF himf EQ -1 AND num_monte GT 0 THEN BEGIN
    num_monte = 100 ; number of data files we have
    max_bins = 40
    tmass = FLTARR(num_monte,max_bins)
    ttheta = FLTARR(num_monte,max_bins)
    tdth = FLTARR(num_monte,max_bins)
    tdth2 = FLTARR(max_bins)
    tnum = INTARR(num_monte,max_bins)
    tsize = INTARR(num_monte)
    FOR mm = LONG(0),LONG(num_monte)-1 DO BEGIN
      filename = "/home/hanish/data/zwaan/swmlphi."+STRTRIM(STRING(500+mm),2)
      readcol_new,filename,junk,lmhi,th,num,dth, $
                  /SILENT,COMMENT='#',FORMAT='(I,F,F,I,F)'
      ind = WHERE(num GT 0,num_himfbins)
      tsize[mm] = num_himfbins
      tmass[mm,0:num_himfbins-1] = DOUBLE(lmhi[ind]) - 2.0*ALOG10(H0 / 75.0)
      tnum[mm,0:num_himfbins-1] = num[ind]
      ttheta[mm,0:num_himfbins-1] = DOUBLE(ALOG10(th[ind])) + 3.0*ALOG10(H0 / 75.0)
      tdth[mm,0:num_himfbins-1] = DOUBLE(dth[ind]/(al10*th[ind]))
    ENDFOR

    min_tmass = MIN(tmass[*,0:30])
    FOR ii = 0,max_bins-1 DO BEGIN
      ind = WHERE(ROUND((tmass-min_tmass)*10.0) EQ ii,count)
      IF count GT 1 THEN BEGIN
        tdth2[ii] = STDDEV(ttheta[ind])
;print,tdth2[ii],MEAN(tdth[ind])
      ENDIF
    ENDFOR
  ENDIF

  theta0 = DBLARR(2,num_monte+1)
  refmass = DBLARR(2,num_monte+1)
  alpha = DBLARR(2,num_monte+1)
  stat = INTARR(num_monte+1)

; Base Schechter function values.  If we aren't using Monte Carlo or
; assembling a new function later, these values are the basis of the
; output.
; Commented values were the quoted fits from the papers, the used
; values are those from our own fit_himf routine, using an iterative
; fit in logarithmic space.
  CASE himf OF
    0: BEGIN
;         btheta0 = [5.372D-3,0.8D-3] * (H0 / 75.d0)^(3.0)  ; in Mpc^-3
         btheta0 = [4.483D-3,0.7D-3] * (H0 / 70.d0)^(3.0)  ; in Mpc^-3
;         brefmass = [9.805D0 - 2.0*ALOG10(H0 / 75.d0),0.03] ; log10, in solar masses
         brefmass = [9.854D0 - 2.0*ALOG10(H0 / 70.d0),0.03] ; log10, in solar masses
;         balpha = [-1.386,0.03] ; unitless
         balpha = [-1.3826,0.03] ; unitless
         himffile = !singgdir+"/mould_himf.dat"
       END
    1: BEGIN
         btheta0 = [6.0D-3,0.8D-3] * (H0 / 75.d0)^(3.0)  ; in Mpc^-3
         brefmass = [9.80D0 - 2.0*ALOG10(H0 / 75.d0),0.03] ; log10, in solar masses
         balpha = [-1.37,0.03] ; unitless
         himffile = ""
       END
    2: BEGIN
;         btheta0 = [8.6D-3,2.1D-3] * (H0 / 75.d0)^(3.0)  ; in Mpc^-3
         btheta0 = [7.07D-3,1.7D-3] * (H0 / 70.d0)^(3.0)  ; in Mpc^-3
;         brefmass = [9.79D0 - 2.0*ALOG10(H0 / 75.d0),0.06] ; log10, in solar masses
         brefmass = [9.865D0 - 2.0*ALOG10(H0 / 70.d0),0.06] ; log10, in solar masses
;         balpha = [-1.3,0.08] ; unitless
         balpha = [-1.331,0.08] ; unitless
         himffile = !singgdir+"/hipass_himf.dat"
       END
    3: BEGIN
         btheta0 = [1.0D-2,0.0D-3] * (H0 / 100.d0)^(3.0)  ; in Mpc^-3
         brefmass = [9.5D0 - 2.0*ALOG10(H0 / 100.d0),0.0] ; log10, in solar masses
         balpha = [-1.32,0.0] ; unitless
         himffile = ""
       END
    4: BEGIN
         btheta0 = [1.4D-2,0.0D-3] * (H0 / 100.d0)^(3.0)  ; in Mpc^-3
         brefmass = [9.55D0 - 2.0*ALOG10(H0 / 100.d0),0.0] ; log10, in solar masses
         balpha = [-1.2,0.0] ; unitless
         himffile = ""
       END
    5: BEGIN
         btheta0 = [5.8D-3,0.0D-3] * (H0 / 75.d0)^(3.0)  ; in Mpc^-3
         brefmass = [9.88D0 - 2.0*ALOG10(H0 / 75.d0),0.0] ; log10, in solar masses
         balpha = [-1.53,0.0] ; unitless
         himffile = ""
       END
    -1: BEGIN
         btheta0 = [3.896D-3,0.7D-3] * (H0 / 70.d0)^(3.0)  ; in Mpc^-3
         brefmass = [9.921D0 - 2.0*ALOG10(H0 / 70.d0),0.039] ; log10, in solar masses
         balpha = [-1.409,0.049] ; unitless
         himffile = ""
       END
    ELSE: BEGIN
         PRINT,"ERROR in setup_himf: invalid HIMF mode ",himf
         RETURN
       END
; This used only the "random" uncertainties.  There are systematic
; ones as well; they're (0.05 for alpha, 0.03 for refmass, 0.6E-3 for theta0)
  ENDCASE

  theta0[*,0] = btheta0
  refmass[*,0] = brefmass
  alpha[*,0] = balpha
  stat[0] = 0

; If we're not in Monte Carlo mode, we're done
  IF num_monte EQ 0 THEN RETURN

  curveflag = KEYWORD_SET(curve)
  IF himf GE 0 AND NOT FILE_TEST(himffile) THEN BEGIN
    IF STRLEN(himffile) GT 1 THEN BEGIN
      PRINT,"WARNING in setup_himf: invalid HIMF file ",himffile
    ENDIF ELSE BEGIN
      PRINT,"WARNING in setup_himf: no HIMF file declared"
    ENDELSE
    curveflag = 1b
  ENDIF

  IF curveflag THEN BEGIN
    theta0[0,1:num_monte] = btheta0[0] + RANDOMN(seed,num_monte,/DOUBLE)*btheta0[1]
    theta0[1,1:num_monte] = MAKE_ARRAY(num_monte,/DOUBLE,VALUE=btheta0[1])

    refmass[0,1:num_monte] = brefmass[0] + RANDOMN(seed,num_monte,/DOUBLE)*brefmass[1]
    refmass[1,1:num_monte] = MAKE_ARRAY(num_monte,/DOUBLE,VALUE=brefmass[1])

    alpha[0,1:num_monte] = balpha[0] + RANDOMN(seed,num_monte,/DOUBLE)*balpha[1]
    alpha[1,1:num_monte] = MAKE_ARRAY(num_monte,/DOUBLE,VALUE=balpha[1])
    ind = INDGEN(num_monte+1)
  ENDIF ELSE BEGIN
; Read in the raw HIMF data.
    IF himf GE 0 THEN BEGIN
      readcol_new,himffile,massbin,num,logtheta,sigplus,sigminus, $
                  /SILENT,COMMENT='#',FORMAT='(F,I,F,F,F)'
      massbin = DOUBLE(massbin) - 2.0*ALOG10(H0 / 75.0)
      logtheta = DOUBLE(logtheta) + 3.0*ALOG10(H0 / 75.0)
      num_himfbins = N_ELEMENTS(massbin)
    ENDIF

; Set up the HI Mass Function by Schechter-fitting to the logtheta array
    IF logmode THEN funct = 'schechter_log' ELSE funct = 'schechter'
    IF himf GE 0 THEN BEGIN
      fittol = 1D-7
    ENDIF ELSE BEGIN
      fittol = 1D-6
    ENDELSE

    Aold = DOUBLE([btheta0[0],brefmass[0],balpha[0]])
    FOR mm = LONG(0),LONG(num_monte)-1 DO BEGIN
      A = Aold
      IF himf GE 0 THEN BEGIN
        newth = FLTARR(num_himfbins)
        FOR ii = 0,num_himfbins-1 DO BEGIN
          val = RANDOMN(seed,1,/DOUBLE)
          IF val GT 0.0 THEN BEGIN
            newth[ii] = 1.d0 + val*(10.d0^sigplus[ii]-1.d0)
          ENDIF ELSE BEGIN
            newth[ii] = MAX([0.01,(1.d0 + val*(1.d0-10.d0^(-1.d0*sigminus[ii])))])
          ENDELSE
        ENDFOR      
      ENDIF ELSE BEGIN
; We're using the bootstrap files, so we'll continually overwrite the
; massbin/logtheta values.
        newth = 1.d0
        num_himfbins = tsize[mm]
        massbin = FLTARR(num_himfbins)
        logtheta = FLTARR(num_himfbins)
        sigplus = FLTARR(num_himfbins)

        massbin[0:tsize[mm]-1] = tmass[mm,0:tsize[mm]-1]
        logtheta[0:tsize[mm]-1] = ttheta[mm,0:tsize[mm]-1]
;        sigplus[0:tsize[mm]-1] = tdth[mm,0:tsize[mm]-1]
        sigplus[0:tsize[mm]-1] = tdth2[ROUND((tmass[mm,0:tsize[mm]-1]-min_tmass)*10.d0)]
        sigminus = sigplus
      ENDELSE

      IF logmode THEN BEGIN
        Y = logtheta + ALOG10(newth)
        err = (sigplus+sigminus)/2.d0
      ENDIF ELSE BEGIN
        Y = 10.d0^logtheta * newth
        err = (10.d0^(logtheta+sigplus)-10.d0^(logtheta-sigminus))/2.d0
      ENDELSE

; Use one of the following:
; Poisson:
;      weights = 1.d0/Y
; Gaussian:
      weights = 1.d0/err^2.d0
; Linear:
;      weights = DBLARR(num_himfbins)+1.0

;      parms = MPFITFUN('schechter_mp',massbin,Y,err,A, $
;              WEIGHTS=weights,PERROR=sigA,/QUIET,STATUS=status)
;      A = parms
;      status = FIX(status EQ 0)

      fit=CURVEFIT(massbin,Y,weights,A,sigA,FUNCTION_NAME=funct, $
                   /double,TOL=fittol,iter=iter,itmax=1000,status=status,chisq=chisq)
      theta0[*,mm+1]  = [A[0],sigA[0]]
      refmass[*,mm+1] = [A[1],sigA[1]]
      alpha[*,mm+1]   = [A[2],sigA[2]]
      stat[mm+1] = status
    ENDFOR

    ind = WHERE(stat EQ 0,fitcount)
    IF fitcount EQ 0 THEN BEGIN
      PRINT,"ERROR in setup_himf: no valid fits performed ",fitcount
      RETURN
    ENDIF
    maxiter= 100
    thnsig = 10.0
    rmnsig = 10.0
    alnsig = 10.0
    grm_avsigclip,theta0[0,ind],thnsig,maxiter,th0,th0sig,ngood,nbad,niter
    grm_avsigclip,refmass[0,ind],rmnsig,maxiter,rm0,rm0sig,ngood,nbad,niter
    grm_avsigclip,alpha[0,ind],alnsig,maxiter,al0,al0sig,ngood,nbad,niter

    IF KEYWORD_SET(debug) THEN BEGIN
      ind2 = WHERE(ABS(theta0[0,*]-th0) LT thnsig*th0sig,count2)
      ind3 = WHERE(ABS(refmass[0,*]-rm0) LT rmnsig*rm0sig,count3)
      ind4 = WHERE(ABS(alpha[0,*]-al0) LT alnsig*al0sig,count4)
      print,count2,count3,count4

      IF psflag THEN BEGIN
        set_plot,'PS'
        setplotcolors
        xs = 10.0
        ys = 6.5
        xoff = 1.0
        yoff = 3.0
        DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
               bits_per_pixel=8,/encapsulated
        charsz = 2.0
      ENDIF ELSE BEGIN
        set_plot,'X'
        setplotcolors
        setbgfg,!white,!black
        DEVICE, RETAIN=2, DECOMPOSED=0
        WINDOW,XSIZE=1200,YSIZE=800
        charsz = 3.0
      ENDELSE

      !P.MULTI=[0,3,2,0,0]

      thrange = th0 + th0sig*[-1.0,1.0] * 4.0
      rmrange = rm0 + rm0sig*[-1.0,1.0] * 4.0
      alrange = al0 + al0sig*[-1.0,1.0] * 4.0
;      thrange = btheta0[0] + btheta0[1]*[-1.0,1.0] * 0.5
;      rmrange = brefmass[0] + brefmass[1]*[-1.0,1.0] * 2.0
;      alrange = balpha[0] + balpha[1]*[-1.0,1.0]
      thbin = 0.00002
      rmbin = 0.002
      albin = 0.002

      PRINT,"  Theta*: ",th0," +/- ",th0sig," +/- ",btheta0[1]
      plothist,theta0[0,ind],xh,yh,bin=thbin,HALFBIN=0,/NAN,COLOR=!black, $
               XRANGE=thrange,CHARSIZE=charsz,XTITLE="Theta*",YSTYLE=1
      PLOTS,th0*[1.0,1.0],MAX(yh)*[0,1],COLOR=!red
      PLOTS,th0 + th0sig*[1.0,1.0],MAX(yh)*[0,1],COLOR=!blue
      PLOTS,th0 - th0sig*[1.0,1.0],MAX(yh)*[0,1],COLOR=!blue
      PRINT,"  M*: ",rm0," +/- ",rm0sig," +/- ",brefmass[1]
      plothist,refmass[0,ind],xh,yh,bin=rmbin,HALFBIN=0,/NAN,COLOR=!black, $
               XRANGE=rmrange,CHARSIZE=charsz,XTITLE="M*",YSTYLE=1
      PLOTS,rm0*[1.0,1.0],MAX(yh)*[0,1],COLOR=!red
      PLOTS,rm0 + rm0sig*[1.0,1.0],MAX(yh)*[0,1],COLOR=!blue
      PLOTS,rm0 - rm0sig*[1.0,1.0],MAX(yh)*[0,1],COLOR=!blue
      PRINT,"  Alpha: ",al0," +/- ",al0sig," +/- ",balpha[1]
      plothist,alpha[0,ind],xh,yh,bin=albin,HALFBIN=0,/NAN,COLOR=!black, $
               XRANGE=alrange,CHARSIZE=charsz,XTITLE="Alpha",YSTYLE=1
      PLOTS,al0*[1.0,1.0],MAX(yh)*[0,1],COLOR=!red
      PLOTS,al0 + al0sig*[1.0,1.0],MAX(yh)*[0,1],COLOR=!blue
      PLOTS,al0 - al0sig*[1.0,1.0],MAX(yh)*[0,1],COLOR=!blue

      PLOT,theta0[0,ind],alpha[0,ind],COLOR=!black,PSYM=SYM(0), $
           XTITLE="Theta*",YTITLE="Alpha",CHARSIZE=charsz,XRANGE=thrange,YRANGE=alrange,YSTYLE=1
      PLOTS,btheta0[0],balpha[0],COLOR=!blue,PSYM=SYM(1),SYMSIZE=1.2
      PLOTS,btheta0[0]+btheta0[1]*[-1.0,1.0],balpha[0],COLOR=!blue,PSYM=-SYM(1),SYMSIZE=0.5,THICK=2.0
      PLOTS,btheta0[0],balpha[0]+balpha[1]*[-1.0,1.0],COLOR=!blue,PSYM=-SYM(1),SYMSIZE=0.5,THICK=2.0
      PLOTS,th0,al0,COLOR=!red,PSYM=SYM(1),SYMSIZE=1.2
      PLOTS,th0+th0sig*[-1.0,1.0],al0,COLOR=!red,PSYM=-SYM(1),SYMSIZE=0.5,THICK=2.0
      PLOTS,th0,al0+al0sig*[-1.0,1.0],COLOR=!red,PSYM=-SYM(1),SYMSIZE=0.5,THICK=2.0
      PLOT,refmass[0,ind],alpha[0,ind],COLOR=!black,PSYM=SYM(0), $
           XTITLE="M*",YTITLE="Alpha",CHARSIZE=charsz,XRANGE=rmrange,YRANGE=alrange,YSTYLE=1
      PLOTS,brefmass[0],balpha[0],COLOR=!blue,PSYM=SYM(1),SYMSIZE=1.2
      PLOTS,brefmass[0]+brefmass[1]*[-1.0,1.0],balpha[0],COLOR=!blue,PSYM=-SYM(1),SYMSIZE=0.5,THICK=2.0
      PLOTS,brefmass[0],balpha[0]+balpha[1]*[-1.0,1.0],COLOR=!blue,PSYM=-SYM(1),SYMSIZE=0.5,THICK=2.0
      PLOTS,rm0,al0,COLOR=!red,PSYM=SYM(1),SYMSIZE=1.2
      PLOTS,rm0+rm0sig*[-1.0,1.0],al0,COLOR=!red,PSYM=-SYM(1),SYMSIZE=0.5,THICK=2.0
      PLOTS,rm0,al0+al0sig*[-1.0,1.0],COLOR=!red,PSYM=-SYM(1),SYMSIZE=0.5,THICK=2.0
      PLOT,refmass[0,ind],theta0[0,ind],COLOR=!black,PSYM=SYM(0), $
           XTITLE="M*",YTITLE="Theta*",CHARSIZE=charsz,XRANGE=rmrange,YRANGE=thrange,YSTYLE=1
      PLOTS,brefmass[0],btheta0[0],COLOR=!blue,PSYM=SYM(1),SYMSIZE=1.2
      PLOTS,brefmass[0]+brefmass[1]*[-1.0,1.0],btheta0[0],COLOR=!blue,PSYM=-SYM(1),SYMSIZE=0.5,THICK=2.0
      PLOTS,brefmass[0],btheta0[0]+btheta0[1]*[-1.0,1.0],COLOR=!blue,PSYM=-SYM(1),SYMSIZE=0.5,THICK=2.0
      PLOTS,rm0,th0,COLOR=!red,PSYM=SYM(1),SYMSIZE=1.2
      PLOTS,rm0+rm0sig*[-1.0,1.0],th0,COLOR=!red,PSYM=-SYM(1),SYMSIZE=0.5,THICK=2.0
      PLOTS,rm0,th0+th0sig*[-1.0,1.0],COLOR=!red,PSYM=-SYM(1),SYMSIZE=0.5,THICK=2.0

      IF psflag THEN BEGIN
        IF logmode THEN psend,!outdir+"/himf_log.eps",/noprint,/clobber $
                   ELSE psend,!outdir+"/himf_lin.eps",/noprint,/clobber
      ENDIF

    ENDIF

; adjust the "average" values
;    theta0[*,0] = [th0,th0sig]
;    refmass[*,0] = [rm0,rm0sig]
;    alpha[*,0] = [al0,al0sig]

    ind = WHERE(ABS(theta0[0,1:num_monte]-th0) LT thnsig*th0sig AND $
                ABS(refmass[0,1:num_monte]-rm0) LT rmnsig*rm0sig AND $
                ABS(alpha[0,1:num_monte]-al0) LT alnsig*al0sig AND $
                stat EQ 0,count)
    ind = [0,ind+1]
    PRINT,"  Valid HIMFs: ",count," out of ",num_monte

    theta0 = theta0[*,ind]
    refmass = refmass[*,ind]
    alpha = alpha[*,ind]
    num_monte = count
  ENDELSE

  PRINT,"HI Mass Function initialized successfully."

  RETURN

END
