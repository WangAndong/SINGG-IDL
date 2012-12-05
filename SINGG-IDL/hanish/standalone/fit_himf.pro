PRO fit_himf,himf

  H0 = 70.0
  al10 = ALOG(10.d0)

  IF himf LE 0 THEN himffile = !singgdir+"/mould_himf.dat" $
               ELSE himffile = !singgdir+"/hipass_himf.dat"

  IF himf GE 0 THEN BEGIN
    IF NOT FILE_TEST(himffile) THEN BEGIN
      PRINT,"ERROR in setup_himf: invalid HIMF file ",himffile
      RETURN
    ENDIF
    readcol_new,himffile,massbin,num,logtheta,sigplus,sigminus, $
                /SILENT,COMMENT='#',FORMAT='(F,I,F,F,F)'
    massbin = DOUBLE(massbin) - 2.0*ALOG10(H0 / 75.0)
    logtheta = DOUBLE(logtheta) + 3.0*ALOG10(H0 / 75.0)
    num_himfbins = N_ELEMENTS(massbin)
  ENDIF ELSE BEGIN
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
  ENDELSE

  funct = 'schechter_log'
  IF himf GE 0 THEN BEGIN
    Y = logtheta
    err = (sigplus+sigminus)/2.d0
    weights = 1.d0/err^2.d0
  ENDIF

  CASE himf OF
     0: Aold = [0.004483,9.854,-1.3826]
     2: Aold = [0.007072,9.8649,-1.33089]
    -1: Aold = [0.003896,9.921,-1.4075]
  ENDCASE
  IF himf GE 0 THEN BEGIN
    tol = 1E-8
    maxcount = 1000
  ENDIF ELSE BEGIN
    tol = 1E-8
    maxcount = 100
  ENDELSE

  del = 1.0
  count = 0
  A = Aold
  WHILE del GT tol AND count LT maxcount DO BEGIN
    IF himf GE 0 THEN BEGIN
      fit=CURVEFIT(massbin,Y,weights,A,sigA,FUNCTION_NAME=funct, $
                  /double,TOL=1E-8,iter=iter,itmax=1000,status=status,chisq=chisq)
      del = ABS(ALOG(A[0]/Aold[0])) + ABS(ALOG(A[1]/Aold[1])) + ABS(ALOG(A[2]/Aold[2]))
      PRINT,A[0],A[1],A[2],del,iter
      Aold = A
    ENDIF ELSE BEGIN
      theta0 = FLTARR(num_monte)
      refmass = FLTARR(num_monte)
      alpha = FLTARR(num_monte)
      stat = INTARR(num_monte)
      FOR mm = 0,num_monte-1 DO BEGIN
        A = Aold
        num_himfbins = tsize[mm]
        massbin = FLTARR(num_himfbins)
        logtheta = FLTARR(num_himfbins)
        sigplus = FLTARR(num_himfbins)
        massbin[0:tsize[mm]-1] = tmass[mm,0:tsize[mm]-1]
        logtheta[0:tsize[mm]-1] = ttheta[mm,0:tsize[mm]-1]
;        sigplus[0:tsize[mm]-1] = tdth[mm,0:tsize[mm]-1]
        sigplus[0:tsize[mm]-1] = tdth2[ROUND((tmass[mm,0:tsize[mm]-1]-min_tmass)*10.d0)]

        Y = logtheta
        weights = 1.d0/sigplus^2.d0

        fit=CURVEFIT(massbin,Y,weights,A,sigA,FUNCTION_NAME=funct, $
                     /double,TOL=1E-7,iter=iter,itmax=1000,status=status,chisq=chisq)
        theta0[mm] = A[0]
        refmass[mm] = A[1]
        alpha[mm] = A[2]
        stat[mm] = status
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
      grm_avsigclip,theta0[ind],thnsig,maxiter,th0,th0sig,ngood,nbad,niter
      grm_avsigclip,refmass[ind],rmnsig,maxiter,rm0,rm0sig,ngood,nbad,niter
      grm_avsigclip,alpha[ind],alnsig,maxiter,al0,al0sig,ngood,nbad,niter
      del = ABS(ALOG(th0/Aold[0])) + ABS(ALOG(rm0/Aold[1])) + ABS(ALOG(al0/Aold[2]))
print,th0,rm0,al0,del,fitcount
      Aold = [th0,rm0,al0]
    ENDELSE
count = count + 1
  ENDWHILE

  PRINT,Aold

  RETURN
END
