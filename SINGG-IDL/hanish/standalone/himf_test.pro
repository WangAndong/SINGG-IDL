PRO himf_test

  binsize = 0.2
  xmin = 6.8
  xmax = 10.6
  num_bins = LONG((xmax-xmin)/binsize) + 1

;  xbin = [0.0,4.0,5.0,6.0,7.0,8.0,8.5,9.0,9.5,10.0,10.5,11.0,11.5]
  xbin = FINDGEN(num_bins)*binsize + xmin

  theta0 = 8.6D-3  ; in Mpc^-3
  refmass = 9.79D0 ; log10, in solar masses
  alpha = -1.30 ; unitless

;  theta0 = 6.0D-3  ; in Mpc^-3
;  refmass = 9.80D0 ; log10, in solar masses
;  alpha = -1.37 ; unitless

  theta = FLTARR(num_bins)
  bigtheta = FLTARR(num_bins)

  FOR ii = 0,num_bins-1 DO BEGIN
    massratio = 10.0^(xbin[ii]-refmass)
    theta[ii] = theta0 * massratio^alpha / EXP(massratio)
    bigtheta[ii] = theta[ii] * massratio * alog(10)
  ENDFOR

  FORPRINT,xbin,ALOG10(theta),ALOG10(bigtheta)

;  DEVICE, RETAIN=2, DECOMPOSED=0
;  WINDOW,XSIZE=800,YSIZE=1000
;  !P.MULTI=[0,0,1,0,0]

;  PLOT,xbin,ALOG10(theta),COLOR=100,XRANGE=[5.0,11.0]
;  OPLOT,xbin,ALOG10(bigtheta),COLOR=100

END

;        6.8       -1.23
;        7.0      -1.003
;        7.2     -0.7958
;        7.4      -1.163
;        7.6      -1.125
;        7.8     -0.9374
;        8.0      -1.285
;        8.2      -1.145
;        8.4       -1.23
;        8.6      -1.296
;        8.8      -1.489
;        9.0       -1.31
;        9.2      -1.642
;        9.4      -1.722
;        9.6      -1.925
;        9.8      -2.172
;       10.0      -2.491
;       10.2      -2.843
;       10.4       -3.71
;       10.6      -4.377
