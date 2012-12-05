PRO temp_mass
; Make mass histograms for SINGG showing what was used in 1/2/3/6

  fullfile = "/home/hanish/sample3_ravsort.dat"
;  fullfile = "/home/hanish/singgsample_oct02.txt"
  readcol_new,fullfile,name,fulllmh1,COMMENT="#", $
              FORMAT="A,X,X,X,X,X,X,X,X,X,X,A,X,X"

  hipassfile = "/home/hanish/par_mF_mNB_may02.txt"
  readcol_new,hipassfile,index,hname,vel,sint,COMMENT="#", $
              FORMAT="(I,A,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,A,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,A,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X)"

  binsize = 0.2
  xmin = (7.0-binsize)
  xmax = (11.0+binsize)

  xbin = [7.0,7.8,8.2,8.4,8.6,8.8,9.0,9.2,9.4,9.6,9.8,10.0,10.2,10.4,10.6,11.0]
  num_bins = N_ELEMENTS(xbin)-1

  hnbin = FLTARR(num_bins)
  snbin = FLTARR(num_bins)

  H0 = 70.0 ; km/s/Mpc
  dist = vel / H0
  pi = ACOS(-1.0)

; sint is integrated flux of the profile in Jy km/s
; dist is distance in Mpc
  lmhhipass = ALOG10(2.36E5 * (dist)^2.0 * sint)

  FOR ii = 0,N_ELEMENTS(lmhhipass)-1 DO BEGIN
    binnum = 0
    FOR jj = 0,num_bins-1 DO BEGIN
      IF lmhhipass[ii] GT xbin[jj] AND lmhhipass[ii] LE xbin[jj+1] THEN binnum=jj
    ENDFOR

    hnbin[binnum] = hnbin[binnum]+0.2/(xbin[binnum+1]-xbin[binnum])
  ENDFOR

  FOR ii = 0,N_ELEMENTS(fulllmh1)-1 DO BEGIN
    binnum = 0
    FOR jj = 0,num_bins-1 DO BEGIN
      IF fulllmh1[ii] GT xbin[jj] AND fulllmh1[ii] LE xbin[jj+1] THEN binnum=jj
    ENDFOR

    snbin[binnum] = snbin[binnum]+0.2/(xbin[binnum+1]-xbin[binnum])
  ENDFOR

  DEVICE, RETAIN=2, DECOMPOSED=0
  WINDOW,XSIZE=800,YSIZE=800
  !P.MULTI=[0,0,1,0,0]

  dummy = FLTARR(2)

  ymin = 1.0

  PLOT,dummy,dummy,COLOR=1,CHARSIZE=2.0, /YLOG,$
       XTITLE="log(M!DH!II!N/M!Dsolar!N)",XRANGE=[7.0,11.0],YRANGE=[ymin,1000], $
       YTITLE="Number / ("+STRMID(STRTRIM(STRING(binsize),2),0,3)+" dex"

  FOR ii = 0,num_bins-1 DO BEGIN
; plot HIPASS
    POLYFILL,[xbin[ii],xbin[ii],xbin[ii+1],xbin[ii+1]], $
             [ymin,hnbin[ii],hnbin[ii],ymin], $
             COLOR=200,/DATA,LINESTYLE=0,THICK=1
    PLOTS,[xbin[ii],xbin[ii],xbin[ii+1],xbin[ii+1]], $
          [ymin,hnbin[ii],hnbin[ii],ymin],COLOR=1
; plot SINGG
    IF snbin[ii] GT ymin THEN BEGIN
      POLYFILL,[xbin[ii],xbin[ii],xbin[ii+1],xbin[ii+1]], $
               [ymin,snbin[ii],snbin[ii],ymin], $
               COLOR=100,/DATA,LINESTYLE=0,THICK=1
      PLOTS,[xbin[ii],xbin[ii],xbin[ii+1],xbin[ii+1]], $
            [ymin,snbin[ii],snbin[ii],ymin],COLOR=1
    ENDIF
  ENDFOR

END
