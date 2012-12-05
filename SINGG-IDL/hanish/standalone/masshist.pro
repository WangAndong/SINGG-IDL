PRO masshist,PS=ps
; Make mass histograms for SINGG

  psflag = KEYWORD_SET(ps)

; SR1
;  datafile = !singgdir+"observed.dat"
;  readcol_new,datafile,run,lmh1,COMMENT="#", $
;              FORMAT="X,A,X,X,X,X,X,X,X,X,X,X,A,X,X,X,X"
  dbopen,'singg_derived',0
  derind = good_derived2()+1
  dbext,derind,'NAME,OBJECT,RUNID,LOGMHI',fname,fobj,run,lmh1
  dbclose

; Filter out the multiple-source objects
  ind = WHERE(STRPOS(fname,':S') LT 0 OR STRPOS(fname,':S1') GT 0,totcount)
  fname = fname[ind]
  fobj = fobj[ind]
  run = run[ind]
  lmh1 = lmh1[ind]

  runnum = STRMID(STRTRIM(run,2),3,2)

  ind = WHERE(runnum EQ '01' OR runnum EQ '02' OR runnum EQ '03' OR runnum EQ '06',sr1count)
  sr1obj = fobj[ind]
  sr1run = run[ind]
  sr1lmh1 = lmh1[ind]

;print,totcount,sr1count

;forprint,sr1obj+' ',sr1run,sr1lmh1
;stop

; Full SINGG sample
;  fullfile = !singgdir+"sample3_ravsort.dat"
;  readcol_new,fullfile,name,fulllmh1,COMMENT="#", $
;              FORMAT="A,X,X,X,X,X,X,X,X,X,X,A,X,X"
  dbopen,'singg_sample',0
  dbext,-1,'NAME,LOGMHI',name,fulllmh1
  dbclose

  hipassfile = !singgdir+"par_mF_mNB_may02.txt"
  readcol_new,hipassfile,hindex,hname,vel,sint,COMMENT="#", $
              FORMAT="(I,A,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,A,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,A,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X)"
;              FORMAT="(I,A,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,A,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X, X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,A, X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X, X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X)"

  binsize = 0.2
  xmin = 6.8
  xmax = 11.0

  num_bins = ((xmax-xmin)/binsize)+3
  xbin = (FINDGEN(num_bins+1)-1) * binsize + xmin
  nbin = INTARR(num_bins)

  H0 = 70.0 ; km/s/Mpc
  dist = vel / H0
  pi = ACOS(-1.0)

; sint is integrated flux of the profile in Jy km/s
; dist is distance in Mpc
  lmhhipass = ALOG10(2.36E5 * (dist)^2.0 * sint)

  FOR ii = 0,N_ELEMENTS(hname)-1 DO BEGIN
    binnum = LONG((lmhhipass[ii] - xmin) / binsize)+1

    IF binnum LT 0 OR binnum GT (num_bins-1) THEN BEGIN
      PRINT,"Bad bin: ",ii," ",lmhhipass[ii]," ",sint[ii]," ",dist[ii]
    ENDIF

    IF binnum GE 0 AND binnum LT num_bins THEN nbin[binnum] = nbin[binnum] + 1
  ENDFOR

  IF NOT psflag THEN BEGIN
    set_plot,'X'
    setplotcolors
    setbgfg,!white,!black
    DEVICE, RETAIN=2, DECOMPOSED=0
    WINDOW,XSIZE=800,YSIZE=800
    !P.MULTI=[0,0,2,0,0]

    charsz = 3.0
    symsz = 1.5
    thick = 1.0
  ENDIF ELSE BEGIN
; We'll write .eps files
    set_plot,'PS'
    setplotcolors ; PS mode changes you to 256 colors
    xs = 6.5
    ys = 8.5
    xoff = 1.0
    yoff = 3.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
           bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,0,2,0,0]

    charsz = 1.5
    symsz = 1.2
    thick = 3.0
  ENDELSE

  hipassclr = !lgray
  singgclr = !dgreen
  sr2clr = !dcyan
  sr1clr = !dblue

  IF psflag THEN title='' ELSE title="HIPASS H!DI!N mass distribution"

; First, the HIPASS-vs-SINGG comparison
  plothist,lmhhipass,bin=binsize,/FILL,FCOLOR=hipassclr,HALFBIN=0,/NAN, $
;  PLOT,(xbin[0:num_bins-2]+0.5*binsize),nbin,PSYM=10, $
       XSTYLE=1,YSTYLE=1,XRANGE=[xmin,xmax],YRANGE=[0,800], $
       TITLE=title,COLOR=!black,CHARSIZE=charsz, $
       XTITLE="log(M!DH!II!N/M!Dsolar!N)", $
       YTITLE="Number / ("+STRMID(STRTRIM(STRING(binsize),2),0,3)+" dex)"
; Draw vertical lines
  plothist,fulllmh1,bin=binsize,/OVERPLOT,/FILL,FCOLOR=singgclr,HALFBIN=0,/NAN, $
           COLOR=!black
  FOR ii = 1,num_bins-2 DO BEGIN
    IF xbin[ii] GE xmin AND xbin[ii+1] LT xmax THEN $
      PLOTS,[xbin[ii+1],xbin[ii+1]],[0,MIN([nbin[ii],nbin[ii+1]])],COLOR=!black
  ENDFOR

;  IF psflag THEN BEGIN
;    psend,!outdir+'/masshist1.eps',/noprint,/clobber
;    set_plot,'PS'
;    xs = 6.5
;    ys = 4.5
;    xoff = 1.0
;    yoff = 3.0
;    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
;           bits_per_pixel=8,/encapsulated
;    !P.MULTI=[0,0,1,0,0]
;  ENDIF

; Next, the SR1-vs-SR2-vs-SINGG comparison.
  IF psflag THEN title='' ELSE title="SINGG H!DI!N mass distribution"

  plothist,fulllmh1,xhist,yhist,bin=binsize,/FILL,FCOLOR=singgclr,HALFBIN=0,/NAN, $
           XSTYLE=1,YSTYLE=1,XRANGE=[xmin,xmax],YRANGE=[0,42], $
           TITLE=title,COLOR=!black,CHARSIZE=charsz, $
           XTITLE="log(M!DH!II!N/M!Dsolar!N)", $
           YTITLE="Number / ("+STRMID(STRTRIM(STRING(binsize),2),0,3)+" dex)"
  plothist,lmh1,bin=binsize,/OVERPLOT,HALFBIN=0,/NAN,/FILL,FCOLOR=sr2clr,COLOR=!black
  plothist,sr1lmh1,bin=binsize,/OVERPLOT,HALFBIN=0,/NAN, $
           /FILL,FCOLOR=sr1clr,COLOR=!black

; Draw vertical lines
  FOR ii = 0,N_ELEMENTS(xhist)-2 DO BEGIN
    PLOTS,[(xhist[ii]+xhist[ii+1])/2.0,(xhist[ii]+xhist[ii+1])/2.0], $
          [0,MIN([yhist[ii],yhist[ii+1]])],COLOR=!black
  ENDFOR

  IF psflag THEN BEGIN
    psend,!outdir+'/masshist.eps',/noprint,/clobber
  ENDIF

END
