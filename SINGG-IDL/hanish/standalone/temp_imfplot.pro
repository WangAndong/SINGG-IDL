PRO temp_imfplot,PS=ps
  psflag = KEYWORD_SET(ps)

; Old Salpeter (1955):
  mS = [0.1,100.0]
  bS = [-2.35]
  aS = [0.172]
  fS = [1.0]

  S_sym = 4
  S_line = 0

; Old Scalo (1986)
  mC = [0.1,0.18,0.42,0.62,1.18,3.5,120.0]
  bC = [1.6,-1.01,-2.75,-2.08,-3.5,-2.63]
  aC = [103.0,1.17,0.26,0.36,0.45,0.15]
  fC = [0.029,0.285,0.168,0.233,0.189,0.098]

  C_sym = 4
  C_line = 2

; Old Kroupa (1991)
  mK = [0.01,0.08,0.5,100.0]
  bK = [-0.3,-1.3,-2.3]
  aK = [5.28,0.42,0.21]
  fK = [0.041,0.269,0.690]

  K_sym = 4
  K_line = 1

  n = 1

  setplotcolors

  IF NOT psflag THEN BEGIN
    set_plot,'X'
    setbgfg,!white,!black
    DEVICE, RETAIN=2, DECOMPOSED=0
    WINDOW,XSIZE=800,YSIZE=600
    !P.MULTI=[0,0,1,0,0]

    charsz = 3.0
  ENDIF ELSE BEGIN
; We'll write .eps files
    set_plot,'PS'
    xs = 6.5
    ys = 0.75*xs
    xoff = 1.2
    yoff = 3.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
           bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,0,1,0,0]

    charsz = 1.5
  ENDELSE

  mass = FLTARR(301)
  FOR ii = 0,300 DO mass[ii] = 10.0^(0.01*FLOAT(ii) - 1.0)

  ybin = FLTARR(301)
  num_bins = N_ELEMENTS(mS)-1
  FOR ii = 0,300 DO BEGIN
    index=0
    FOR jj = 0,num_bins-1 DO BEGIN
      IF mS[jj] LT mass[ii] AND mS[jj+1] GE mass[ii] THEN index=jj
    ENDFOR
    
    ybin[ii] = aS[index] * mass[ii]^(bS[index]+n)
  ENDFOR

  PLOT,ALOG10(mass),ALOG10(ybin),XRANGE=[-1.0,2.0],YRANGE=[-4.0,1.0], $
       LINESTYLE=S_line,CHARSIZE=2.0,XTITLE="log!D10!N(M/M!Dsolar!N)",COLOR=1, $
       YTITLE="log!D10!N(dN/d(ln(M/M!Dsolar!N)))"

  ybin = FLTARR(301)
  num_bins = N_ELEMENTS(mC)-1
  FOR ii = 0,300 DO BEGIN
    index=0
    FOR jj = 0,num_bins-1 DO BEGIN
      IF mC[jj] LT mass[ii] AND mC[jj+1] GE mass[ii] THEN index=jj
    ENDFOR
    
    ybin[ii] = aC[index] * mass[ii]^(bC[index]+n)
  ENDFOR

  OPLOT,ALOG10(mass),ALOG10(ybin),COLOR=1,LINESTYLE=C_line

  ybin = FLTARR(301)
  num_bins = N_ELEMENTS(mK)-1
  FOR ii = 0,300 DO BEGIN
    index=0
    FOR jj = 0,num_bins-1 DO BEGIN
      IF mK[jj] LT mass[ii] AND mK[jj+1] GE mass[ii] THEN index=jj
    ENDFOR
    
    ybin[ii] = aK[index] * mass[ii]^(bK[index]+n)
  ENDFOR

  OPLOT,ALOG10(mass),ALOG10(ybin),COLOR=1,LINESTYLE=K_line

  IF psflag THEN BEGIN
    !p.multi   = 0
    !p.noerase = 0
    psend,!outdir+"imf.eps",/noprint,/clobber
  ENDIF

END
