PRO plot_fluxcurve,binA,skysig,ratio,bwflag,as_pix, $
                   FI,FB,FoutI,FoutB,Funits,Fscale, $
                   a_f,a_s,Title,SB=sb,CNTSIGI=cntsigI,CNTSIGB=cntsigB
; INPUTS:
; binA       Semimajor axis for each bin, in arcsec
; skysig     Uncertainty in sky level
; ratio      =a/b, the axial ratio of the ellipse
; bwflag     switch to black and white
; as_pix     Arcsec per pixel
; FI         Total flux contained in each bin, centered on the isophote-center
; FB         Total flux contained in each bin, centered on the brightness peak
; FoutI[3]   Flux outside the apertures[value, sigsky, sigcnt]
; FoutB[3]   Flux outside the apertures[value, sigsky, sigcnt]
; Funits     Units of flux array
; Fscale     Conversion factor into Funits
; a_f        Estimated location of the edge of the galaxy, in arcsec
; a_s        Estimated location of sky radius, in arcsec
; Title      Plot title
; OPTIONAL INPUTS:
; /SB        It's a surface brightness plot, not flux
; cntsigI    Used by net images only, these are R-band flux*timeratio*ecntrat
; cntsigB      for both centerpoints

  minval = 0.0
  sbflag = KEYWORD_SET(SB)

; Initialize colors
  IF bwflag THEN BEGIN
    FI_clr = 100
    FIerr_clr = 100
    FB_clr = 200
    FBerr_clr = 200
  ENDIF ELSE BEGIN
    FI_clr = !red
    FIerr_clr = !dred
    FB_clr = !blue
    FBerr_clr = !dblue
  ENDELSE

  num_bins = N_ELEMENTS(binA)

  FIHi = FLTARR(num_bins)
  FILo = FLTARR(num_bins)
  FBHi = FLTARR(num_bins)
  FBLo = FLTARR(num_bins)
  FluxSkyErr = FLTARR(num_bins)

  FOR ii = 0,num_bins-1 DO BEGIN
    IF (NOT sbflag OR ii EQ 0) THEN BEGIN
      FluxSkyErr[ii] = skysig*!pi*(binA[ii]^2)/ratio
    ENDIF ELSE BEGIN
      FluxSkyErr[ii] = skysig*!pi*(binA[ii]^2-binA[ii-1]^2)/ratio
    ENDELSE
  ENDFOR

  ymax=MAX([FI,FB]) * 2.0 * Fscale

  Amin = binA[0]
  Amax = binA[num_bins-1]

  dummy = FLTARR(2)
  thick = 1.0
  charsz = 2.0
;  thick = 2.0
;  charsz = 3.0

  IF sbflag THEN ymin = skysig*Fscale*(as_pix)^2 $ ; was ymax / 1000.0
            ELSE ymin = ymax / 100.0 ; was SQRT(ymax)

  IF NOT sbflag THEN BEGIN
    PLOT,dummy,dummy,XRANGE=[Amin,Amax],YRANGE=[ymin,ymax],/YLOG, $
         XSTYLE=1,YSTYLE=1,COLOR=1,CHARSIZE=charsz,THICK=thick, $
         XTITLE='Semi-major axis [arcsec]',YTITLE='Flux ['+Funits+']', $
         TITLE=title,CHARTHICK=thick
  ENDIF ELSE BEGIN
    PLOT,dummy,dummy,XRANGE=[Amin,Amax],YRANGE=[ymin,ymax],/YLOG, $
         XSTYLE=1,YSTYLE=1,COLOR=1,CHARSIZE=charsz,THICK=thick, $
         XTITLE='Semi-major axis [arcsec]',YTITLE='Surf Bright ['+Funits+'/arcsec^2]', $
         TITLE='Net Image Surface Brightness',CHARTHICK=thick
  ENDELSE

; Plot the isophote image curves...
  PLOTS,binA,(FI*Fscale),THICK=2.0,COLOR=FI_clr,LINESTYLE=0, $
             CLIP=[Amin,ymin,Amax,ymax],NOCLIP=0
  PLOTS,binA,((FI+FluxSkyErr)*Fscale),THICK=1.0,COLOR=FIerr_clr,LINESTYLE=1, $
             CLIP=[Amin,ymin,Amax,ymax],NOCLIP=0
  PLOTS,binA,((FI-FluxSkyErr)*Fscale),THICK=1.0,COLOR=FIerr_clr,LINESTYLE=1, $
             CLIP=[Amin,ymin,Amax,ymax],NOCLIP=0
  IF KEYWORD_SET(cntsigI) THEN BEGIN
    PLOTS,binA,((FI+cntsigI)*Fscale),THICK=1.0,COLOR=FIerr_clr,LINESTYLE=2, $
               CLIP=[Amin,ymin,Amax,ymax],NOCLIP=0
    PLOTS,binA,((FI-cntsigI)*Fscale),THICK=1.0,COLOR=FIerr_clr,LINESTYLE=2, $
               CLIP=[Amin,ymin,Amax,ymax],NOCLIP=0
  ENDIF

; ...and the brightness peak curves.
  PLOTS,binA,(FB*Fscale),THICK=2.0,COLOR=FB_clr,LINESTYLE=0, $
             CLIP=[Amin,ymin,Amax,ymax],NOCLIP=0
  PLOTS,binA,((FB+FluxSkyErr)*Fscale),THICK=1.0,COLOR=FBerr_clr,LINESTYLE=1, $
             CLIP=[Amin,ymin,Amax,ymax],NOCLIP=0
  PLOTS,binA,((FB-FluxSkyErr)*Fscale),THICK=1.0,COLOR=FBerr_clr,LINESTYLE=1, $
             CLIP=[Amin,ymin,Amax,ymax],NOCLIP=0
  IF KEYWORD_SET(cntsigB) THEN BEGIN
    PLOTS,binA,((FB+cntsigB)*Fscale),THICK=1.0,COLOR=FBerr_clr,LINESTYLE=2, $
               CLIP=[Amin,ymin,Amax,ymax],NOCLIP=0
    PLOTS,binA,((FB-cntsigB)*Fscale),THICK=1.0,COLOR=FBerr_clr,LINESTYLE=2, $
               CLIP=[Amin,ymin,Amax,ymax],NOCLIP=0
  ENDIF

  Flux_fI = SPLINE(binA,FI,a_f)
  Flux_fB = SPLINE(binA,FB,a_f)

  PLOTS,[a_f,a_f,Amin],[ymin,Flux_fI*Fscale,Flux_fI*Fscale],COLOR=1, $
        CLIP=[Amin,ymin,Amax,ymax],NOCLIP=0
  PLOTS,[a_s,a_s],[ymin,ymax],COLOR=1,LINESTYLE=1, $
        CLIP=[Amin,ymin,Amax,ymax],NOCLIP=0

  IF sbflag THEN RETURN

; Now, given the radius used so far, calculate FWHM and such.

; Our "Fout" array is [value,uncertainty]

; Find half-light radius in the isophote image
  halflight,FI,FluxSkyErr,binA,a_f, AIhalf,AIerr,Fout=[FoutI[0],FoutI[1]]
  PLOTS,[AIhalf,AIhalf],[ymin,((Flux_fI+FoutI[0])*Fscale/2.0)],COLOR=FI_clr, $
        CLIP=[Amin,ymin,Amax,ymax],NOCLIP=0

  IF KEYWORD_SET(cntsigI) THEN BEGIN
    halflight,FI,cntsigI,binA,a_f, AIhalf,AIerr,Fout=[FoutI[0],FoutI[2]]
    PLOTS,[AIhalf,AIhalf],[ymin,((Flux_fI+FoutI[0])*Fscale/2.0)],COLOR=FI_clr, $
          CLIP=[Amin,ymin,Amax,ymax],NOCLIP=0
  ENDIF

; Find half-light radius in the brightness image
  halflight,FB,FluxSkyErr,binA,a_f, ABhalf,ABerr,Fout=[FoutB[0],FoutB[1]]
  PLOTS,[ABhalf,ABhalf],[ymin,((Flux_fB+FoutB[0])*Fscale/2.0)],COLOR=FB_clr, $
        CLIP=[Amin,ymin,Amax,ymax],NOCLIP=0

  IF KEYWORD_SET(cntsigB) THEN BEGIN
    halflight,FB,cntsigB,binA,a_f, ABhalf,ABerr,Fout=[FoutB[0],FoutB[2]]
    PLOTS,[ABhalf,ABhalf],[ymin,((Flux_fB+FoutB[0])*Fscale/2.0)],COLOR=FB_clr, $
          CLIP=[Amin,ymin,Amax,ymax],NOCLIP=0
  ENDIF

  RETURN

END
