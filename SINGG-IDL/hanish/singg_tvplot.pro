PRO singg_tvplot,Rimg,Nimg,Simg,Rsky,Nsky,Ssky,bxw,cntrat, $
                 MASK=mask,POSITION=position,NOCLEAR=noclear,PLOTSIG=plotsig, $
                 LINEAR=linear,OUTPUT=output
; Creates a 3-color TV plot using the three input files.
; INPUTS:
;   Rimg[x,y]     R-band input image
;   Nimg[x,y]     Narrow-band (not subtracted) input image)
;   Rsky[2]       R-band image's sky value and uncertainty
;   Nsky[2]       Narrow-band image's sky value and uncertainty
;   Ssky[2]       Net image's sky value and uncertainty
;   bxw           Image compression ratio (usually 2:1)
;   cntrat        Continuum scaling ratio
; OPTIONAL INPUTS:
;   mask[x,y]     Bad-pixel mask to override the normal plotting
;   position[4]   [xmin,xmax,ymin,ymax] from 0.0 to 1.0
;   /noclear      Do not create a new window (clearing any old plots)
;   plotsig[2]    Number of sigmas to use as plot range
;   /linear       Plot on a linear scale instead of the default SQRT.
; (optional) OUTPUTS:
;   output[3,x,y] Actual array to be displayed, so that we don't need
;                   to do the whole thing again.

  sz = SIZE(Rimg)

  IF NOT KEYWORD_SET(mask) THEN mask = BYTARR(sz[1],sz[2])
  IF NOT KEYWORD_SET(plotsig) THEN plotsig = [-2.0,10.0]
  maxsig = [-999.0,999.0]

  output = DBLARR(3,sz[1],sz[2])

; R is blue
  Rplot = (Rimg-Rsky[0])*FLOAT(1b-mask)
  output[2,*,*] = maxsig[0] > (Rplot/Rsky[1]) < maxsig[1]
; N is green
  Nplot = (Nimg-Nsky[0])*FLOAT(1b-mask)
  output[1,*,*] = maxsig[0] > (Nplot/(1.5*Rsky[1]*cntrat)) < maxsig[1]
; S is red
  Splot = (Simg-Ssky[0])*FLOAT(1b-mask)
  output[0,*,*] = maxsig[0] > (Splot/(0.75*Rsky[1]*cntrat)) < maxsig[1]

; Just to make sure, load the color table.
  setplotcolors

  num_x = FIX(sz[1]/bxw)
  num_y = FIX(sz[2]/bxw)

  IF NOT KEYWORD_SET(position) THEN position = [0.0,0.0,1.0,1.0]
  max_x = FIX(num_x / (position[2]-position[0]))
  max_y = FIX(num_y / (position[3]-position[1]))

  IF NOT KEYWORD_SET(noclear) THEN BEGIN
    set_plot,'X'
    setbgfg,!white,!black
    DEVICE, RETAIN=2, DECOMPOSED=0
    WINDOW,XSIZE=max_x,YSIZE=max_y
    !P.MULTI=[0,0,0,0,0]
  ENDIF

  IF KEYWORD_SET(linear) THEN BEGIN
    tvscale,output,POSITION=position,MAXVALUE=plotsig[1],MINVALUE=plotsig[0], $
            BOTTOM=0,TOP=!ngrays-1
  ENDIF ELSE BEGIN
    tvscale,SSQRT(output),POSITION=position, $
            MAXVALUE=SSQRT(plotsig[1]),MINVALUE=SSQRT(plotsig[0]), $
            BOTTOM=0,TOP=!ngrays-1
  ENDELSE

  RETURN
END
