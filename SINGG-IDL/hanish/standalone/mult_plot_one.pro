PRO mult_plot_one,xval,yval,xrange,yrange,color,xtitle,ytitle,psflag, $
                  ERR=err,LOW=low,LABEL=label,DEBUG=debug

  IF psflag THEN BEGIN
    charsz = 1.5
    symsz = 1.2   
  ENDIF ELSE BEGIN
    charsz = 3.0
    symsz = 1.2
  ENDELSE

  IF psflag THEN clr = !black ELSE clr = color

  dummy = FLTARR(2)
  PLOT,dummy,dummy,XRANGE=xrange,YRANGE=yrange,XSTYLE=1,YSTYLE=1, $
       XTITLE=xtitle,YTITLE=ytitle,COLOR=!black,CHARSIZE=charsz,THICK=thick

  good = WHERE(ABS(xval/MAX(ABS(xval),/NAN)) GT 0.0001 AND ABS(yval/MAX(ABS(yval),/NAN)) GT 0.0001,count)
  IF count GT 0 THEN BEGIN
    PLOTS,xrange,dummy,COLOR=!black
    PLOTS,xval[good],yval[good],COLOR=clr,PSYM=SYM(1)

    IF KEYWORD_SET(err) THEN BEGIN
      sz = SIZE(err)
      IF sz[0] EQ 2 AND sz[2] EQ 2 THEN BEGIN
; We passed in asymmetric errors.
        ERRPLOT,xval[good],(yval[good]-err[good,0]),(yval[good]+err[good,1]),COLOR=clr
      ENDIF ELSE BEGIN
;forprint,yval,err
        ERRPLOT,xval[good],(yval[good]-err[good]),(yval[good]+err[good]),COLOR=clr
      ENDELSE
    ENDIF
  ENDIF

  IF KEYWORD_SET(low) THEN BEGIN
    good = WHERE(ABS(xval/MAX(ABS(xval),/NAN)) GT 0.0001 AND ABS(yval/MAX(ABS(yval),/NAN)) GT 0.0001 AND xval GT low,count)
  ENDIF

  grm_avsigclip,yval[good],3.0,100,val,sig,ngood,nbad,niter,/fixmean0
  IF KEYWORD_SET(debug) THEN BEGIN
    PRINT,'DEBUG!'
    PRINT,max(yval[good]),min(yval[good]),ngood,nbad
    PRINT,count
  ENDIF

  PRINT,xtitle,val,sig
;;  PLOTS,xrange,dummy+val,COLOR=!ddgray,LINESTYLE=0
  PLOTS,xrange,dummy+val+sig,COLOR=!ddgray,LINESTYLE=2
  PLOTS,xrange,dummy+val-sig,COLOR=!ddgray,LINESTYLE=2

  IF KEYWORD_SET(label) THEN BEGIN
    IF psflag THEN labelsz = charsz ELSE labelsz = charsz/2.0
    XYOUTS,(0.85*(xrange[1]-xrange[0])+xrange[0]), $
           (0.8*(yrange[1]-yrange[0])+yrange[0]), $
           STRTRIM(label,2),COLOR=!black,CHARSIZE=labelsz, $
           ALIGNMENT=0.5,CHARTHICK=1
  ENDIF

  RETURN
END
