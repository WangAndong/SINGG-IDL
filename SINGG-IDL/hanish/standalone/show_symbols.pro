PRO show_symbols

  DEVICE, RETAIN=2, DECOMPOSED=0
  WINDOW,XSIZE=800,YSIZE=1000
  !P.MULTI=[0,0,1,0,0]

  sz = 25

  dummy = FLTARR(2)
  setplotcolors
  PLOT,dummy,dummy,XRANGE=[0,25],YRANGE=[0,25],COLOR=!black

  FOR ii=0,23 DO BEGIN
    PLOTS,[ii,ii],1.0,PSYM=SYM(ii),COLOR=!black,SYMSIZE=3.0
    IF ii LT 9 THEN PLOTS,[ii,ii],2.0,PSYM=ii,COLOR=!ddgray,SYMSIZE=3.0
  ENDFOR

END
