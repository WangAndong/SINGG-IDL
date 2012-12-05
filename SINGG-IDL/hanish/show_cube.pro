PRO show_cube,cube

  sz = SIZE(cube)
  factor = 2
  xsize = sz[1]/factor
  ysize = sz[2]/factor
  n_img = sz[3]

  dummy = FLTARR(2)

  DEVICE, RETAIN=2, DECOMPOSED=0
  WINDOW,XSIZE=800,YSIZE=800
  !P.MULTI=[0,0,0,0,0]
;  !P.MULTI=[0,0,3,0,0]

  FOR im=0,n_img-1 DO BEGIN

    PLOT,dummy,dummy,XRANGE=[0,xsize-1],YRANGE=[0,ysize-1],$
         XSTYLE=1,YSTYLE=1
    mx = FLTARR(xsize)
    FOR ii=0,xsize-1 DO BEGIN
      FOR jj=0,ysize-1 DO BEGIN
        xdum = make_array(2,val=ii)
        ydum = make_array(2,val=jj)
        clr = 255.0-(cube[ii*factor,jj*factor,im]/50.0)
        IF clr LT 0 THEN clr = 0
        OPLOT,xdum,ydum,COLOR=clr
      ENDFOR
;      mx[ii] = MAX(cube[ii,*,im])
    ENDFOR
    row = INDGEN(xsize)
;    PLOT,row,mx,COLOR=1,CHARSIZE=2.0
    PRINT,"press a key"
    key=GET_KBRD(1)
  ENDFOR

END