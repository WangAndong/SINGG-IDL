PRO show_mask,mask
; Show mask, with black squares corresponding to mask=1b 
  DEVICE, RETAIN=2, DECOMPOSED=0
  WINDOW,XSIZE=800,YSIZE=800
  !P.MULTI=[0,0,1,0,0]

  sz = SIZE(mask)
  IF sz[0] NE 2 THEN PRINT,"ERROR, show_mask can only do 2d arrays"
  PRINT,"Displaying mask of size ",sz[1]," by ",sz[2]

  good = WHERE(mask,goodcount)
  PRINT,"good=",goodcount

  goodx = INTARR(goodcount)
  goody = INTARR(goodcount)

  count = LONG(0)
  FOR ii = 0,sz[1]-1 DO BEGIN
    FOR jj = 0,sz[2]-1 DO BEGIN
      IF mask[ii,jj] THEN BEGIN
        goodx[count] = ii
        goody[count] = jj
        count = count + 1
      ENDIF
;      PRINT,ii,jj,mask[ii,jj],count
    ENDFOR
  ENDFOR

  PLOT,goodx,goody,XRANGE=[0,sz[1]],YRANGE=[0,sz[2]],PSYM=4,SYMSIZE=0.1,COLOR=100

  RETURN
END
