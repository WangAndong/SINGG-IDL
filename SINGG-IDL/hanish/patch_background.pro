PRO patch_background,infile,thresh
; Any value below the threshold will be reset to skylev plus a random
; variation for skysig, to reduce structure without changing other
; algorithms

  r = 1400

  fits_read,infile,img_in,hd

  sz = SIZE(img_in)

  loc = WHERE(img_in LT thresh AND img_in GT 0.0000001,nloc)

  IF nloc GT 1 THEN BEGIN
    img = img_in
    y = LONG(loc/sz[1])
    x = loc - y*sz[1]

    skyval = img[x,y]
    skymean = MEAN(skyval)
    skysig = STDEV(skyval)

    val = RANDOMN(seed,nloc,/double)

    PRINT,"Cleaning up background structure"
    FOR ii = LONG(0),LONG(nloc)-1 DO BEGIN
      img[x[ii],y[ii]] = skymean + val[ii]*skysig
    ENDFOR

    cen_X = sz[1]/2
    cen_Y = sz[2]/2

    PRINT,"Cleaning up edge structure"
    FOR ii = 0,sz[1]-1 DO BEGIN
      FOR jj = 0,sz[2]-1 DO BEGIN
        IF SQRT((ii-cen_X)^2 + (jj-cen_Y)^2) GT r THEN $
          img[ii,jj] = skymean + RANDOMN(seed,1,/double)*skysig
      ENDFOR
    ENDFOR

  ENDIF

  fits_write,infile,img,hd

END

