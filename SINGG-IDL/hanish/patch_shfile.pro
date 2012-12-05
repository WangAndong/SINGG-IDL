PRO patch_shfile,infile,EDGE=edge

; Patches the edge area of each .sh file.
  IF NOT KEYWORD_SET(edge) THEN edge=10

  fits_read,infile,img,hdr
  sky = SXPAR(hdr,'SKYLEV')
  sz = SIZE(img)

  k = WHERE(img GT 0.0001)
  y = FIX(k/sz[1])
  x = k - y*sz[1]

  minX = MIN(x) + edge
  maxX = MAX(x) - edge
  minY = MIN(y) + edge
  maxY = MAX(y) - edge
  
  img2 = FLTARR(sz[1],sz[2]) + sky

  img2[minX:maxX,minY:maxY] = img[minX:maxX,minY:maxY]

  fits_write,infile,img2,hdr

  RETURN

END
