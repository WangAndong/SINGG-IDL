PRO make_09m_mask
; Creates basic mask for use in my modified imregister script.
; Basically, it makes a 2-pixel-wide band around the edge, and covers the bad
; columns (0:508,112:113)
  fili = "basic_09m_mask.fits"

  fits_read,fili,img,hd,/header_only

  img = FLTARR(2048,2046)
  sz = SIZE(img)

  buff=3

; fill everything but the outermost two pixels on each size to 1s
  img[buff:(sz[1]-buff-1),buff:(sz[2]-buff-1)] = 1.0

; Cover the bad columns.  This assumes you're already flipped.
  img[70:110,2031:2045] = 0.0
  img[83:99,1022:2045] = 0.0
  img[85:94,0:1021] = 0.0
  img[1500,0:1166] = 0.0
  img[1512,0:1145] = 0.0
  img[1229,1022:2045] = 0.0
  img[1774,1022:2045] = 0.0
  img[202,1700:2045] = 0.0
  img[219,1600:2045] = 0.0

; we need the extra because when you imregister the bad column it gets worse

; Add any special masks here:
; NORMAL
  filo = "basic_09m_mask.fits"

; TOP
;  filo = "mask_top.fits"
;  img[0:(sz[1]-1),(sz[2]-50):(sz[2]-1)] = 0.0

; RIGHT
;  filo = "mask_right.fits"
;  img[(sz[1]-50):(sz[1]-1),0:(sz[2]-1)] = 0.0

; BOTTOM
;  filo = "mask_bottom.fits"
;  img[0:(sz[1]-1),0:20] = 0.0

  fits_write,filo,img,hd

END
