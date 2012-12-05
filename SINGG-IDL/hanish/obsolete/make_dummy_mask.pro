PRO make_dummy_mask
; Creates basic mask for use in my modified imregister script.
; Basically, it makes a 2-pixel-wide band around the edge, and covers the bad
; columns (0:508,112:113)
  fili = "basic_mask.fits"

  fits_read,fili,img,hd

  sz = SIZE(img)
  img = FLTARR(sz[1],sz[2])

  buff=3

; fill everything but the outermost two pixels on each size to 1s
  img[buff:(sz[1]-buff-1),buff:(sz[2]-buff-1)] = 1.0

; Cover the bad column.  This assumes you're already transposed.
;;  img[0:(sz[1]-1),109:114] = 0.0 ; later runs
;;  img[0:(sz[1]-1),8:15] = 0.0 ; Run 1 only
; we need the extra because when you imregister the bad column it gets worse
; If you hadn't transposed:
  img[109:114,0:(sz[2]-1)] = 0.0

; Add any special masks here:
; NORMAL
  filo = "basic_mask.fits"

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
