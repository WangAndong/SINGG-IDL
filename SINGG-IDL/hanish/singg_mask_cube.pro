FUNCTION singg_mask_cube,incube,sky,skysig,scale,maskfile,saturation,sat_mask
; Creates a mask cube.  1b indicates a GOOD pixel, 0b indicates BAD.
; INPUTS
; incube      X by Y by N weighted data cube, where X and Y are imsize
;             and N is the number of images used
; sky         Array of sky values
; skysig      Array of sky uncertainty values
; scale       Array of weighting factors
; maskfile    Names of basic mask files, containing 0s where pixels
;             should not be used due to edge effects or bad columns.
; saturation  The pixel value corresponding to a saturated pixel.  In general
;             we'll set this below both the data maximum (65536) and the well
;             capacity, although the SDflat correction can skew this a bit.
; OUTPUTS
; (function)  Output mask cube.  1b is a GOOD pixel, 0b is BAD pixel.
; sat_mask    Saturation pixel mask, showing 1b where the pixel was masked 
;             solely due to saturation and 0b everywhere else

  sz = SIZE(incube)
  n_files = sz[3]

  IF sz[0] GT 3 or sz[0] LT 2 THEN BEGIN
    PRINT,"ERROR in singg_mask_cube: input image cube is not 3-dimensional"
    PRINT,sz
    RETURN,incube
  ENDIF
  IF sz[0] EQ 2 THEN BEGIN
; There must have only been one image, so IDL automatically concatenated down
; to a 2D array.  We still want to mask it and such.
    n_files = 1
  ENDIF

  ii = LONG(0)
  nsig = 5.0

; To know whether something is saturated, we want to make sure the number of
; saturated images equals the number of nonzero images.  So, set temp_sat_mask
; and count down; if it's zero at the end, mask it.
  sum_mask = INTARR(sz[1],sz[2])
  temp_sat_mask = INTARR(sz[1],sz[2])

; The starting mask is that given by the flats, with edge effects and bad
; columns already masked.

  mask_cube = BYTARR(sz[1],sz[2],n_files)

  FOR kk=0,n_files-1 DO BEGIN
    IF FILE_TEST(maskfile[kk]) THEN maskimg = readfits(maskfile[kk],/SILENT) $
                               ELSE maskimg = readfits(maskfile[kk]+'.gz',/SILENT)
    mask_cube[*,*,kk] = (maskimg GT 0.5)
    sum_mask = sum_mask + LONG(maskimg GT 0.5)

; That is, at this point sum_mask is an Npix map, excluding edges, bad
; pixels, and buffer areas.

; Now, begin the masking steps.

; Remove zero or negative pixels, if there are still any left.
; Negative pixels may be okay in a sky-subtracted or Rsub image, but we're only
; using unscaled, non-subtracted object images here, so any negative count is
; an error or a cosmic ray.

    zero = WHERE ((incube[*,*,kk] LT -MIN([sky[kk],(nsig*skysig[kk])])*scale[kk]) AND $
;                  (incube[*,*,kk] GT (-0.0001-sky[kk])*scale[kk]) AND $
                   mask_cube[*,*,kk],count)
    IF count GT 0 THEN BEGIN
      zY = UINT(zero/sz[1])
      zX = zero - sz[1]*zY
      FOR ii = LONG(0),LONG(count-1) DO mask_cube[zX[ii],zY[ii],kk] = 0b
    ENDIF

; Remove saturated pixels
    saturated = WHERE(incube[*,*,kk] GT (saturation-sky[kk])*scale[kk] $
                      AND mask_cube[*,*,kk], count)
    IF count GT 0 THEN BEGIN
      satY = UINT(saturated/sz[1])
      satX = saturated - sz[1]*satY
      FOR ii = LONG(0),LONG(count-1) DO BEGIN
        mask_cube[satX[ii],satY[ii],kk] = 0b
        temp_sat_mask[satX[ii],satY[ii]] = $
             temp_sat_mask[satX[ii],satY[ii]] + 1
      ENDFOR
    ENDIF

  ENDFOR

; temp_sat_mask is zero at this point if every non-masked pixel was saturated.
; If it was masked in every image before this step (edges, for example), don't
; fill it in.  If there are any images where it wasn't saturated, don't fill.
  sat_mask = (sum_mask GT 0) AND (temp_sat_mask EQ sum_mask)
;  sat_mask = (temp_sat_mask EQ 0)

; Note that if the edges are NOT masked correctly by the maskfile, the stars
; around the edges will not have their saturated areas filled.

  RETURN,mask_cube
END
