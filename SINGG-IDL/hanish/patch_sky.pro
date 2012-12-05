FUNCTION patch_sky,img,mask,MODE=mode,RADIUS=radius,EDGE=edge,SKY=sky,BUFFER=buffer
; Given an image and a mask, replace any masked pixels with the sky
; level of the unmasked area, plus or minus the uncertainty.
; INPUTS
;   img(X,Y)        Input image
;   mask(X,Y)       Mask to be used
; OPTIONAL INPUT
;   mode            Sky mode:
;                     0: uses one overall sky level
;                     1: uses a 21x21 box around each masked pixel
;                     2: calculates a sky for each half
;                     3: calculates a sky for each quadrant
;
;                     the image into two sections with different sky
;                     values, and 2+ splits into four quadrants.
;   radius          In mode 1, the +/- distance to average.  Default=10
;   sky             If it's a single element, use instead of skyval.
;                   If it's a 2-element array, replace skysig as well.
; OUTPUT
;   (function)      Image, modified.  Don't want to overwrite inputs.

  IF NOT KEYWORD_SET(mode) THEN mode = 0
  img2 = img
  sz = SIZE(img)
  xdiv = LONG(sz[1]/2)
  ydiv = LONG(sz[2]/2)

  buff_mask = calc_outer_mask(mask,10)

  badind = WHERE(mask EQ 1b AND buff_mask EQ 0b,badcount)
;;  badind = WHERE((mask EQ 1b OR img LT -1000 OR img GT 59000) AND buff_mask EQ 0b,badcount)
  IF badcount EQ 0 THEN BEGIN
    PRINT,'WARNING in patch_sky: no pixels are masked.'
    RETURN,img
  ENDIF

  y = FIX(badind/sz[1])
  x = badind - y*sz[1]
  
; In modes 0, 2, and 3, we actually only want to draw sky values from
; a band between 50 and 150 pixels from each edge, since they're far
; less likely to include any galaxies.

  IF KEYWORD_SET(sky) THEN BEGIN
    skyval = sky[0]
    IF N_ELEMENTS(sky) GT 1 THEN skysig = sky[1] ELSE skysig = 0.0
  ENDIF ELSE BEGIN
    IF NOT KEYWORD_SET(edge) THEN edge = 50
    band = 100
    sky_mask = MAKE_ARRAY(sz[1],sz[2],/BYTE,VALUE=1b)
    sky_mask[edge:(sz[1]-1)-edge,$
             edge:(sz[2]-1)-edge] = 0b
    sky_mask[(edge+band):(sz[1]-1)-(edge+band),$
             (edge+band):(sz[2]-1)-(edge+band)] = 1b
    sky_ind = WHERE(NOT sky_mask AND NOT mask)
    mysky,img,skyval,skysig,mask=(mask OR sky_mask),/SILENT
;; sky_ind isn't being used in modes 2-3
  ENDELSE

  randb = RANDOMN(seed,badcount,/DOUBLE)

  IF NOT KEYWORD_SET(radius) THEN radius = 10 ELSE radius = LONG(radius)

; Patch the "buffer" area first.  This is ALWAYS set to the general sky
; level, although possibly with noise.
  buffind = WHERE(mask EQ 1b AND buff_mask EQ 1b,buffcount)
  IF buffcount GT 0 THEN BEGIN
    randbuff = RANDOMN(seed,buffcount,/DOUBLE)
    img2[buffind] = skyval + skysig*randbuff
  ENDIF

  CASE mode OF
  0: BEGIN
; Note: if the image has some sort of gradient, not only will this
; method give a bad value, but mysky tends to freak out.  In those
; cases, the other modes are a bit better.
       img2[badind] = skyval + skysig*randb
     END
  1: BEGIN
; A 21x21 boxing is used to fix each point dynamically.  Generally
; speaking, this method is used by the hump correction logic.  Note
; that it only works if there are no really large blocks of masked
; pixels, so you shouldn't use this one on partial/aperture-masked images.
       FOR ii = LONG(0),LONG(badcount)-1 DO BEGIN
; Find good pixels within the given radius (default 10) of the current spot.

         xmin = (x[ii]-radius > 0)
         xmax = (x[ii]+radius < (sz[1]-1))
         ymin = (y[ii]-radius > 0)
         ymax = (y[ii]+radius < (sz[2]-1))

         chunkmask = WHERE(mask[xmin:xmax,ymin:ymax] EQ 0b,count)
         IF count GE 2 THEN BEGIN
; Use a local value for sky
           chunkimg = img[xmin:xmax,ymin:ymax]
;           mmm2,chunkimg[chunkmask],skyval2,skysig2
           grm_avsigclip,chunkimg[chunkmask],5.0,20,skyval2,skysig2,nuse,nrej,nit
;; should use skysig instead of skysig2, because the local "variation"
;; might be just due to a natural gradient or star, and not actual randomness.
           img2[x[ii],y[ii]] = skyval2 + skysig*randb[ii]
         ENDIF ELSE BEGIN
; This pixel, and all around it, are masked.
           img2[x[ii],y[ii]] = skyval + skysig*randb[ii]
         ENDELSE
       ENDFOR
     END
  2: BEGIN
       mysky,img[0:xdiv-1,0:sz[2]-1],skyval1,skysig1,mask=mask[0:xdiv-1,0:sz[2]-1],/SILENT
       mysky,img[xdiv:sz[1]-1,0:sz[2]-1],skyval2,skysig2,mask=mask[xdiv:sz[1]-1,0:sz[2]-1],/SILENT
       FOR ii = LONG(0),LONG(badcount)-1 DO BEGIN
         IF x[ii] LT xdiv THEN BEGIN
           skyval = skyval1
           skysig = skysig1
         ENDIF ELSE BEGIN
           skyval = skyval2
           skysig = skysig2
         ENDELSE
         img2[x[ii],y[ii]] = skyval + skysig*randb[ii]
       ENDFOR
     END
  3: BEGIN
       mysky,img[0:xdiv-1,0:ydiv-1],skyval1,skysig1,mask=mask[0:xdiv-1,0:ydiv-1],/SILENT
       mysky,img[0:xdiv-1,ydiv:sz[2]-1],skyval2,skysig2,mask=mask[0:xdiv-1,ydiv:sz[2]-1],/SILENT
       mysky,img[xdiv:sz[1]-1,0:ydiv-1],skyval3,skysig3,mask=mask[xdiv:sz[1]-1,0:ydiv-1],/SILENT
       mysky,img[xdiv:sz[1]-1,ydiv:sz[2]-1],skyval4,skysig4,mask=mask[xdiv:sz[1]-1,ydiv:sz[2]-1],/SILENT
       FOR ii = LONG(0),LONG(badcount)-1 DO BEGIN
         IF x[ii] LT xdiv THEN BEGIN
           IF y[ii] LT ydiv THEN BEGIN
             skyval = skyval1
             skysig = skysig1
           ENDIF ELSE BEGIN
             skyval = skyval2
             skysig = skysig2
           ENDELSE
         ENDIF ELSE BEGIN
           IF y[ii] LT ydiv THEN BEGIN
             skyval = skyval3
             skysig = skysig3
           ENDIF ELSE BEGIN
             skyval = skyval4
             skysig = skysig4
           ENDELSE
         ENDELSE
         img2[x[ii],y[ii]] = skyval + skysig*randb[ii]
       ENDFOR
     END
  ELSE: BEGIN
     END
  ENDCASE

  RETURN,img2
END
