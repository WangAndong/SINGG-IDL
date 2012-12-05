PRO grow_mask, inmask, outmask, grow, goodval=goodval, badval=badval
   ;
   ; make a mask larger by number of pixels=grow around each "good" pixel 
   ; that is already masked.
   ;
   ; G. Meurer  ~2004
   ; G. Meurer 5/2010  fixed round off error issue.
   ;
   ; currently grows just the goodvals.
   IF KEYWORD_SET(goodval) AND KEYWORD_SET(badval) THEN BEGIN 
      IF goodval EQ badval THEN BEGIN 
         message, 'GOODVAL = BADVAL. How can I mask anything?', /CONTINUE
         return 
      ENDIF 
   ENDIF 

   IF (NOT KEYWORD_SET(goodval)) THEN goodval = 0b
   IF (NOT KEYWORD_SET(badval))  THEN badval  = 1b - goodval

   outmask = 0b*inmask + badval

   IF (grow GE 1) THEN BEGIN
;
; Add buffer by convolving map of good pixels with a circular top hat.
; (Thanks to J. McCann for the convolution idea).
;
      ksize            = 2*fix(grow+0.5) + 1
      dcol             = (lindgen(ksize*ksize) MOD ksize) - fix(ksize/2)
      drow             = lindgen(ksize*ksize) / ksize - fix(ksize/2)
      rkern            = sqrt(float(dcol*dcol + drow*drow))
      kuse             = where(rkern LE grow)
      fkern            = make_array(ksize, ksize, /FLOAT, value=0.0)
      fkern[kuse]      = 1.0
      fmask            = float(inmask)
      fcmask           = convolve(fmask, fkern)
      icmask           = fix(fcmask+0.5)             ; take nearest integetr to fix round off 5/2010
      goodpix          = where(icmask GE 1, ngood)
      IF ngood GT 0 THEN outmask[goodpix] = goodval
      ;stop
   ENDIF 
END 
