PRO test_mask
  wd    = '~/SUNGG/OUPIPE/J0249-02_play/'
  fcnt  = 'J0249-02_R_ss.fits'
  fha   = 'J0249-02_Rsub_ss.fits'
  fmsk1 = 'J0249-02_mask.fits'
  fmsk2 = 'J0249-02_Rsub_mask.fits'
  fmsk  = [fmsk1, fmsk2]
  prog  = 'TEST_MASK: '
  ;
  ; * read fits images
  fits_read, fcnt, imc, hdc
  fits_read, fha, imh, hdh
  ;
  ; get image size
  szc   = size(imc)
  szh   = size(imh)
  IF (szc[1] NE szh[1]) OR (szc[2] NE szh[2]) THEN stop, prog+'****ERROR: Image sizes do not match '
  nx0   = szc[1]
  ny0   = szc[2]
  ;
  ; * OR masks
  nm    = n_elements(fmsk)
  mask  = make_array(nx0, ny0, /byte, value=0b)
  FOR ii = 0, nm-1 DO BEGIN 
     fits_read, fmsk[ii], imsk, hmsk
     jj  = where(imsk GE 1b, njj)
     IF njj GT 0 THEN mask[jj] = 1b
  ENDFOR
  stop
  ;
  ; * bin images
  ;
  ; * make 3 color image
  ;   R = ha, G = R, B = R
  ;   set masked pixels to gray 125,125,125
  ;
  ; * display image
  ;
  ; * start loop
  ;   * get commands do things
  ;     "+" zoom in: then get position from cursor
  ;     "-" zoom out: just zoom out by a factor of 2 centred at current pos
  ;     "m" mask rectangle: get two corners with cursor
  ;     "u" unmask rectangle: get two corners with cursor
  ;     "c" mask circle: get centre and outer edge
  ;
  ;   * do masking/zooming
  ; * end loop
  ;
  ; * write out edited mask
END 
