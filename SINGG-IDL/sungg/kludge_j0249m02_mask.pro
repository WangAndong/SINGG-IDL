PRO kludge_j0249m02_mask
  wd    = '~/SUNGG/OUPIPE/J0249-02_play/'
  ;fili  = 'J0249-02_mask.fits'
  ;filo  = 'J0249-02_kludge_mask.fits'
  fili  = 'J0249-02_Rsub_mask.fits'
  filo  = 'J0249-02_kludge_Rsub_mask.fits'
  i1    = 1307
  j1    = 1462
  i2    = 1311
  j2    = 1468
  ;
  cd, wd, current=cwd
  ;
  ; open image
  fits_read, fili, imm, hdm
  ;
  ; fix mask in kludge region
  imm[i1:i2,j1:j2] = 0b
  ;
  ; write output mask
  fits_write, filo, imm, hdm
  ;
  cd, cwd
END 
