pro convert_zz_mask, fili, filo
  ;
  ; convert a mask from Zheng Zheng to one that will work with SSOUP.
  ; input zz masks have
  ;    1 = good
  ;    0 = bad (star?)
  ;    2 = bad (galaxy?)
  ; output mask has
  ;    0 = good
  ;    1 = bad
  ;
  ; fili -> input file mask name
  ; filo -> output file mask name
  ;
  ; G. Meurer 4/2011
  goodval   = 0b
  badval    = 1b
  ;
  ; read in input image
  fits_read, fili, imi, hdi
  ;
  ; make output image array, set to goodval
  imo    = byte(imi)*0b + goodval
  ;
  ; get indecis of bad pixels
  ii     = where(imi ne 1, nii)
  ;
  ; set bad pixel
  if nii gt 0 then imo[ii] = badval
  ;
  ; write output image with same header as input image
  fits_write, filo, imo, hdi
end
