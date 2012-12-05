pro mk_dummy_uvmask
  ;
  ; make a simple circular mask for galex UV images
  ;
  ; G. Meurer 02/2011 (ICRAR/UWA)
  filo    = 'uvmask.fits'
  xc      = 1920.5
  yc      = 1920.5
  naxis1  = 3840l
  naxis2  = 3840l
  radm    = 1401.
  ;
  ; make empty array
  mask    = make_array(naxis1, naxis2, /byte, value=0b)
  ;
  ; pixel coords
  xx      = float(lindgen(naxis1*naxis2) mod long(naxis1))
  yy      = float(lindgen(naxis1*naxis2) / long(naxis1))
  ;
  ; radius from xc,yc
  rr      = sqrt((xx-xc)^2+(yy-yc)^2)
  ;
  ; pixels outside radm
  pp      = where(rr gt radm, npp)
  ;
  ; do the masking
  if npp gt 0 then mask[pp] = 1b
  ;
  ; write the fits file, 
  ;   note only minimal header will be used at this stage
  fits_write, filo, mask
end 
