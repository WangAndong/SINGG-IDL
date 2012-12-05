PRO hdfn_calc_mask_area
   ;
   ; calculate area in zero order mask for HDFN data
   filsgoodg  = ['grism_ccd1.dat', 'grism_ccd2.dat']
   ; 1. make mask file
   zero_ord_mask, 'detectionImage.fits', 'hdf-north-wfpc2_g800l_sharp133.fits', 'poop.fits', $
    [-111.527183299, 1.002591233, -0.002904541, -1.372370755, 0.000483156, 0.999925759], $
    21.5096, 28.6, grow=3, mask_out='hdfn_zordmask.fits'
   ;
   ; 2. make xp & yp arrays giving x,y positions of pixel
   fits_read, 'hdfn_zordmask.fits', imz, hdz
   nx = long(sxpar(hdz, 'naxis1'))
   ny = long(sxpar(hdz, 'naxis2'))
   xx = findgen(nx*ny) MOD float(nx)
   yy = float(fix(findgen(nx*ny)/float(ny)))
   ;
   ; determine which pixels are inside good areas
   readcol, filsgoodg[0], xp1, yp1, format='(f,f)'
   xp1  = xp1 - 1.0
   yp1  = yp1 - 1.0
   readcol, filsgoodg[1], xp2, yp2, format='(f,f)'
   xp2  = xp2 - 1.0
   yp2  = yp2 - 1.0
   gp   = make_array(nx*ny, /byte, value=0b)
   FOR jj = 0l, ny-1l DO BEGIN 
      i1 = jj*nx
      i2 = i1 + nx - 1
      ;print, i1, i2
      gg1 = inside(xx[i1:i2], yy[i1:i2], xp1, yp1)
      gg2 = inside(xx[i1:i2], yy[i1:i2], xp2, yp2)
      gp[i1:i2] = gg1 + gg2
   ENDFOR 
   ;
   ; do the counting
   imz = reform(imz, nx*ny)
   pp  = where(imz GT 0b, nmask0)
   pp  = where(gp GT 0b, ngood)
   pp  = where(gp GT 0b AND imz GT 0b, nmask1)
   print, 'Number of zero order masked pixels    : ', nmask0
   print, 'Number of pixels in good areas        : ', ngood
   print, 'Number of masked pixels in good areas : ', nmask1
   print, 'Fractional area in masked pixels in good area : ', float(nmask1)/float(ngood)
   
END 
