PRO mask_hdfn_grism
   ;
   ; Mask the HDFN grism data so that only the "good" areas in
   ; the images are kept, everything else zeroed out.
   ;
   forward_FUNCTION genmask_area
   ;
   wd        = '/home/meurer/ACS/Grism/HDFN_jun04/Apsis/Work/'
   fili      = 'hdf-north-wfpc2_g800l_drz_sci.fits'
   filo      = 'hdf-north-wfpc2_g800l_masked.fits'
   goodareas = ['grism_ccd1.dat', 'grism_ccd2.dat']
   xoffset   = 1.0
   yoffset   = 1.0
   ;
   cd, wd, current=cwd
   ;
   ; open image
   img       = readfits(fili, hdr)
   ;
   ; make multiplicative mask
   ng        = n_elements(goodareas) 
   mask      = 0b*byte(img)
   FOR i = 0, ng-1 DO BEGIN
      readcol, goodareas[i], xpoly, ypoly, format='(f,f)'
      mask   = genmask_area(xpoly, ypoly, xoffset=xoffset, yoffset=yoffset, inmask=mask)
   ENDFOR 
   ;
   ; apply mask
   img       = float(mask)*img
   ;
   ; write output
   writefits, filo, img, hdr
   ;
   cd, cwd
END 
