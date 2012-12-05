PRO mask_pears_testdata
   ;
   ; Mask the HDFN grism data so that only the "good" areas in
   ; the images are kept, everything else zeroed out.
   ;
   forward_FUNCTION genmask_area
   ;
   wd        = '/home/meurer/ACS/Grism/PEARS/Testdata/'
   fili1     = 'direct_SCI_1.fits'
   filo1     = 'direct_edgemsk.fits'
   fili2     = 'grism_SCI_1.fits'
   filo2     = 'grism_edgemsk.fits'
   goodareas1 = ['direct_ccd1.dat', 'direct_ccd2.dat']
   goodareas2 = ['grism_ccd1.dat', 'grism_ccd2.dat']
   xoffset   = 1.0
   yoffset   = 1.0
   ;
   cd, wd, current=cwd
   ;
   ; open image
   img       = readfits(fili1, hdr)
   ;
   ; make multiplicative mask
   ng        = n_elements(goodareas1) 
   mask      = 0b*byte(img)
   FOR i = 0, ng-1 DO BEGIN
      readcol, goodareas1[i], xpoly, ypoly, format='(f,f)'
      mask   = genmask_area(xpoly, ypoly, xoffset=xoffset, yoffset=yoffset, inmask=mask)
   ENDFOR 
   ;
   ; apply mask
   img       = float(mask)*img
   ;
   ; write output
   writefits, filo1, img, hdr
   ;
   ; open image
   img       = readfits(fili2, hdr)
   ;
   ; make multiplicative mask
   ng        = n_elements(goodareas2) 
   mask      = 0b*byte(img)
   FOR i = 0, ng-1 DO BEGIN
      readcol, goodareas2[i], xpoly, ypoly, format='(f,f)'
      mask   = genmask_area(xpoly, ypoly, xoffset=xoffset, yoffset=yoffset, inmask=mask)
   ENDFOR 
   ;
   ; apply mask
   img       = float(mask)*img
   ;
   ; write output
   writefits, filo2, img, hdr
   ;
   cd, cwd
END 
