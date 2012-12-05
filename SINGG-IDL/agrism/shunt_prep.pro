PRO shunt_prep, fgrism, mask_files, xbin, pfxo, filsci, ftrim, fmask, fsquash, fsqmask, $
                exptime, verbose=verbose, nobreak=nobreak
   ;
   ; Prepare grism images for Supernova Hunt.
   ;
   ; fgrism     -> Name of input fits grism image.
   ; mask_files -> String array of mask file names.  Each file 
   ;               will have two columns giving the x,y pixel
   ;               coords deliminating the good areas of the
   ;               image.
   ; xbin       -> binning factor in x for squashing
   ; pfxo       -> prefix for output fits images
   ; filsci     <- Broken out simple fits file of science extension
   ; ftrim      <- edge-trimmed image
   ; fmask      <- good pixel mask of trimmed image
   ; fsquash    <- squashed grism image
   ; fsqmask    <- good pixel mask for squashed grism image
   ; exptime    <- exposure time of grism image
   ; verbose    -> if set then shunt_prep tells you what it is doing.
   ;
   ; G. Meurer 09/2005
   ;
   ; kludge some noise model parameters.  Need to work out
   ; how to get them from the headers...
   ;ncomb    = 6
   ;rn       = 5.0 
   ;drate    = 8.0/3600.0
   ;
   ; break out multi-extension fits file
   IF NOT keyword_set(nobreak) THEN BEGIN 
      IF keyword_set(verbose) THEN print, 'SHUNT_PREP: Breaking out multi-extension file: '+fgrism
      breakfits, fgrism, pfxo 
   ENDIF ELSE BEGIN 
      IF keyword_set(verbose) THEN print, 'SHUNT_PREP: copying science frame: '+fgrism
      spawn, 'cp -f '+fgrism+' '+pfxo+'_SCI_1.fits'
   ENDELSE 
   ;
   ; get exposure time, and multiply grism image by this
   ; to get grism image in counts
   texp = drz_texptime(fgrism)
   exptime = texp
   IF keyword_set(verbose) THEN print, 'SHUNT_PREP: multiplying by expoure time: ', texp
   filsci  = pfxo+'_SCI_1.fits'
   fits_read, filsci, img, hdg
   img  = img*texp
   fits_write, filsci, img, hdg
   ;
   ; make multiplicative mask
   IF keyword_set(verbose) THEN print, 'SHUNT_PREP: masking grism image '
   ng        = n_elements(mask_files) 
   mask      = 0b*byte(img)
   FOR i = 0, ng-1 DO BEGIN
      readcol, mask_files[i], xpoly, ypoly, format='(f,f)'
      mask   = genmask_area(xpoly, ypoly, xoffset=xoffset, yoffset=yoffset, inmask=mask)
   ENDFOR 
   ;
   ; apply mask
   img2      = float(mask)*img
   ;
   ; determine size for output squashed image
   IF keyword_set(verbose) THEN print, 'SHUNT_PREP: squashing grism image '
   szg       = size(img)
   nco       = fix(szg[1]/xbin) 
   img3      = rebin(img[0:nco*xbin-1,0:szg[2]-1],nco,szg[2])
   img3      = float(xbin)*img3 
   ;
   ; Now mask squashed image
   IF keyword_set(verbose) THEN print, 'SHUNT_PREP: masking squashed grism image '
   mask2     = 0b*byte(img3)
   FOR i = 0, ng-1 DO BEGIN
      readcol, mask_files[i], xpoly, ypoly, format='(f,f)'
      xpoly2 = (xpoly - 0.5*float(xbin + 1))/xbin
      mask2  = genmask_area(xpoly2, ypoly, xoffset=xoffset, yoffset=yoffset, inmask=mask2)
   ENDFOR 
   ;
   ; apply mask
   img4      = float(mask2)*img3
   ; 
   ; write output files
   ftrim     = pfxo+'_trimmed.fits'
   fmask     = pfxo+'_mask.fits'
   fsquash   = pfxo+'_squash.fits'
   fsqmask   = pfxo+'_sqmask.fits'
   IF keyword_set(verbose) THEN print, 'SHUNT_PREP: writing trimmed grism image: '+ftrim
   writefits, ftrim, img2, hdr
   IF keyword_set(verbose) THEN print, 'SHUNT_PREP: writing mask for trimmed grism image: '+fmask
   writefits, fmask, mask, hdr
   IF keyword_set(verbose) THEN print, 'SHUNT_PREP: writing squashed grism image: '+fsquash
   writefits, fsquash, img4, hdr
   IF keyword_set(verbose) THEN print, 'SHUNT_PREP: writing mask for squashed grism image: '+fsqmask
   writefits, fsqmask, mask2, hdr
END 
