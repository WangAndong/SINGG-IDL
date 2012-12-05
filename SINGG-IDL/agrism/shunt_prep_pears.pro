PRO shunt_prep_pears, fgrism, fdirect, mask_files, xbin, pfxo, zrange_grism, zrange_direct, $
                      fgsci, fgtrim, fgmask, fgsquash, fqsqmask, fdsci, verbose=verbose
   ;
   ; Prepare PEARS grism and direct images for Supernova Hunt.
   ;
   ; fgrism        -> Name of input fits grism image.
   ; mask_files    -> String array of mask file names.  Each file 
   ;                  will have two columns giving the x,y pixel
   ;                  coords deliminating the good areas of the
   ;                  image.
   ; xbin          -> binning factor in x for squashing
   ; pfxo          -> prefix for output fits images
   ; zrange_grism  <> passed as the grism image display range in counts/sec
   ;                  returned as the same value in counts
   ; zrange_direct <> passed as the direct image display range in counts/sec
   ;                  returned as the same value in counts
   ; fgsci         <- Broken out simple fits file of grism image science extension
   ; fgtrim        <- edge-trimmed image
   ; fgmask        <- good pixel mask of trimmed image
   ; fgsquash      <- squashed grism image
   ; fqsqmask      <- good pixel mask for squashed grism image
   ; fdsci         <- name of direct image science extension
   ; exptime       <- exposure time of grism image
   ; verbose       -> if set then shunt_prep tells you what it is doing.
   ;
   ; G. Meurer 09/2005
   ;
   ; kludge some noise model parameters.  Need to work out
   ; how to get them from the headers...
   ;ncomb    = 6
   ;rn       = 5.0 
   ;drate    = 8.0/3600.0
   ;
   ; break out multi-extension fits files
   fits_info, fgrism, n_ext=nextg, /silent
   fits_info, fdirect, n_ext=nextd, /silent
   IF nextg GT 0 THEN BEGIN 
      IF keyword_set(verbose) THEN print, 'SHUNT_PREP_PEARS: Breaking out multi-extension file: '+fgrism
      breakfits, fgrism, pfxo+'_grism'
      fgsci  = pfxo+'_grism_SCI_1.fits'
   ENDIF ELSE BEGIN 
      fgsci  = pfxo+'_grism.fits'
      IF keyword_set(verbose) THEN print, 'SHUNT_PREP_PEARS: copying: '+fgrism+' > '+fgsci
      cmd = 'cp '+fgrism+' '+fgsci
      spawn, cmd
   ENDELSE 
   IF nextd GT 0 THEN BEGIN 
      IF keyword_set(verbose) THEN print, 'SHUNT_PREP_PEARS: Breaking out multi-extension file: '+fdirect
      breakfits, fdirect, pfxo+'_direct'
      fdsci  = pfxo+'_direct_SCI_1.fits'
   ENDIF ELSE BEGIN 
      fdsci  = pfxo+'_direct.fits'
      IF keyword_set(verbose) THEN print, 'SHUNT_PREP_PEARS: copying: '+fdirect+' > '+fdsci
      cmd = 'cp '+fdirect+' '+gdsci
      spawn, cmd
   ENDELSE 
   ;
   ; get exposure time, and multiply grism image by this
   ; to get grism image in counts
   texpg = drz_texptime(fgrism)
   IF texpg LE 0 THEN texpg = 1.0
   IF keyword_set(verbose) THEN print, 'SHUNT_PREP_PEARS: multiplying grism image by exposure time: ', texpg
   fits_read, fgsci, img, hdg
   img  = img*texpg
   fits_write, fgsci, img, hdg
   zrange_grism = texpg*zrange_grism
   ;
   ; get exposure time, and multiply direct image by this
   ; to get direct image in counts
   texpd = drz_texptime(fdirect)
   IF texpd LE 0 THEN texpd = 1.0
   IF keyword_set(verbose) THEN print, 'SHUNT_PREP_PEARS: multiplying direct image by exposure time: ', texpd
   fits_read, fdsci, imd, hdd
   imd  = imd*texpd
   fits_write, fdsci, imd, hdd
   zrange_direct = texpd*zrange_direct
   ;
   ; make multiplicative mask
   IF keyword_set(verbose) THEN print, 'SHUNT_PREP_PEARS: masking grism image '
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
   IF keyword_set(verbose) THEN print, 'SHUNT_PREP_PEARS: squashing grism image '
   szg       = size(img)
   nco       = fix(szg[1]/xbin) 
   img3      = rebin(img[0:nco*xbin-1,0:szg[2]-1],nco,szg[2])
   img3      = float(xbin)*img3 
   ;
   ; Now mask squashed image
   IF keyword_set(verbose) THEN print, 'SHUNT_PREP_PEARS: masking squashed grism image '
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
   fgtrim     = pfxo+'_grism_trimmed.fits'
   fgmask     = pfxo+'_grism_mask.fits'
   fgsquash   = pfxo+'_grism_squash.fits'
   fqsqmask   = pfxo+'_grism_sqmask.fits'
   IF keyword_set(verbose) THEN print, 'SHUNT_PREP_PEARS: writing trimmed grism image: '+fgtrim
   writefits, fgtrim, img2, hdr
   IF keyword_set(verbose) THEN print, 'SHUNT_PREP_PEARS: writing mask for trimmed grism image: '+fgmask
   writefits, fgmask, mask, hdr
   IF keyword_set(verbose) THEN print, 'SHUNT_PREP_PEARS: writing squashed grism image: '+fgsquash
   writefits, fgsquash, img4, hdr
   IF keyword_set(verbose) THEN print, 'SHUNT_PREP_PEARS: writing mask for squashed grism image: '+fqsqmask
   writefits, fqsqmask, mask2, hdr
END 
