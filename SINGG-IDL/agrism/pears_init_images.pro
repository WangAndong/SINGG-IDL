PRO pears_init_images, fili, pfxo, fgood_area, verbose=verbose
   ;
   ;  Initialize the drizzled direct and grism images as received from 
   ;  the pears team.
   ;
   ;   G. Meurer 08/2005
   ;   G. Meurer 09/2008 monitor -> verbose
   xoffset   = 1.0
   yoffset   = 1.0
   box1      = [13, 3]
   box0      = [3, 1]
   nsigma    = 5.0
   ;
   ; 1. breakfits
   IF keyword_set(verbose) THEN print, 'PEARS_INIT_IMAGES: breaking up multi-extension fits files. '+systime()
   breakfits, fili, pfxo
   ;
   ; 2. read the image back in:
   ;    * fix EXPTIME in header
   ;    * write NCOMBINE keyword
   ;    * guesstimate sky value for ALIGNSKY keyword
   ;    * multiply by exposure time
   ;    * write the image back to the same file
   IF keyword_set(verbose) THEN print, 'PEARS_INIT_IMAGES: adjusting science image & header. '+systime()
   file = pfxo + '_SCI_1.fits'
   fits_read, file, img, hdr
   ndrizim = sxpar(hdr, 'NDRIZIM')
   exp1    = float(sxpar(hdr, 'EXPTIME'))
   sky1    = float(sxpar(hdr, 'SKYSUM'))
   exptime = 0.0
   FOR ii = 1, ndrizim DO BEGIN 
      istr = strtrim(ii,2)
      IF ii LT 10 THEN istr = '0'+istr
      IF ii LT 100 THEN istr = '0'+istr
      kwd = 'D'+istr+'DEXP'
      exptime = exptime + float(sxpar(hdr, kwd))
   ENDFOR 
   IF exp1 GT 0.0 AND sky1 GT 0.0 THEN sky = exptime*sky1/exp1 ELSE sky = 0.0
   ;
   sxaddpar, hdr, 'EXPTIME', exptime
   sxaddpar, hdr, 'NCOMBINE', ndrizim
   sxaddpar, hdr, 'ALIGNSKY', sky
   sxaddpar, hdr, 'BUNIT', 'ELECTRONS'
   ;
   img       = exptime*img
   mwrfits, img, file, hdr, /create
   ;
   ; 3. mask image
   IF keyword_set(verbose) THEN print, 'PEARS_INIT_IMAGES: masking edges. '+systime()
   film      = pfxo + '_edgemsk.fits'
   ng        = n_elements(fgood_area) 
   mask      = 0b*byte(img)
   FOR ii = 0, ng-1 DO BEGIN
      readcol, fgood_area[ii], xpoly, ypoly, format='(f,f)'
      np     = n_elements(xpoly)
      IF xpoly[0] NE xpoly[np-1] AND ypoly[0] NE ypoly[np-1] THEN BEGIN 
         xpoly = [xpoly, xpoly[0]]
         ypoly = [ypoly, ypoly[0]]
         np    = np+1
      ENDIF 
      mask   = genmask_area(xpoly, ypoly, xoffset=xoffset, yoffset=yoffset, inmask=mask)
   ENDFOR 
   img       = float(temporary(mask))*img
   ;
   ; write output
   mwrfits, img, film, hdr, /create
   ; mask      = 0.0 * total(temporary(mask))
   ;
   ; 4. unsharp mask the image
   IF keyword_set(verbose) THEN print, 'PEARS_INIT_IMAGES: starting unsharp masking. '+systime()
   grm_rectsmooth, img, imsmooth, box1, box0, verbose=verbose
   ; imsmooth = sigclipper(img, box1[0], box1[1], nsigma=nsigma, /iterate, /neg, /quiet)
   imsharp  = img - imsmooth
   ;
   ; write results.
   filsm   = pfxo + '_smooth.fits'
   filsh   = pfxo + '_sharp.fits'
   mwrfits, imsmooth, filsm, hdr, /create
   mwrfits, imsharp, filsh, hdr, /create
   IF keyword_set(verbose) THEN print, 'PEARS_INIT_IMAGES: finished. '+systime()
END 
