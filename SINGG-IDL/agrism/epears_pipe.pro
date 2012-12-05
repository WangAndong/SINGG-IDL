PRO epears_pipe, fsci, fwht, fscio, frms, fsmooth, fsharp, fcat, fcheck, $
                 freg, fmsk=fmsk, skyrate=skyrate, quiet=quiet, bx1=bx1, bx0=bx0, $
                 se_inpar=se_inpar
   ;
   ; Process distortion corrected grism images to find the emission line
   ; sources.
   ;
   ; fsci    -> Name of INPUT drizzled science frame.  This should be a 
   ;            count rate image that has been background subtracted.
   ; fwht    -> name of INPUT weight map.  This is produced during the
   ;            drizzle process and gives the exposure time (s) of each 
   ;            pixel.
   ; fscio   -> name of OUTPUT science image scaled to total counts (at
   ;            nominal exposure time).
   ; frms    -> name of OUTPUT rms image this is in total counts.
   ; fsmooth -> name of OUTPUT smoothed image.  Smoothing is with a 13x3
   ;            median filter with a 3x1 hole in the center (unless 
   ;            otherwise set with bx1 and bx0 parameters).
   ; fsharp  -> name of OUTPUT sharpened grism image.  This is the original
   ;            image (scaled by total exposure time) - the smooth image.
   ; fcat    -> Name of the OUTPUT SE catalog.
   ; fcheck  -> Name of the OUTPUT SE check image.  For standard
   ;            setup this should be a segmentation image.  
   ; freg    -> Name of the OUTPUT regions file made from the SE catalog
   ; fmsk    -> Optional mask file specification.  If not set then no 
   ;            masking is done.  If set this can be either 
   ;            1. a string array of filenames specifying the good 
   ;               area as a polygon (not closed).
   ;               x y in columns 1 2 of each file .
   ;               a file mask.fits is written specifying the mask.
   ;            2. scalar string rootname of a single file
   ;               if fmsk+'.fits' exists then this a mask of 1 (good)
   ;               0 (bad) which will be multiplied with sharpened image
   ;               before it is written.
   ;               if fmsk+'.fits' does not exist, then the program 
   ;               looks for fmsk+'.dat' and uses that as a single
   ;               polygon mask specification.  An output mask in fits
   ;               format is written as fmsk+'.fits'
   ; skyrate -> Optional sky count rate.  This is set to 0.1 ct/s/pixel 
   ;            if not supplied.
   ; quiet   -> if set turns off verbose
   ; bx1     -> if set, then this a 2 dimensional array giving x,y 
   ;            outer dimensions of the median filter to apply.  
   ;            The default is bx1 = [13,3]
   ; bx0     -> if set, then this is a 2 dimensional array giving x,y 
   ;            inner dimensions (i.e. the hole) of the median filter 
   ;            to apply.  The default is bx0 = [3,1]
   ; se_inpar -> If set, this the name of a SExtractor input parameter
   ;             file to use in the catalog stage.
   ;
   ; G. Meurer  9/2008
   ;
   ; default parameters
   _srate     = 0.1
   _box1      = [13,3]
   _box0      = [3,1]
   _verbose   = 1b
   _se_inpar  = 'hiifinder_grm.inpar'
   prog       = 'EPEARS_PIPE'
   ;
   ; override defaults
   IF keyword_set(skyrate) THEN srate = skyrate ELSE srate = _srate
   IF keyword_set(quiet) THEN verbose = 0b ELSE verbose=_verbose
   IF keyword_set(box1) THEN box1 = bx1 ELSE box1 = _box1
   IF keyword_set(box0) THEN box0 = bx0 ELSE box0 = _box0
   IF keyword_set(se_inpar) THEN separ = separ ELSE separ = _se_inpar
   ;
   ; make rms map and retrieve science data.
   infile    = [fsci, fwht]
   IF verbose THEN print, prog+': calling MKRMSMAP for infile = ', infile
   mkrmsmap, infile, frms, skyrate=srate, sci=sci, hdsci=hdsci, /verbose, /countout, /primary_wht_hdr
   ;
   ; save exposure time scaled science frame
   hdscio    = hdsci
   IF verbose THEN print, prog+': writing output image = '+fscio
   fits_write, fscio, sci, hdsci
   ;
   ; create smooth and sharpened images
   IF verbose THEN print, prog+': calling GRM_RECTSMOOTH.  Box1 = ', box1, ' Box0 = ', box0   
   grm_rectsmooth, sci, imsm, box1, box0, verbose=verbose
   IF verbose THEN print, prog+': sharpening. '
   imsh      = sci - imsm
   ;
   ; (mask the sharpened image)
   IF keyword_set(fmsk) THEN BEGIN 
      IF verbose THEN print, prog+': will mask smooth and sharp images '
      ng         = n_elements(fmsk)
      mask       = 0b*byte(sci)
      mmade      = 0b
      wrmsk      = 0b
      fgood_area = fmsk
      IF ng EQ 1 THEN BEGIN 
         ;
         ; test if there is a fits file meeting specification
         filem  = ''
         IF strpos(fmsk, '.fit') GT 0 THEN BEGIN 
            ;
            ; fmsk looks like a fits image
            filem  = fmsk
         ENDIF 
         IF filem EQ '' AND strpos(fmsk, '.') LE 0 THEN BEGIN 
            ;
            ; fmsk is a root file name try this for fits image
            filem = fmsk+'.fits'
         ENDIF 
         info = file_info(filem)
         IF filem NE '' AND info.exists THEN BEGIN 
            IF verbose THEN print, prog+': reading mask image: '+filem
            fits_read, filem, mask, hdmsk
            mmade = 1b
         ENDIF 
         IF filem NE '' AND NOT mmade THEN BEGIN 
            fgood_area = fmsk+'.dat'
            filem      = ''
         ENDIF 
      ENDIF 
      IF NOT mmade THEN BEGIN 
         FOR ii = 0, ng-1 DO BEGIN
            IF verbose THEN print, prog+': reading polygon mask file: '+filem
            readcol, fgood_area[ii], xpoly, ypoly, format='(f,f)'
            np     = n_elements(xpoly)
            IF xpoly[0] NE xpoly[np-1] AND ypoly[0] NE ypoly[np-1] THEN BEGIN 
               xpoly = [xpoly, xpoly[0]]
               ypoly = [ypoly, ypoly[0]]
               np    = np+1
            ENDIF 
            IF verbose THEN print, prog+': creating or supplementing mask (calling genmask_area)'
            mask   = genmask_area(xpoly, ypoly, inmask=mask)
         ENDFOR 
         mmade = 1b
         wrmsk = 1b
      ENDIF 
      ;
      ; decide whether to write mask file
      IF wrmsk THEN BEGIN 
         IF filem EQ '' THEN filem = 'mask.fits'
         IF verbose THEN print, prog+': writing mask file: '+filem
         hdmsk = hdsci
         sxaddpar, hdmsk, 'IMTYPE', 'MASK'
         check_fits, filem, mask, hdmsk, /update
         fits_write, filem, mask, hdmsk
      ENDIF 
      ;
      ; mask smooth and sharp images
      IF verbose THEN print, prog+': masking smooth and sharp image arrays' 
      mask      = float(mask)
      imsm      = mask*imsm
      imsh      = mask*imsh
   ENDIF 
   ;
   ; write output sharp and smooth images
   IF verbose THEN print, prog+': writing output smoothed image: '+fsmooth
   fits_write, fsmooth, imsm, hdsci
   IF verbose THEN print, prog+': writing output sharpened image: '+fsharp
   fits_write, fsharp, imsh, hdsci
   ;
   ; run SE
   cmd       = 'sex '+fsharp+' -WEIGHT_IMAGE '+frms+' -c '+separ+' -CATALOG_NAME '+fcat+' -CHECKIMAGE_NAME '+fcheck
   IF verbose THEN print, prog+': starting SExtractor with command: '
   IF verbose THEN print, cmd
   spawn, cmd
   ;
   ; make regions file using awk
   inf = file_info(freg)
   IF inf.exists THEN BEGIN
      IF verbose THEN print, prog+': deleting previous regions file: '+freg
      file_delete, freg
   ENDIF 
   dq        = '"'
   cmd       = "awk 'substr($1,1,1) != "+dq+'#'+dq+" {printf "$
               +dq+"image; ellipse (%s,%s,%s,%s,%s)\n"+dq+",$2,$3,2.0*$14,2.0*$15,$16}' "+fcat+" > "+freg
   IF verbose THEN print, prog+': awk-ing catalog to make regions file: '+freg+'  using command: '+cmd
   spawn, cmd
END 
