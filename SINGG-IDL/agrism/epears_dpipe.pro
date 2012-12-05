PRO epears_dpipe, fsci, fscio, fsmooth, fsharp, fcat, fcheck, freg, $
                  quiet=quiet, bx1=bx1, bx0=bx0, se_inpar=se_inpar
   ; 
   ; process distortion corrected direct images so that
   ; and catalog structure in sharpened images.
   ;
   ; fsci    -> Name of INPUT drizzled science frame.  This should be a 
   ;            count rate image that has been background subtracted.
   ; fscio   -> name of OUTPUT science image scaled to total counts (at
   ;            nominal exposure time).
   ; fsmooth -> name of OUTPUT smoothed image.  Smoothing is with a 13x3
   ;            median filter with a 3x1 hole in the center (unless 
   ;            otherwise set with bx1 and bx0 parameters).
   ; fsharp  -> name of OUTPUT sharpened image.  This is the original
   ;            image (scaled by total exposure time) - the smooth image.
   ; fcat    -> Name of the OUTPUT SE catalog.
   ; fcheck  -> Name of the OUTPUT SE check image.  For standard
   ;            setup this should be a segmentation image.  
   ; freg    -> Name of the OUTPUT regions file made from the SE catalog
   ; quiet   -> if set turns off verbose
   ; bx1     -> if set, then this a 2 dimensional array giving x,y 
   ;            outer dimensions of the median filter to apply.  
   ;            The default is bx1 = [13,3]
   ; bx0     -> if set, then this is a 2 dimensional array giving x,y 
   ;            inner dimensions (i.e. the hole) of the median filter 
   ;            to apply.  The default is bx0 = [3,1]
   ; se_inpar -> If set, this the name of a SExtractor input parameter
   ;             file to use in the catalog stage.
   ; default parameters
   _box1      = [13,3]
   _box0      = [3,1]
   _verbose   = 1b
   _se_inpar  = 'hiifinder_amber.inpar'
   prog       = 'EPEARS_DPIPE'
   ;
   ; override defaults
   IF keyword_set(quiet) THEN verbose = 0b ELSE verbose=_verbose
   IF keyword_set(box1) THEN box1 = bx1 ELSE box1 = _box1
   IF keyword_set(box0) THEN box0 = bx0 ELSE box0 = _box0
   IF keyword_set(se_inpar) THEN separ = separ ELSE separ = _se_inpar
   ;
   ; open science frame
   IF verbose THEN print, prog+': reading science image = ', fsci
   fits_read, fsci, sci, hdsci
   ;
   ; scale by exposure time
   exptime    = sxpar(hdsci, 'EXPTIME')
   IF verbose THEN print, prog+': scaling by exposure time = ', exptime
   sci        = exptime*sci
   sxaddpar, hdsci, 'BUNIT', 'ELECTRONS'
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
   ; write output sharp and smooth images
   IF verbose THEN print, prog+': writing output smoothed image: '+fsmooth
   fits_write, fsmooth, imsm, hdsci
   IF verbose THEN print, prog+': writing output sharpened image: '+fsharp
   fits_write, fsharp, imsh, hdsci
   ;
   ; run SE
   cmd       = 'sex '+fsharp+' -c '+separ+' -CATALOG_NAME '+fcat+' -CHECKIMAGE_NAME '+fcheck
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
