PRO mkrmsmap, infile, rmsfile, skyrate=skyrate, sci=sci, hdsci=hdsci, $
              primary_wht_hdr=primary_wht_hdr, countout=countout, verbose=verbose
   ;
   ; make an rms map from the sci and wgt images, 
   ; following the example of apsis
   ;
   ; infile  ->  the name of the input image(s):  If this is
   ;             a single string variable then it is taken to
   ;             to be the name of a MEF (multi-extension) fits 
   ;             format image.
   ;             If it is a 2-element string array then the first
   ;             element is taken to be the name of the fits file, 
   ;             containing the science frame and the 
   ;             second element is the file name of weight array.
   ;             In either case the input images are assumed to be in 
   ;             count rate (electrons/sec).
   ; rmsfile ->  name of output rms image.
   ; sci     <-  optionally returns science image array 
   ; hdsci   <-  optionally returns science image header
   ; primary_wht_hdr -> if set use the weight image header is taken to
   ;                    to be the primary header.
   ; countout -> if set then the output images (and arrays) are
   ;             in counts (otherwise they are in count rate).
   ; verbose ->  tells what it's doing.
   ;
   ; G. Meurer 09/2008
   ;
   _rnVarSuppFac_ = 1.38                  ; boost N*rn^2 by this factor for RMS images
   _exVarPerSec_  = 0.02222               ; add this much extra var per sec for RMS ims
   _maxval_       = 1.0d29
   prog           = 'MKRMSMAP'
   ;
   ; determine type of input 
   sz             = size(infile)
   idim           = sz[0]
   ityp           = sz[idim+1]
   IF ityp NE 7 THEN stop, '**** ERROR in '+prog+': infile must be a string or string array'
   IF idim GT 1 THEN stop, '**** ERROR in '+prog+': infile must be a scalar or 1 dimensional array'
   IF idim EQ 0 OR sz[1] EQ 1 THEN BEGIN 
      ;
      ; input is an MEF file
      ; read primary header, open science and weight extensions
      inmef       = infile[0]
      IF keyword_set(verbose) THEN print, prog+': reading MEF file: '+inmef
      fits_read, inmef, dum, hd0, /header_only
      fits_read, inmef, sci, hdsci, extname='SCI'
      fits_read, inmef, wht, hdwht, extname='WHT'

   ENDIF ELSE BEGIN 
      ;
      ; read science and weight images, call the science
      ; image header the "primary" header, unless primary_wht_hdr is set
      IF keyword_set(verbose) THEN print, prog+': reading science image: '+infile[0]
      fits_read, infile[0], sci, hdsci
      IF keyword_set(verbose) THEN print, prog+': reading weight image: '+infile[1]
      fits_read, infile[1], wht, hdwht
      IF keyword_set(primary_wht_hdr) THEN BEGIN 
         IF keyword_set(verbose) THEN print, prog+': kludging ncombine in weight image header (yuck ...) '
         sxaddpar, hdwht, 'NCOMBINE', sxpar(hdsci, 'NCOMBINE')
         IF keyword_set(verbose) THEN print, prog+': using weight image header as primary header '
         hd0      = hdwht
      ENDIF ELSE BEGIN 
         IF keyword_set(verbose) THEN print, prog+': using science image header as primary header '
         hd0      = hdsci
      ENDELSE 
   ENDELSE 
   ;
   ; copy weight header to rms header
   hdrms          = hdwht
   ;
   ; need to code this: area_ratio = (self.asecpix / self.origscale)**2
   ; for now take
   area_ratio     = 1.0
   ;
   ; get stuff from primary header
   exptime        = sxpar(hd0, 'EXPTIME')
   ncombine       = sxpar(hd0, 'NCOMBINE')
   gnvals         = [sxpar(hd0,'ATODGNA'), sxpar(hd0,'ATODGNB'), sxpar(hd0,'ATODGNC'), sxpar(hd0,'ATODGND')]
   rnvals         = [sxpar(hd0,'READNSEA'), sxpar(hd0,'READNSEB'), sxpar(hd0,'READNSEC'), sxpar(hd0,'READNSED')]
   ;
   ; take gain = average gain
   ; rn = readnoise = max(read noise)
   mom            = moment(gnvals)
   gain           = mom[0]
   rn             = max(rnvals)
   IF keyword_set(verbose) THEN print, prog+': image EXPTIME: ', exptime
   IF keyword_set(verbose) THEN print, prog+': image NCOMBINE: ', ncombine
   IF keyword_set(verbose) THEN print, prog+': adopted gain: ', gain
   IF keyword_set(verbose) THEN print, prog+': adopted read noise: ', rn
   IF keyword_set(verbose) THEN print, prog+': calculating rms image ... '
   ;
   ; calculate expected readout variance per pixel from read noise and
   ; number of frames
   readvar        = ncombine*(rn/gain)^2
   ;
   ; calculate total instrumental variance
   ; supplement factor for reference bias subtraction, etc
   ; extra var per sec for dark subtraction, repaired cosmic rays, etc.
   totinstvar     = area_ratio*((_rnVarSuppFac_ * readvar) + (_exVarPerSec_ * exptime))
   IF keyword_set(verbose) THEN print, prog+': total instrumental variance: ', totinstvar
   ;
   ; get skyrate level to add
   IF keyword_set(skyrate) THEN skylevel = skyrate ELSE skylevel = 0.0
   IF keyword_set(verbose) THEN print, prog+': sky level: ', skylevel
   ;
   ; apply noise model
   rms            = (exptime*(area_ratio*(sci + skylevel)*exptime/gain + totinstvar))/wht
   jj             = where(rms LE 0.0, njj)
   IF njj GT 0 THEN rms[jj] = 0.0
   ;
   ; set output products depending on whether a total counts 
   ; (countout=1b) or count rate (countout=1b) image is desired.
   IF keyword_set(countout) THEN BEGIN 
      IF keyword_set(verbose) THEN print, prog+': Will create total counts images (BUNIT=ELECTRONS)'
      rms         = sqrt(rms)
      sci         = exptime*sci
      sxaddpar, hdrms, 'BUNIT', 'ELECTRONS'
      sxaddpar, hdsci, 'BUNIT', 'ELECTRONS'
   ENDIF ELSE BEGIN 
     IF keyword_set(verbose) THEN print, prog+': Will create total count rate images (BUNIT=ELECTRONS/S)'
      rms         = sqrt(rms)/exptime
      sxaddpar, hdrms, 'BUNIT', 'ELECTRONS/S'
      sxaddpar, hdsci, 'BUNIT', 'ELECTRONS/S'
   ENDELSE 
   jj             = where(rms GT _maxval_, njj)
   IF njj GT 0 THEN rms[jj] = 0.0
   ; adjust some values
   sxaddpar, hdrms, 'FILENAME', rmsfile
   sxaddpar, hdrms, 'FILETYPE', 'RMS'
   ;
   ; write out rms image
   IF keyword_set(verbose) THEN print, prog+': writing RMS image: '+rmsfile
   fits_write, rmsfile, rms, hdrms
END 
