PRO grm_rectsmooth, imin, imout, box1, box0, sclip=sclip, imsig=imsig, $
                    verbose=verbose
   ;
   ; Rectangular smoothing of a 2d array
   ;
   ;   imin   -> input image
   ;   imout  <- output image
   ;   box1   -> outer dimensions of box.  This is a 2d array
   ;             [column width, row width].  Pixels within
   ;             a box this size will be used in the smoothing,
   ;             except for those pixels within box0
   ;   box0   -> dimensions of inner box excluded from the 
   ;             smoothing kernal.  If not present, all pixels
   ;             within box1 will be used in the smoothing.
   ;   sclip  -> if set, the clipping to use in an iterative 
   ;             avsigclip algorithm; values deviating by more than
   ;             sclip * sigma are ignored in later iterations.  If not 
   ;             then median smoothing is performed.
   ;   imsig  <- If set then this is returned as the disperion at 
   ;             each pixel value.  imsig is only valid if sclip is set.
   ;   verbose -> if set then you get messages.
   ;
   ; G. Meurer 08/2005
   ; G. Meurer 09/2008 more documentation.  monitor changed to verbose
   ;
   IF keyword_set(verbose) THEN print, 'GRM_RECTSMOOTH: initializing - ' + systime()
   szim     = size(imin)
   nx       = szim[1]
   ny       = szim[2]
   npix     = long(nx)*long(ny)
   maxit    = 5
   nmin     = 2
   ;
   ; calculate offset position for pixels in kernels
   nkern1   = long(box1[0]) * long(box1[1])
   poffx1   = lindgen(nkern1) MOD long(box1[0]) - long(box1[0])/2l
   poffy1   = lindgen(nkern1) / long(box1[0]) - long(box1[1])/2l
   poff     = poffy1*long(nx) + poffx1
   ;
   IF keyword_set(box0) THEN BEGIN 
      pkeep   = make_array(nkern1, /byte, value=1b)
      nkern0  = long(box0[0]) * long(box0[1])
      poffx0  = lindgen(nkern0) MOD long(box0[0]) - long(box0[0])/2l
      poffy0  = lindgen(nkern0) / long(box0[0]) - long(box0[1])/2l
      poff0   = poffy0*long(ny) + poffx0
      FOR jj = 0, nkern0-1 DO BEGIN 
         kk   = where(poff EQ poff0[jj], nkk)
         IF nkk GT 0 THEN pkeep[kk] = 0b
      ENDFOR 
      jj      = where(pkeep, njj)
      IF njj GT 0 THEN poff = poff[jj]
   ENDIF 
   ;
   ; reformat the input image to a 1d array
   imin = reform(imin, npix, /overwrite)
   ;
   ; initialize output array
   imout = 0.0*imin
   IF keyword_set(sclip) AND keyword_set(imsig) THEN imsig = 0.0*imin
   ;
   ; loop through all pixels
   rtest = nx * 100l
   IF keyword_set(verbose) THEN print, 'GRM_RECTSMOOTH: starting smoothing loop - ' + systime()
   ;
   ; keep test for smoothing method outside of loop
   IF keyword_set(sclip) THEN BEGIN
      ;
      ; loop for av sig clip
      FOR jj = 0l, npix-1l DO BEGIN 
         IF keyword_set(verbose) THEN IF (jj MOD rtest) EQ 0l THEN print, 'GRM_RECTSMOOTH: Working on row # ', jj / nx
         ;
         ; get offset pixels to use
         kk = poff + jj
         ;
         ; adjust for edge effects
         qq = where(kk GE 0 AND kk LT npix, nkk)
         kk = kk[qq]
         ;
         ; grab pixel values
         arr = imin[kk]
         ;
         ; setup for av sig clip
         nit     =  0
         nch     =  0
         usenew  =  lindgen(nkk)
         nusenew =  nkk
         good    =  make_array(nkk, /byte, value=1b)
         ;
         REPEAT BEGIN 
            use     = usenew
            nuse    = nusenew
            mom     = moment(arr[use], sdev=sigma)
            res     = arr - mom[0]
            usenew  = where(abs(res) LE sclip*sigma, nusenew)
            IF nusenew GE nmin THEN BEGIN 
               goodnew         = 0b*good
               goodnew[usenew] = 1b
               nch             = total(abs(fix(good) - fix(goodnew)))
               good            = goodnew
            ENDIF  
            nit = nit + 1
         ENDREP UNTIL (nch EQ 0 OR nit GE maxit OR nusenew LT nmin)
         imout[jj] = mom[0]
         IF keyword_set(imsig) THEN imsig[jj] = sigma
      ENDFOR
   ENDIF ELSE BEGIN 
      FOR jj = 0l, npix-1l DO BEGIN 
         IF keyword_set(verbose) THEN IF (jj MOD rtest) EQ 0l THEN print, 'GRM_RECTSMOOTH: Working on row # ', jj / nx
         ;
         ; get offset pixels to use
         kk = poff + jj
         ;
         ; adjust for edge effects
         qq = where(kk GE 0 AND kk LT npix)
         kk = kk[qq]
         ;
         ; grab pixel values
         arr = imin[kk]
         ;
         ; calculate average (clipped average, median, ...)
         imout[jj] = median(arr, /even)
      ENDFOR
   ENDELSE  
   IF keyword_set(verbose) THEN print, 'GRM_RECTSMOOTH: smoothing loop finished - ' + systime()
   ;
   ; put the input and output images into final format
   imin = reform(imin, nx, ny)
   imout = reform(imout, nx, ny)
   IF keyword_set(verbose) THEN print, 'GRM_RECTSMOOTH: finished - ' + systime()
END 
