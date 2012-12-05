pro poisson_sky, image, skylev, eskylev, circlerad=circlerad, mask=mask, silent=silent
  ;
  ; Get sky level in the limits of low counts: the Poissonian limit.
  ;
  ;   image      -> input image or sky vector
  ;   skylev     <- output sky level
  ;   eskylev    <- uncertainty in the sky level
  ;   circlerad  -> Use the keyword to only select pixels within
  ;                 the specified pixel radius of the center of the image.  If 
  ;                 CIRCLERAD = 1, then the radius is set equal to half the image
  ;                 width.   Useful when the data is restricted to a circular area
  ;                 of the image.
  ;   mask       -> Mask of pixels not to use in the calculation
  ;                 these should be set to 1b (good pixels have 
  ;                 mask = 0b).
  ;   silent     -> if set, then skylev and eskylev will not 
  ;                 be displayed at the terminal
  ;
  ; G. Meurer 02/2011  (ICRAR/UWA) 
  ;
  ; future changes: 
  ;   1. optional turn off clipping? 
  ;   2. if circlerad < 0 then sky is pixels at rad > abs(circlerad)
  ;
  prog    = 'POISSON_SKY: '
  nsig    =  3.0
  maxiter =  50
  ;
  ; set a verbose flag
  vb      = not keyword_set(silent)
  ;
  siz     = size(image)
  siz     = siz[0:siz[0]]
  if keyword_set(mask) then begin
     ;
     ; make sure mask has the right dimensions
     msiz    = size(mask)
     msiz    = msiz[0:msiz[0]]
     if min(siz eq msiz) eq 0b then stop, '**** error in '+prog+' image and mask have different dimensions '
     ;
     ; invert bad pixel mask to make good pixel mask
     mgood   = byte((1 - mask) > 0 )
  endif else begin
     ;
     ; default good mask
     mgood   = byte(0.0*image + 1.0)
  endelse 
  ;
  ; mask out pixels beyond circlerad, if need be
  if keyword_set(circlerad) then begin 
     ;
     ; can only use circlerad with 2D arrays
     if siz[0] ne 2 then stop, '**** error in '+prog+' parameter CIRCLERAD only works for 2D arrays '
     width  = max([siz[1],siz[2]])
     if circlerad EQ 1 then rad = width/2 else rad = long(circlerad)
     ;
     npix      = siz[1]*siz[2]
     xp        = reform((lindgen(npix) mod long(siz[1])), siz[1], siz[2])
     yp        = reform((lindgen(npix) / long(siz[1])), siz[1], siz[2])
     pixrad    = (xp-siz[1]/2.0)^2 + (yp-siz[2]/2.0)^2
     jj        = where(pixrad gt rad*rad)
     mgood[jj] = 0b
  endif 
  ;
  ; prepare for REPEAT loop
  use     = mgood
  ii      = 0
  repeat begin 
     ii       = ii + 1   ; increment iteration
     jj       = where(use eq 1b, njj)
     if njj le 0 then  stop, '**** error in '+prog+' no pixels left'
     skylev   = total(image[jj])/float(njj)
     ;
     ; for pixel-pixel uncertainty take maximum of 
     ; rms, sqrt(level), and 1.0
     rms      = sqrt(total((image[jj]-skylev)^2)/float(njj))
     eskylev  = max([rms,sqrt(skylev),1.0])
     ;
     ; print results if verbose (keyord SILENT not set)
     if vb then print,prog+'iteration = '+strtrim(string(ii),2)+' mean = '+strtrim(string(skylev),2)+' error = '+strtrim(string(eskylev,2))+' npix = '+strtrim(string(njj),2)
     ;
     ; for testing set minimum pixel-pixel uncertainty to 1
     ;etest    = eskylev > 1.0    
     ;
     ; limits for good pixels in next iteration
     cut0     = (skylev - nsig*eskylev) > 0.0
     cut1     = skylev + nsig*eskylev
     ;
     ; mark pixels to use in next iteration
     use1     = mgood
     jj       = where((image lt cut0) or (image gt cut1), njj)
     if njj gt 0 then use1[jj] = 0b
     if vb then print,prog+'number of pixels masked by cut0,1 = '+strtrim(string(njj),2)
     ;
     ; look for pixels that are different
     kk       = where(use ne use1, nkk)
     ;
     ; set for next iteration
     use      = use1
  endrep until ((ii ge maxiter) or (nkk eq 0))   ; stop at iteration maximum or when pixels measured does not change
end 
