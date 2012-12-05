FUNCTION calc_mdiff,imfile,reffile,shift,error,nmatch, $
                    IMSKY=imsky,REFSKY=refsky,NSIG=nsig,FAST=fast,DEBUG=debug,$
                    OFFSET=offset
;                    IMOFFSET=imoffset,REFOFFSET=refoffset
; INPUTS:
; imfile       Name of image file
; reffile      Name of reference file
; shift        Size of the edge to ignore (edge+buffer)
; OPTIONAL INPUTS:
; imsky[2]     mean and sigma of sky in first image
; refsky[2]    mean and sigma of sky in reference image
; imoffset     X and Y of positional offset between the two files
; refoffset
; nsig         Number of sigmas to use for rejection (default 3.0)
; /fast        Just use SExtractor magnitudes instead of actually
;                trying to place apertures.
; /debug       Turn on all the debug print statements
; OUTPUTS:
; (function)   Average difference in magnitudes between the two files.
;              Note that a negative number means im is brighter than ref
; error        Standard deviation of mean of the function value
; nmatch       Number of stars used to generate sigma

; Size tolerance, to rule out CRs and galaxies
  min_size = 0.65
  max_size = 5.0
; Ratio tolerance, to rule out galaxies or streaks or such
  max_R = 1.5  ;; 2.0 for really streaky images
  satur = 50000
  min_mag = 20.0

  fits_read,imfile,Iimg,Ihd
  fits_read,reffile,Rimg,Rhd

; If these images have already been converted to counts/sec, then
; there's a mismatch with the .stars files (which were made from the
; count images).
  timeratio = 1.0
  IF STRTRIM(SXPAR(Ihd,'BUNIT'),2) EQ 'counts/sec' THEN $
             timeratio = timeratio * SXPAR(Ihd,'WEXPTIME')
  IF STRTRIM(SXPAR(Rhd,'BUNIT'),2) EQ 'counts/sec' THEN $
             timeratio = timeratio / SXPAR(Rhd,'WEXPTIME')

  IF NOT KEYWORD_SET(imsky) THEN BEGIN
    IF STRTRIM(SXPAR(Ihd,'SKYSUB'),2) EQ 'Y' THEN BEGIN
      imsky = [0.0,SXPAR(Ihd,'SKYSIG')]
    ENDIF ELSE BEGIN
      imsky = [SXPAR(Ihd,'SKYLEV'),SXPAR(Ihd,'SKYSIG')] / (SXPAR(Ihd,'EXPTIME')/SXPAR(Ihd,'NCOMBINE'))
    ENDELSE
  ENDIF
  IF NOT KEYWORD_SET(refsky) THEN BEGIN
    IF STRTRIM(SXPAR(Rhd,'SKYSUB'),2) EQ 'Y' THEN BEGIN
      refsky = [0.0,SXPAR(Rhd,'SKYSIG')]
    ENDIF ELSE BEGIN
      refsky = [SXPAR(Rhd,'SKYLEV'),SXPAR(Rhd,'SKYSIG')] / (SXPAR(Rhd,'EXPTIME')/SXPAR(Rhd,'NCOMBINE'))
    ENDELSE
  ENDIF

  imsize = SIZE(Iimg)
  Ipixsize = SXPAR(Ihd,'XPIXSIZE')
  Rpixsize = SXPAR(Rhd,'XPIXSIZE')

  imstarfile = STRTRIM(imfile,2)+'.stars'
  refstarfile = STRTRIM(reffile,2)+'.stars'

  bugflag = KEYWORD_SET(debug)
  IF NOT KEYWORD_SET(nsig) THEN nsig = 3.0
  maxiter = 100

  IF bugflag THEN BEGIN
    PRINT,'Image: ',imstarfile
    PRINT,'Ref: ',refstarfile
    PRINT,'Skysig: ',imsky[1],refsky[1]
  ENDIF

  error = 0.0
  nmatch = 0
  readcol_new,refstarfile,refnumber,refX,refY,refMag,refflag,$
              refA,refB,refR,refFWHM,refClass,$
              FORMAT='(I,F,F,F,A,F,F,F,F,F)',COMMENT="#",/SILENT
  readcol_new,imstarfile,imnumber,imX,imY,imMag,imflag,$
              imA,imB,imR,imFWHM,imClass,$
              FORMAT='(I,F,F,F,A,F,F,F,F,F)',COMMENT="#",/SILENT

;  IF KEYWORD_SET(refoffset) THEN BEGIN
;    refX = refX + refoffset[0]
;    refY = refY + refoffset[1]
;  ENDIF

;  IF KEYWORD_SET(imoffset) THEN BEGIN
;    imX = imX + imoffset[0]
;    imY = imY + imoffset[1]
;  ENDIF

  reftemp = WHERE(refflag EQ '000' $
              AND refR LT max_R $
              AND refA LT max_size AND refB GT min_size $
              AND refX GT shift $
              AND refX LT ((imsize[1]-1)-shift) $
              AND refY GT shift $
              AND refY LT ((imsize[2]-1)-shift) $
              AND refMag LT min_mag $
              AND refFWHM GT 0.0,ref_count)
  IF ref_count LT 3 THEN BEGIN
    PRINT,"ERROR in calc_mdiff: reference image star file has no valid entries ",refstarfile
    RETURN,999.0
  ENDIF

  IF bugflag THEN BEGIN
    PRINT,"REF ",ref_count,N_ELEMENTS(refflag)
;;    FORPRINT,(refflag EQ '000'),(refR LT max_R),(refA LT max_size AND refB GT min_size)
  ENDIF

  imtemp = WHERE(imflag EQ '000' $
                 AND imR LT max_R $
                 AND imA LT max_size AND imB GT min_size $
                 AND imX GT shift $
                 AND imX LT ((imsize[1]-1)-shift) $
                 AND imY GT shift $
                 AND imY LT ((imsize[2]-1)-shift) $
                 AND imMag LT min_mag $
                 AND imFWHM GT 0.0,im_count)
  IF im_count LT 3 THEN BEGIN
    PRINT,"ERROR in calc_mdiff: image star file has no valid entries ",imstarfile
    RETURN,999.0
  ENDIF

  IF bugflag THEN BEGIN
    PRINT,"IM ",im_count,N_ELEMENTS(imflag)
;;    FORPRINT,(imflag EQ '000'),(imR LT max_R),(imA LT max_size AND imB GT min_size)
  ENDIF

; From the stars that passed this first check, find average FWHM to calculate
; the minimum seeing
  grm_avsigclip,refFWHM(reftemp),nsig,maxiter,$
                refseeing,refseesig,ngood,nbad,niter
  min_refMag = -2.5*ALOG10(nsig * refsky[1] * SQRT(!pi) * refseeing)

  grm_avsigclip,imFWHM(imtemp),nsig,maxiter,$
                imseeing,imseesig,ngood,nbad,niter
  min_imMag = -2.5*ALOG10(nsig * imsky[1] * SQRT(!pi) * imseeing)

; Now, add that one extra constraint.
  refgood = reftemp[WHERE(refMag[reftemp] LT (min_refMag + 25.0),refcount2)]
  imgood = imtemp[WHERE(imMag[imtemp] LT (min_imMag + 25.0),imcount2)]

  IF bugflag THEN BEGIN
    PRINT,'COUNT ',refcount2,imcount2
    PRINT,'SEEING ',refseeing,imseeing
  ENDIF

; Force 1:1 mapping within a radius of 2.0 arcsec or the worst seeing.
  tol = MAX([imseeing*Ipixsize,refseeing*Rpixsize,2.0])/3600.0
  IF Ipixsize GT 1.0 OR Rpixsize GT 1.0 THEN tol = 2.0*tol

; Convert X,Y to RA,Dec.  For matching an R to its narrow-band, this
; won't matter, but we also use this script for matching across
; different runs.
  xyad,Rhd,refX,refY,refRA,refDec

  IF KEYWORD_SET(offset) THEN BEGIN
; We already know the shift of the two images; good if the WCS was bad
; for the non-reference image.
    xyad,Rhd,imX+offset[0],imY+offset[1],imRA,imDec
  ENDIF ELSE BEGIN
    xyad,Ihd,imX,imY,imRA,imDec
  ENDELSE

  srcor2,imRA[imgood],imDec[imgood],refRA[refgood],refDec[refgood],$
        tol,imloc,refloc,option=1,/silent

  nmatch = N_ELEMENTS(imloc)
  IF N_ELEMENTS(imloc) NE N_ELEMENTS(refloc) THEN BEGIN
    PRINT,'ERROR in calc_mdiff: size mismatch from srcor2 ',nmatch,N_ELEMENTS(refloc)
    RETURN,999.0
  ENDIF

  IF nmatch LT 3 THEN BEGIN ; was 5
    IF imloc[0] LT 0 OR refloc[0] LT 0 THEN nmatch = 0
    PRINT,"ERROR in calc_mdiff: not enough valid objects ",nmatch
    PRINT,imstarfile,refstarfile
    PRINT,nmatch,imcount2,refcount2
    RETURN,999.0
  ENDIF

  igloc = imgood[imloc]
  rgloc = refgood[refloc]

; Note that imX[igloc[jj]] = ((imX[imgood])[imloc])[jj]

; Calculate deltaM for all matched stars, then run through grm_avsigclip
; to find the average deltaM, using that to determine the scale ratio

; Old method:
  mdiff = (imMag[igloc] - refMag[rgloc])-(2.5*ALOG10(timeratio))
  grm_avsigclip,mdiff,nsig,maxiter, $
                oldmode,sigma,ngood,nbad,niter
  IF KEYWORD_SET(fast) THEN BEGIN
; Don't bother with the more detailed aperture stuff.
    error = sigma
    RETURN,oldmode
  ENDIF

; New method:
  mdiff = FLTARR(nmatch)
  good = BYTARR(nmatch)+1b
  checkrad = 5.0 ; in arcsec

  IF Ipixsize GT 1.0 THEN outmult = 2.0 ELSE outmult = 1.4
  shift = LONG((outmult-1.0)*checkrad/Ipixsize)+1
  Icenter = LONG(checkrad/Ipixsize) + shift
  csize = 2*Icenter - 1
  dist_ellipse,circ,[csize,csize],Icenter,Icenter,1.0,45.0
  circ = circ*Ipixsize
  circind = WHERE(circ LT checkrad)
  y1 = FIX(circind/csize)
  x1 = circind - y1*csize
  skyind = WHERE(circ GE checkrad AND circ LT outmult*checkrad)
  IskyY = FIX(skyind/csize)
  IskyX = skyind - IskyY*csize

  IF Rpixsize GT 1.0 THEN outmult = 2.0 ELSE outmult = 1.4
  shift = LONG((outmult-1.0)*checkrad/Rpixsize)+1
  Rcenter = LONG(checkrad/Rpixsize) + shift
  csize = 2*Rcenter - 1
  dist_ellipse,circ,[csize,csize],Rcenter,Rcenter,1.0,45.0
  circ = circ*Rpixsize
  circind = WHERE(circ LT checkrad)
  y2 = FIX(circind/csize)
  x2 = circind - y2*csize
  skyind = WHERE(circ GE checkrad AND circ LT outmult*checkrad)
  RskyY = FIX(skyind/csize)
  RskyX = skyind - RskyY*csize

  FOR ii = 0,nmatch-1 DO BEGIN
    Ix = x1 + imX[igloc[ii]] - Icenter
    Iy = y1 + imY[igloc[ii]] - Icenter
    Ival = Iimg[Ix,Iy]
    Isky = Iimg[IskyX+imX[igloc[ii]]-Icenter,IskyY+imY[igloc[ii]]-Icenter]
    grm_avsigclip,Isky,nsig,maxiter,mode,sigma,ngood,nbad,iter
    Iflux = MEAN(Ival)-mode
;;    Iflux = MEAN(Ival-imsky[0])
;print,mode,imsky[0],sigma,imsky[1]

    Rx = x2 + refX[rgloc[ii]] - Rcenter
    Ry = y2 + refY[rgloc[ii]] - Rcenter
    Rval = Rimg[Rx,Ry]
    Rsky = Rimg[RskyX+refX[rgloc[ii]]-Rcenter,RskyY+refY[rgloc[ii]]-Rcenter]
    grm_avsigclip,Rsky,nsig,maxiter,mode,sigma,ngood,nbad,iter
    Rflux = MEAN(Rval)-mode
;;    Rflux = MEAN(Rval-refsky[0])

;print,Iflux,Rflux,2.5*ALOG10(Rflux/Iflux)
    
; Throw out any data points with negative fluxes, or with any
; saturated pixels at all.
    IF Iflux GT 0 AND Rflux GT 0 AND $
       MAX(Ival) LT satur AND MAX(Rval) LT satur THEN BEGIN
      mdiff[ii] = 2.5*ALOG10(Rflux/Iflux)
    ENDIF ELSE BEGIN
      good[ii] = 0b
    ENDELSE
  ENDFOR

  goodind = WHERE(good,goodcount)
  IF goodcount LT 3 THEN BEGIN
    PRINT,'ERROR in calc_mdiff: not enough good entries ',goodcount
    RETURN,999.0
  ENDIF
  grm_avsigclip,mdiff[goodind],nsig,maxiter, $
                mode,sigma,ngood,nbad,niter
  error = sigma

  IF bugflag THEN BEGIN
    PRINT,'Old way ',oldmode
    PRINT,'NMATCH ',nmatch
    PRINT,'Mdiff ',mode,' +/- ',sigma
    PRINT,'Time ratio ',timeratio
  ENDIF

  RETURN,mode

END
