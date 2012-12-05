PRO box2boxbg3,img, bxw, Nsigma=Nsigma, mask=mask, use_sky=use_sky, $
               startguess=startguess, outfile=outfile, nodisplay=nodisplay,$
               results=results, boxinfo=boxinfo, num_boxes=num_boxes, $
               silent=silent,reject_frac=reject_frac

; box2box background levels
; Needs Astrolib
; requires stv.pro for display
;
; file      - fits file name
; bxw       - box width in pixels
; Nsigma    - exclude boxes with mean >(<) image mean+(-)Nsigma*rms  
; mask      - 2D binary array same size as image. Pixels = 1b are exclude
; use_sky   - use astrolib sky routine to get image mean and rms
;             as opposed to just taking the image mean and rms.
;             This is ussually a better option.
; startguess- optional array that provides a starting value for the
;             image [sky,sigma] instead of taking the image mean and rms 
;             or using the Astrolib sky routine. The use_sky keyword
;             will be used for box and box2box means if specified.
;
; outfile   - string of named outputfile
; nodisplay - do not display image and boxes
;
; results   - named array to contain output [sky,sigma,sdom]
; boxinfo   - named array to contain output boxmean, x, and y in 3xn array
; num_boxes - total number of boxes in boxinfo
; silent    - set keyword to prevent all printing to screen
; reject_frac - rejection fraction; only throw out a box if more than this 
;               fraction of pixels are masked.
;
; M. Seibert 11/01
;
;example of self iteration: 
;
; box2boxbg,'name.fits', 50, Nsigma=3.,/use_sky,$
;           results=results,mask=mask
; print,result
; diff=1
;
; WHILE diff GT .01 DO BEGIN
;  oldresults=results
;  box2boxbg,'name.fits', 50, Nsigma=2.,mask=mask,$
;            results=results,startguess=[results[0],results[1]],/use_sky
;  diff=abs((oldresults[0]-results[0])/oldresults[0])
; ENDWHILE
;
  IF n_params() NE 2 THEN BEGIN
    PRINT,"USAGE: box2boxbg, file, bxw [, Nsigma=Nsigma, $"
    PRINT,"        mask=mask, use_sky=use_sky, outfile=outfile"
    PRINT,"        startguess=startguess,results=results, silent=silent]"
    RETURN
  ENDIF

; Reject_frac is the fraction of pixels within each box that have to
; be masked before we just go ahead and chuck the whole box.  1.0
; means to never throw out boxes.
  IF NOT KEYWORD_SET(reject_frac) THEN reject_frac = 1.0

;---------------------------------------------------------------
;display image -- will scale image from -3 to +7 sigma
  IF NOT KEYWORD_SET(nodisplay) THEN stv,img,/auto 

  IF NOT KEYWORD_SET(silent) THEN PRINT,'working...'

;---------------------------------------------------------------
;define boxes
  sz=size(img)
  sx=sz[1]
  sy=sz[2]

  num_x = long(FIX(sx/(bxw+2)))
  num_y = long(FIX(sy/(bxw+2)))
  n_boxes = num_x*num_y

  boxinfo = FLTARR(3,n_boxes)

  xc = INDGEN(num_x)*(bxw+2) + 1
  yc = INDGEN(num_y)*(bxw+2) + 1

  goodbox = MAKE_ARRAY(num_x,num_y,/BYTE,value=1b)

;---------------------------------------------------------------
;exclude boxes with values greater than image mean + Nsigma*rms
  IF NOT KEYWORD_SET(Nsigma) THEN BEGIN 
    limit_hi = MAX(img)
    limit_lo = MIN(img)
  ENDIF ELSE BEGIN 
    IF NOT KEYWORD_SET(startguess) THEN begin
; If mask is being used, mask out the image
      IF NOT KEYWORD_SET(use_sky) THEN begin
        m_im=moment(img,maxmoment=2,/NAN,/double)
        limit_hi = m_im[0] + Nsigma*SQRT(m_im[1])
        limit_lo = m_im[0] - Nsigma*SQRT(m_im[1])
      ENDIF ELSE BEGIN
        IF KEYWORD_SET(mask) THEN BEGIN
           mysky,img,imsky,imskysig,mask=mask,/SILENT
        ENDIF ELSE mysky,img,imsky,imskysig,/SILENT
        limit_hi = imsky + Nsigma*imskysig
        limit_lo = imsky - Nsigma*imskysig
      ENDELSE 
    ENDIF ELSE BEGIN
      limit_hi = startguess[0] + Nsigma*startguess[1]
      limit_lo = startguess[0] - Nsigma*startguess[1]
    ENDELSE 
  ENDELSE

  FOR ii=0l,num_x-1l DO BEGIN
    FOR jj=0l,num_y-1l DO BEGIN
      mskcount=0
;      PLOTS,[xc[ii],xc[ii]+bxw,xc[ii]+bxw,xc[ii]],[yc[jj],yc[jj],yc[jj]+bxw,yc[jj]+bxw]
      boximg = img[xc[ii]:xc[ii]+bxw,yc[jj]:yc[jj]+bxw]
      IF KEYWORD_SET(mask) THEN BEGIN
        boxmask = mask[xc[ii]:xc[ii]+bxw,yc[jj]:yc[jj]+bxw]
        masklist = WHERE(boxmask EQ 1b,mskcount)
      ENDIF

      IF (mskcount GT (reject_frac)*(bxw^2)) THEN BEGIN
; If a box has too many bad pixels, throw the entire box out.
        goodbox[ii,jj] = 0b
      ENDIF ELSE BEGIN
;        PLOTS,[xc[ii],xc[ii]+bxw],[yc[jj]+bxw,yc[jj]]       
        IF KEYWORD_SET(use_sky) THEN BEGIN 
          IF KEYWORD_SET(mask) THEN BEGIN
;            mmm2,boximg,boxsky,boxskysig
; Note that mmm's boxsky is a MODE, not a mean
            mysky,boximg,boxsky,boxskysig,mask=boxmask,/SILENT
          ENDIF ELSE BEGIN
            mysky,boximg,boxsky,boxskysig,/SILENT
          ENDELSE
        ENDIF ELSE BEGIN
          tempbox=moment(boximg,maxmoment=1,/NAN,/double)
          boxsky = tempbox[0]
        ENDELSE
        IF ((boxsky GT limit_hi) OR (boxsky LT limit_lo)) THEN goodbox[ii,jj] = 0b
      ENDELSE
    ENDFOR
  ENDFOR

  good = WHERE(goodbox,num_boxes)
  IF num_boxes LT 2 THEN BEGIN
    PRINT,"ERROR: No boxes - all excluded due to Nsigma or mask ",num_boxes
    results=[0,0]
    RETURN
  ENDIF 
  ygood = LONG(good / num_x)
  xgood = LONG(good) - ygood*num_x
  boxinfo = FLTARR(3,num_boxes)
  meanboxflux = FLTARR(num_boxes)

;draw boxes at the locations defined by corners
;tv box reguires center position
  IF NOT KEYWORD_SET(nodisplay) THEN BEGIN
    FOR ii=0l,num_x-1l DO BEGIN
      FOR jj=0l,num_y-1l DO BEGIN
        IF goodbox[ii,jj] THEN tvbox,bxw,xc[ii]+bxw/2,yc[jj]+bxw/2,color=!d.n_colors-1
      ENDFOR
    ENDFOR
  ENDIF
;---------------------------------------------------------------
;compute box2box sky values

;mean of each non-masked box
  FOR ii = 0l,num_boxes-1l DO BEGIN
    boximg = img[xc[xgood[ii]]:xc[xgood[ii]]+bxw,yc[ygood[ii]]:yc[ygood[ii]]+bxw]
    IF KEYWORD_SET(use_sky) THEN BEGIN 
      IF KEYWORD_SET(mask) THEN BEGIN
        boxmask = mask[xc[xgood[ii]]:xc[xgood[ii]]+bxw,yc[ygood[ii]]:yc[ygood[ii]]+bxw]
;        mmm2,boximg,boxsky,boxskysig
        mysky,boximg,boxsky,boxskysig,mask=boxmask,/SILENT
      ENDIF ELSE BEGIN
        mysky,boximg,boxsky,boxskysig,/SILENT
      ENDELSE
    ENDIF ELSE boxsky=moment(boximg,maxmoment=1,/NAN,/double)
    meanboxflux[ii] = boxsky[0]
  ENDFOR

;box2box mean, std.dev, & std dev of mean
  boxmoments = moment(meanboxflux,/DOUBLE)
  ambflux    = boxmoments[0]           ; amb  -> avg. mean box
  ambfsigma  = SQRT(boxmoments[1])   
  ambfsdom   = ambfsigma/SQRT(num_boxes) ; sdom -> std. dev of mean
;place results into an array for return
  results=[ambflux,ambfsigma,ambfsdom]

  boxinfo[0,*] = meanboxflux
  boxinfo[1,*] = xc[xgood]+bxw/2
  boxinfo[2,*] = yc[ygood]+bxw/2

;---------------------------------------------------------------
;write output
  IF NOT KEYWORD_SET(silent) THEN BEGIN
    IF NOT KEYWORD_SET(outfile) THEN outfile = '/dev/tty'

    OPENW, outunit, outfile, /GET_LUN, /MORE

    PRINTF, outunit, '=================================================='
    PRINTF, outunit, 'Box2boxbg: '+ systime(0)
;;    PRINTF, outunit, 'Input file: '+file

    IF KEYWORD_SET(Nsigma) THEN BEGIN 
      PRINTF, outunit, 'Pix-to-Pix Exclusion Level: ',Nsigma,' Nsigma'
    ENDIF ELSE BEGIN
      PRINTF, outunit, 'Pix-to-Pix Exclusion Level: none'
    ENDELSE

    IF KEYWORD_SET(use_sky) THEN BEGIN 
      PRINTF, outunit, 'Use_sky: yes'
    ENDIF ELSE BEGIN
      PRINTF, outunit, 'Use_sky: no'
    ENDELSE 

    PRINTF, outunit, '=================================================='
    PRINTF, outunit, 'Avg. Box-to-Box Mean Sky Per Pixel: '
    PRINTF, outunit, '--------------------------------------------------'
    PRINTF, outunit
    PRINTF, outunit, '  number of boxes: ',num_boxes
    PRINTF, outunit, '  box width (pixels):',bxw
    PRINTF, outunit
    PRINTF, outunit, '  sky   =',ambflux
    PRINTF, outunit, '  sigma =',ambfsigma
    PRINTF, outunit, '  sdom  =',ambfsdom
    PRINTF, outunit

    CLOSE, outunit
    FREE_LUN, outunit
  ENDIF

  RETURN

END



