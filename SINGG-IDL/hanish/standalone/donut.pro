PRO donut
; Sanity-check script for Mark's images.

  infile = "/data/m100_fuv_d25_cln_str.fits"
  outfile = "/data/donut.out"
  fits_read,infile,img,hd
  sz = SIZE(img)
  getrot,hd,rot,cdelt
  cdelt = ABS(cdelt)*60.
  am_pix = cdelt[0] ; arcmin per pixel

  bxw = 35
  nsig_init = 3.0
  nsig_fin = 5.0
  rejfrac = 0.01
  pa = 0.0

  AreaZ = 2.0 ; go from X to root2 X
  holerad = 195 ; in pixels
  donutrad = holerad*SQRT(AreaZ)

; Circular donut at the location of the galaxy:

  num_donuts_X = 4
  num_donuts_Y = 4

  tot_donuts = (num_donuts_X * num_donuts_Y) + 1
; Element 0 is the center.  Element 1-* are periodically-placed donuts.

  Xcen = FLTARR(tot_donuts)
  Ycen = FLTARR(tot_donuts)

  FOR xx = 0,num_donuts_X-1 DO BEGIN
    FOR yy = 0,num_donuts_Y-1 DO BEGIN
      index = (yy*num_donuts_X + xx) + 1
      Xcen[index] = donutrad + (FLOAT(xx)/FLOAT(num_donuts_X - 1))*(sz[1]-(2*donutrad))
      Ycen[index] = donutrad + (FLOAT(yy)/FLOAT(num_donuts_Y - 1))*(sz[2]-(2*donutrad))
    ENDFOR
  ENDFOR

; Location of the galaxy
  Xcen[0] = 531
  Ycen[0] = 529

  diamaj = am_pix*holerad

  dr = (SQRT(AreaZ)-1.0) * diamaj

  holeflux = FLTARR(tot_donuts)
  donutflux = FLTARR(tot_donuts)
  avhole = FLTARR(tot_donuts)

  GET_LUN,unit
  OPENW,unit,outfile

  PRINTF,unit,'Index  x    y    RA        dec     donut(sky,  sig)        '+$
        'hole(sky,   sig)        flux        ave        Npix'

  FOR ii=0,tot_donuts-1 DO BEGIN
; Find the RA and Dec of the center point
    xyad,hd,Xcen[ii],Ycen[ii],racen,deccen

    donutmask = mask_ellipse_ann3(img,hd,racen,deccen,$
         (2.0*diamaj),(2.0*diamaj),dr,pa,goodmask=1b, /amflag)
; donutmask = 1b for pixels inside or outside the donut
    holemask = mask_ellipse_ann3(img,hd,racen,deccen,$
         (2.0*diamaj),(2.0*diamaj),-1.0,pa,goodmask=1b, /amflag)
; holemask = 1b for pixels outside the donut hole

; First, find the sky level in the donut
    mysky,img,imdsky,imdsig,mask=donutmask,/silent
    box2boxbg2,infile,bxw,nsigma=nsig_init,/use_sky,$
             mask=donutmask,results=donutsky,startguess=[imdsky,imdsig],$
             num_boxes=oldnum,reject_frac=rejfrac,/silent
    sigdiff = 1.0
    skydiff = 1.0
    num_ratio = 1.0
    WHILE (sigdiff GT 0.01 AND skydiff GT 0.001 AND num_ratio GT 0.5) DO BEGIN 
      oldsky = donutsky
      box2boxbg2,infile,bxw,nsigma=nsig_fin,/use_sky,$
                 mask=donutmask,results=donutsky,startguess=[donutsky[0],donutsky[1]],$
                 boxinfo=boxdata,num_boxes=num_boxes,reject_frac=rejfrac,/silent
      sigdiff = ABS((oldsky[0] - donutsky[0])/oldsky[1])
      skydiff = ABS((oldsky[0] - donutsky[0])/oldsky[0])
      num_ratio = FLOAT(num_boxes)/FLOAT(oldnum)
;      PRINT,"sky",donutsky[0],donutsky[1]
;      PRINT,sigdiff,skydiff,num_ratio
    ENDWHILE

    PRINT,"Donut sky level for number ",ii," = ",donutsky[0]," +/- ",donutsky[1]

; Idiot check on hole sky:
    mysky,img,imhsky,imhsig,mask=holemask,/silent
    box2boxbg2,infile,bxw,nsigma=nsig_init,/use_sky,$
             mask=holemask,results=holesky,startguess=[imhsky,imhsig],$
             num_boxes=oldnum,reject_frac=rejfrac,/silent
    sigdiff = 1.0
    skydiff = 1.0
    num_ratio = 1.0
    WHILE (sigdiff GT 0.01 AND skydiff GT 0.001 AND num_ratio GT 0.5) DO BEGIN 
      oldsky = holesky
      box2boxbg2,infile,bxw,nsigma=nsig_fin,/use_sky,$
                 mask=holemask,results=holesky,startguess=[holesky[0],holesky[1]],$
                 boxinfo=boxdata,num_boxes=num_boxes,reject_frac=rejfrac,/silent
      sigdiff = ABS((oldsky[0] - holesky[0])/oldsky[1])
      skydiff = ABS((oldsky[0] - holesky[0])/oldsky[0])
      num_ratio = FLOAT(num_boxes)/FLOAT(oldnum)
;      PRINT,"sky",holesky[0],holesky[1]
;      PRINT,sigdiff,skydiff,num_ratio
    ENDWHILE

    PRINT,"Hole sky level for number ",ii," = ",holesky[0]," +/- ",holesky[1]

    holearea = LONG(0)
    donutarea = LONG(0)
; Then, find the sky-subtracted flux of everything inside the hole
    FOR xx=0,sz[1]-1 DO BEGIN
      FOR yy=0,sz[2]-1 DO BEGIN
;        IF ABS(xx-Xcen[ii]) LT holerad AND $
;           ABS(yy-Ycen[ii]) LT holerad AND $
        IF NOT holemask[xx,yy] THEN BEGIN
          holeflux[ii] = holeflux[ii] + (img[xx,yy]-donutsky[0])
          holearea = holearea + 1
        ENDIF
        IF NOT donutmask[xx,yy] THEN BEGIN
          donutflux[ii] = donutflux[ii] + (img[xx,yy]-donutsky[0])
          donutarea = donutarea + 1
        ENDIF
      ENDFOR
    ENDFOR

    avhole[ii] = holeflux[ii]/holearea

    PRINT,"Subtracted flux: ",holeflux[ii],"; average = ",avhole[ii],"; n=",holearea

    PRINTF,unit,ii,Xcen[ii],Ycen[ii],racen,deccen,$
                   donutsky[0],donutsky[1],holesky[0],holesky[1],$
                   holeflux[ii],avhole[ii],holearea,$
      FORMAT='(I2,"    ",2(I4," "),2(F8.3," "),6(E11.4," "),I6)'

  ENDFOR

  CLOSE,/ALL
  FREE_LUN,unit

END
