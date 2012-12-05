PRO sigplot,input,CATALOG=catalog

; STEP 1: Initialize constants and dummy arrays
  edgebuffer=60
  nsig_init=3.0
  nsig_fin=3.0
  Xdum=FLTARR(2)
  Ydum=FLTARR(2)
  pi=ACOS(-1.0)

; STEP 2: Read the input file ("run??.catalog")
; Run from the run's root directory, where the catalog files are.
  IF KEYWORD_SET(catalog) THEN BEGIN
    IF NOT FILE_TEST(catalog) THEN BEGIN
      PRINT,"ERROR in run_sky: specified catalog file does not exist"
      RETURN
    ENDIF
  ENDIF ELSE BEGIN
    spawn,"ls run??.catalog",catlist

    IF N_ELEMENTS(catlist) NE 1 THEN BEGIN
      PRINT,"ERROR in run_sky: Need one and only one catalog file in the directory",N_ELEMENTS(catlist)
      RETURN
    ENDIF
    catalog = catlist[0]
  ENDELSE

  readcol_new,catalog,object,ellipsename,Rimage,Cimage,Simage,$
              format="A,A,A,A,A",comment='#'

  current = WHERE(STRMID(object,1,7) EQ STRMID(input,1,7), count)
; Drops the "j", just giving the object coordinates

  IF count NE 1 THEN BEGIN
     PRINT,"ERROR, wrong number of matches ",count
     RETURN
  ENDIF

  CD,object[current]

; STEP 3: Read the image and header information.  For the most part, what we 
; REALLY want from the header is the exposure time.
  fits_read, Rimage[current], Rimg, Rhd
  fits_read, Cimage[current], Cimg, Chd
  fits_read, Simage[current], Simg, Shd

; Read the headers to get filters and exposure times
  read_header, Rhd, junk1, Rexptime, Rtexptime, Rfilters, $
               Rncombine, Rbunit, Rbcomment
  read_header, Chd, junk1, Cexptime, Ctexptime, Cfilters, $
               Cncombine, Cbunit, Cbcomment
  read_header, Shd, junk1, Sexptime, Stexptime, Sfilters, $
               Sncombine, Sbunit, Sbcomment

; STEP 4: Define the ellipse.
; "ellipsename" is the ellipse.dat file, replace with code when needed
  readcol_new,ellipse,Dx,Dy,Px,Py,pa,a,b,FORMAT='F,F,F,F,F,F,F',comment='#'
  n_ellipses = N_ELEMENTS(Px)

; Convert to more standard units (logic copied from ellipse_mask):
  getrot,Rhd,rot,cdelt
  cdelt = ABS(cdelt)*60.
  am_pix = cdelt[0] ; arcmin per pixel
  scale = a[0]*am_pix

; Find the RA and Dec of the center point
  xyad,Rhd,Dx,Dy,racen,deccen
  diamaj = am_pix*a[0]
  diamin = am_pix*b[0]

; STEP 5: Create the coarse mask
  zmin = 0.75
  zmax = 3.0
  AreaZ = (zmax^2 - zmin^2)
; If the resulting area would be less than ~200 boxes before mask, increase AreaZ
;  Area_ratio = 500.0*bxw^2/(pi*a[0]*b[0])
;  IF (AreaZ LT area_ratio) THEN AreaZ = area_ratio
  dr = diamaj * 0.0
  dr = (SQRT((diamaj+diamin)^2 + 4*(AreaZ-1.0)*diamaj*diamin)-diamaj-diamin)/2.0

  PRINT,"Creating coarse mask"
; Initialize the coarse mask, using the R image as a baseline.
  mask = mask_ellipse_ann3(Rimg, Rhd, racen, deccen, (2*diamaj*zmin), (2*diamin*zmin),$
                           dr, pa, goodmask=1b, /amflag)
  mask = mask_edge(Rimg, Rhd, edgebuffer, /badmask, inmask=mask)
; mask at this point is 1b for the squares we want to reject

;  show_mask,mask
;  CD,".."
;  RETURN

; Now for the part unique to this algorithm.  
; We're going to step through box size to see what sigma it gives.

  smin = 20
  smax = 50
  num_sizes = smax-smin+1

  Skyres = FLTARR(num_sizes,3)

  FOR bxw = smin, smax DO BEGIN

  isize = bxw-smin

; STEP 6: Box the images
  PRINT,"Boxing the R image at width=",bxw
  mysky,Rimg,imsky,imsig,mask=mask,/silent
;  PRINT,"start sky",imsky,imsig
  box2boxbg2,Rimage[current],bxw,nsigma=nsig_init,/use_sky,$
            mask=mask,results=Rskyres,startguess=[imsky,imsig],num_boxes=oldnum,/silent
;  PRINT,"sky",Rskyres[0],Rskyres[1]
  sigdiff = 1.0
  skydiff = 1.0
  num_ratio = 1.0
  WHILE (sigdiff GT 0.01 AND skydiff GT 0.001 AND num_ratio GT 0.5) DO BEGIN 
    oldsky = Rskyres
    box2boxbg2,Rimage[current],bxw,nsigma=nsig_fin,/use_sky,$
              mask=mask,results=Rskyres,startguess=[Rskyres[0],Rskyres[1]],$
              boxinfo=Rboxdata,num_boxes=num_boxes,/silent
    sigdiff = ABS((oldsky[0] - Rskyres[0])/oldsky[1])
    skydiff = ABS((oldsky[0] - Rskyres[0])/oldsky[0])
    num_ratio = FLOAT(num_boxes)/FLOAT(oldnum)
;    PRINT,"sky",Rskyres[0],Rskyres[1]
;    PRINT,sigdiff,skydiff,num_ratio
  ENDWHILE

  Skyres[isize,0] = Rskyres[1]/Rskyres[0]

  PRINT,"Boxing the narrow-band image at width=",bxw
  mysky,Cimg,imsky,imsig,mask=mask,/silent
;  PRINT,"start sky",imsky,imsig
  box2boxbg2,Cimage[current],bxw,nsigma=nsig_init,/use_sky,$
            mask=mask,results=Cskyres,startguess=[imsky,imsig],num_boxes=oldnum,/silent
  sigdiff = 1.0
  skydiff = 1.0
  num_ratio = 1.0
  WHILE (sigdiff GT 0.01 AND skydiff GT 0.001 AND num_ratio GT 0.5) DO BEGIN 
    oldsky = Cskyres
    box2boxbg2,Cimage[current],bxw,nsigma=nsig_fin,/use_sky,$
              mask=mask,results=Cskyres,startguess=[Cskyres[0],Cskyres[1]],$
              boxinfo=Cboxdata,num_boxes=num_boxes,/silent
    sigdiff = ABS((oldsky[0] - Cskyres[0])/oldsky[1])
    skydiff = ABS((oldsky[0] - Cskyres[0])/oldsky[0])
    num_ratio = FLOAT(num_boxes)/FLOAT(oldnum)
;    PRINT,"sky",Cskyres[0],Cskyres[1]
;    PRINT,sigdiff,skydiff,num_ratio
  ENDWHILE 

  Skyres[isize,1] = Cskyres[1]/Cskyres[0]

;  PRINT,"Boxing the subtracted image at width=",bxw
; Occasionally, due to subtraction problems, the mask will be different for the subtracted
; image.  For now, assume the trimming is symmetric; it SHOULD use the header, but that won't
; have been modified to fit the new FOV.  Realistically, the image should have been fixed in 
; the first place, it'll screw up flux calibrations otherwise.
;  masksize = SIZE(mask)
;  imsize = SIZE(Simg)
;  IF masksize[4] NE imsize[4] THEN BEGIN ; element 4 is total number of pixels
;    tempmask = mask
;    xshift = (masksize[1]-imsize[1])/2
;    yshift = (masksize[2]-imsize[2])/2
;    mask = BYTARR(imsize[1],imsize[2])
;    FOR ii = 0, imsize[1]-1 DO BEGIN
;      FOR jj = 0, imsize[2]-1 DO BEGIN
;        mask[ii,jj] = tempmask[ii+xshift,jj+yshift]
;      ENDFOR
;    ENDFOR
;  ENDIF

;  mysky,Simg,imsky,imsig,mask=mask,/silent
;  PRINT,"start sky",imsky,imsig
;  box2boxbg2,Simage[current],bxw,nsigma=nsig_init,/use_sky,$
;            mask=mask,results=Sskyres,startguess=[imsky,imsig],num_boxes=oldnum,/silent
;  sigdiff = 1.0
;  skydiff = 1.0
;  num_ratio = 1.0
;  WHILE (sigdiff GT 0.01 AND skydiff GT 0.001 AND num_ratio GT 0.5) DO BEGIN 
;    oldsky = Sskyres
;    box2boxbg2,Simage[current],bxw,nsigma=nsig_fin,/use_sky,$
;              mask=mask,results=Sskyres,startguess=[Sskyres[0],Sskyres[1]],$
;              boxinfo=Sboxdata,num_boxes=num_boxes,/silent
;    sigdiff = ABS((oldsky[0] - Sskyres[0])/oldsky[1])
;    skydiff = ABS((oldsky[0] - Sskyres[0])/oldsky[0])
;    num_ratio = FLOAT(num_boxes)/FLOAT(oldnum)
;    PRINT,"sky",Sskyres[0],Sskyres[1]
;    PRINT,sigdiff,skydiff,num_ratio
;  ENDWHILE 

;  Skyres[isize,2] = Sskyres[1]/Sskyres[0]

  ENDFOR

  CD,".."

; Output time

  xx = INDGEN(num_sizes)+smin

  WDELETE
  DEVICE, RETAIN=2, DECOMPOSED=0
  WINDOW,XSIZE=800,YSIZE=800

  ymin = MIN([MIN(Skyres[*,0]),MIN(Skyres[*,1])])
  ymax = MAX([MAX(Skyres[*,0]),MAX(Skyres[*,1])])

  PLOT,xx,Skyres[*,0],XRANGE=[smin,smax],YRANGE=[ymin,ymax],COLOR=50,PSYM=4
  OPLOT,xx,Skyres[*,1],COLOR=100,PSYM=5
;  OPLOT,xx,Skyres[*,2],COLOR=150,PSYM=6

  pngfile = input+"_sigma.png"

  makepng,pngfile

END
