PRO sky_cal_single,infile,ELLIPSE=ellipse

; Takes a single input image, sky-subtracts it using the defaults for
; everything, and creates the new version.
; No interactive mode, no plotting, no output files.
  bxw=35
  setflag = 0b
  bwflag = 0b

  header_template = !singgdir+"/hdr_template2.dat"

  IF NOT FILE_TEST(infile) THEN BEGIN
    PRINT,"ERROR in sky_cal_single: Input file not found. ",infile
    RETURN
  ENDIF
  object = "j"+STRTRIM(STRMID(infile,1,7),2)

  IF NOT KEYWORD_SET(ellipse) THEN ellipse = object+"_ellipse.dat"
  IF NOT FILE_TEST(ellipse) THEN BEGIN
    PRINT,"ERROR in sky_cal_single: Ellipse file not found ",ellipse
    RETURN
  ENDIF

  plfile = STRMID(infile,0,STRLEN(infile)-5)+".pl.fits"

  fits_read, infile, img, hd
  IF FILE_TEST(plfile) THEN fits_read, plfile, plimg, plhd, /data_only

  exptime = SXPAR(hd,'EXPTIME',count=matches)
  IF matches LT 1 THEN BEGIN
    PRINT,"ERROR in sky_cal_single: unset exposure time"
    RETURN
  ENDIF
  buffer = SXPAR(hd,'BUFFER',count=matches)
  IF matches LT 1 THEN BEGIN
    PRINT,"ERROR in sky_cal_single: buffer not set, setting to 0"
    buffer = 0
  ENDIF

  read_ellipse_file,ellipse,n_ellipses,refname,Dx,Dy,Px,Py,pa, $
                    a_i,b_i,z_s,z_f,z_c
  a = a_i * z_s
  b = b_i * z_s

  Ztemp = MAKE_ARRAY(n_ellipses,/FLOAT,VALUE=1.0)

; Add in buffer:
  Px = Px + buffer
  Py = Py + buffer
  Dx = Dx + buffer
  Dy = Dy + buffer

; Note that a,b, and pa are arrays
  theta = (pa-90.0)*!dtor
  getrot,hd,rot,cdelt
  cdelt = ABS(cdelt)*3600.
  as_pix = cdelt[0] ; arcsec per pixel
  scale = a*as_pix

; Find the RA and Dec of the center point
  xyad,hd,Dx,Dy,racen,deccen
  diamaj = as_pix*a/60.0
  diamin = as_pix*b/60.0

  zmin = 0.5
  zmax = 3.0
; If the area would be less than ~200 boxes before mask, increase AreaZ
  AreaZ = 500.0*bxw^2/(!pi*a*b) > (zmax^2 - zmin^2)

  dr = (SQRT((diamaj+diamin)^2 + 4*(AreaZ-1.0)*diamaj*diamin) - $
        diamaj-diamin)/2.0

  tempmask = mask_ellipse_ann3(img,hd,racen, deccen, $
                               (2.0*diamaj*zmin),(2.0*diamin*zmin),$
                               dr,pa,goodmask=1b,/amflag)
  IF FILE_TEST(plfile) THEN mask = tempmask OR plimg LT 1.5 $
                       ELSE mask = tempmask
  mysky,img,imsky,imsig,mask=mask,/silent

  skyres = box_sky(infile,mask,[imsky,imsig],bxw,boxdata)

  zbinsize=0.02
  Zbin=FINDGEN(LONG(zmax/zbinsize)+1)*zbinsize

;;  setplotcolors
;;  DEVICE, RETAIN=2, DECOMPOSED=0
;;  WINDOW,XSIZE=800,YSIZE=800
;;  !P.MULTI=[0,0,1,0,0]

  FOR ii = 0,n_ellipses-1 DO BEGIN
    bindata,boxdata,skyres,zmin,zmax,zbinsize,SIZE(img),$
            Dx[ii],Dy[ii],a[ii],b[ii],theta[ii],Zbox,Zbinout,Skybin

    IF ii EQ 0 THEN BEGIN
      Zbox0 = Zbox
      Skybin0 = Skybin
    ENDIF

    IF n_ellipses EQ 1 THEN BEGIN
      title = STRTRIM(infile,2)
    ENDIF ELSE BEGIN
      title = STRTRIM(infile,2)+" ellipse #"+STRTRIM(STRING(ii),2)
    ENDELSE

;;    plot_skydist,Zbox,Zbin,boxdata,Skybin,skyres,bwflag,$
;;                 zmin,zmax,scale[ii],1.0,Ztemp[ii],title

  ENDFOR

; STEP 11: Create the final mask.
; modify the axis values
  diamaj = diamaj*Ztemp
  diamin = diamin*Ztemp
; If the resulting area would be less than ~100 boxes, increase AreaZ
  AreaZ = 250.0*bxw^2/(!pi*a*b) > 2.0

  dr = (SQRT((diamaj+diamin)^2.0 + $
       4.0*(AreaZ-1)*diamaj*diamin)-diamaj-diamin)/2.0

  PRINT,"Creating final mask"

; Now, narrow down the image to the donut we want, from Z' to SQRT(2)*Z', and
; ONLY around galaxy #0 (the one we want to subtract from the image).
  tempmask = mask_ellipse_ann3(img,hd,racen[0],deccen[0], $
                               2.0*diamaj[0],2.0*diamin[0], $
                               dr[0],pa[0],goodmask=1b,/amflag)
  IF FILE_TEST(plfile) THEN mask = tempmask OR plimg LT 1.5 $
                       ELSE mask = tempmask
; Again, mask=1b for the pixels we want to remove

; STEP 12: Now that we have new masks, feed the old box data into the new mask
; to determine a new sky level for each image.  While you're at it, update the
; header information with the new variables, correct for exposure time, and
; then write an output .fits image.
  PRINT,"For object ",object,":"

  sky = calc_box_sky(boxdata,mask,bxw,Dx[0],Dy[0],theta[0],a[0],b[0])
  PRINT,sky[0],sky[1],$
        FORMAT='("Image sky is: ",F9.6," +/- ",F8.6," counts/sec")'

;;  !P.MULTI=[0,0,1,0,0]
;;  plot_skydist,Zbox0,Zbin,boxdata,Skybin0,sky, $
;;               bwflag,zmin,zmax,scale[0],1.0,Ztemp[0],infile

  img = img - sky[0]
  write_ssfits,infile,img,hd,Dx[0],Dy[0],Px[0],Py[0],sky,imsig, $
               header_template,racen[0],deccen[0],diamin[0],diamaj[0], $
               dr[0],pa[0],bxw

  PRINT,"Object "+object+" completed."
  CLOSE,/ALL

END
