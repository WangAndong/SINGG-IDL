PRO sky_calibration,RFILE=Rfile,NFILE=Nfile,SFILE=Sfile,ELLIPSE=ellipse, $
                    INDIR=indir,OUTDIR=outdir, $
                    RUNID=runid,OBJ=obj, $
                    RPLFILE=Rplfile,NPLFILE=Nplfile,SPLFILE=Splfile, $
                    RMASKFILE=Rmaskfile,NMASKFILE=Nmaskfile,SMASKFILE=Smaskfile, $
                    OUTPUT=output,HEADER_TEMPLATE=header_template, $
                    FAST=fast,NOMASK=nomask,NOFITS=nofits, $
                    BOXDUMP=boxdump,COUNTS=counts,BW=bw
; This program determines the sky level around all galaxies in the run, and
; writes sky-subtracted .fits files corrected by these values.

; OPTIONAL INPUTS
; Rfile         Name of combined R image(if not set, J???????_R.fits)
; Nfile         Name of narrow-band image(if not set, J???????_6???.fits)
; Sfile         Name of the subtracted image(if not set, J???????_Rsub.fits)
;               The only time these should be different is when multiple 
;               filters are used on the same object, in which case this
;               script must be run multiple times, which usually requires
;               multiple lines in the catalog file.  While Rfile and Nfile
;               can stay the same for the different filters, the Sfile needs
;               clarification.
; indir         Input directory (default is the current one)
; outdir        Output directory (default is the current one)
; ellipse       Name of the ellipse parameter file (located in outdir)
; Rplfile       Name of the R-band pixel file (if not set, (Rfile).pl.fits)
; Nplfile       Name of narrow-band pixel file (if not set, (Nfile).pl.fits)
; Splfile       Name of the subtracted pixel file (if not set, (Sfile).pl.fits)
; Rmaskfile     Optional mask files to remove stars from calculations.
; Nmaskfile       If not set, only the .pl files (above) will be used
; Smaskfile       to mask stars.
; output        The optional sky output file.  As long as it's defined, add the
;               data to that file in addition to the header modification.
; header_template Template for header reduction.  If not set, it uses a 
;               hard-coded machine-specific default.
; /fast         Skips the interactive mode.  If the Z in the _ellipse.dat is 
;               negative, it'll ignore this on an object-by-object basis.
; /nofits       Do not create the _ss.fits files.  Rarely used.
; /nomask       Do not use any mask files.  Don't use unless you're testing.
; /boxdump      Creates data tables where each row is x,y,Z,sky[0], 
;               and a logical flag of whether it was used in the final step
; /counts       Input image is in total counts, not counts per second
; /bw           Sets plot to black and white

; NOTE: "!singgdir" is an environmental variable that must be set as part of
; your ~/.idlstartup script.  Set it to a directory containing the template
; file, basic mask file, and so on.

; STEP 1: Initialize constants and dummy arrays
  bxw=35
  setflag = 0b
  fastflag = KEYWORD_SET(fast)

  countflag = KEYWORD_SET(counts)
  dumpflag = KEYWORD_SET(boxdump)
  bwflag = KEYWORD_SET(bw)
  fitsflag = NOT KEYWORD_SET(nofits)
  IF NOT KEYWORD_SET(header_template) THEN $
       header_template = !singgdir+"/hdr_template2.dat"

  spawn,"pwd",cdir

; An easier way to specify directories:
  IF KEYWORD_SET(obj) AND KEYWORD_SET(runid) THEN BEGIN
    indir = STRTRIM(cdir[0],2)+'/'+STRTRIM(runid,2)+'/Proc3/'+STRTRIM(obj,2)+'/'
    outdir = STRTRIM(cdir[0],2)+'/'+STRTRIM(runid,2)+'/Proc4/'+STRTRIM(obj,2)+'/'
  ENDIF

; indir = Run*/Proc3/Jwhatever
  IF KEYWORD_SET(indir) THEN BEGIN
    idir = STRTRIM(indir,2)
; If it doesn't end in a slash, add one.
    IF STRMID(idir,0,1,/reverse_offset) NE '/' THEN idir = idir+'/'
; If it's a relative path (either './' or nothing), add the pwd.
    IF STRMID(idir,0,2) EQ './' THEN idir = STRTRIM(cdir[0],2)+STRMID(idir,1,STRLEN(idir)-1)
    IF STRMID(idir,0,1) NE '/' THEN idir = STRTRIM(cdir[0],2)+'/'+idir
  ENDIF ELSE BEGIN
    idir = STRTRIM(cdir[0],2)+'/'
  ENDELSE

; outdir = Run*/Proc4/Jwhatever
  IF KEYWORD_SET(outdir) THEN BEGIN
    odir = STRTRIM(outdir,2)
; If it doesn't end in a slash, add one.
    IF STRMID(odir,0,1,/reverse_offset) NE '/' THEN odir = odir+'/'
; If it's a relative path (either './' or nothing), add the pwd.
    IF STRMID(odir,0,2) EQ './' THEN odir = STRTRIM(cdir[0],2)+STRMID(odir,1,STRLEN(odir)-1)
    IF STRMID(odir,0,1) NE '/' THEN odir = STRTRIM(cdir[0],2)+'/'+odir
  ENDIF ELSE BEGIN
    odir = STRTRIM(cdir[0],2)+'/'
  ENDELSE

  IF STRPOS(odir,'Run04s') GE 0 THEN rejfrac = 0.1 ELSE rejfrac = 0.01

  IF KEYWORD_SET(Rfile) THEN BEGIN $
    IF NOT FILE_TEST(idir+Rfile) THEN BEGIN
      PRINT,"ERROR in sky_calibration: R file not found. ",Rfile
      RETURN
    ENDIF
  ENDIF ELSE BEGIN
; Rfile wasn't specified, so we need to find it.
; Assume Rfile will be of the form *_C.fits or *_R.fits.
    spawn,"ls "+idir+"*_?.fits",Rlist

    IF N_ELEMENTS(Rlist) GT 1 THEN BEGIN
      PRINT,"ERROR in sky_calibration: Multiple combined continuum images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the RFILE keyword explicitly."
      RETURN
    ENDIF

    IF NOT FILE_TEST(Rlist[0]) THEN BEGIN
      PRINT,"ERROR in sky_calibration: No combined R-band images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the RFILE keyword explicitly."
      RETURN
    ENDIF

    Rfile = STRMID(Rlist[0],STRLEN(idir),STRLEN(Rlist[0])-STRLEN(idir))
    PRINT,"R image used: ",Rfile
  ENDELSE
  object = STRTRIM(STRMID(Rfile,0,STRLEN(Rfile)-7),2)
  IF NOT KEYWORD_SET(Rplfile) THEN $
    Rplfile = STRMID(Rfile,0,STRLEN(Rfile)-5)+".pl.fits"

; For narrow-band, it's a bit more complex since there could be multiple
; narrow-band filters used for this object.
  IF KEYWORD_SET(Nfile) THEN BEGIN $
    IF NOT FILE_TEST(idir+Nfile) THEN BEGIN
      PRINT,"ERROR in sky_calibration: Narrow-band file not found. ",Nfile
      RETURN
    ENDIF
  ENDIF ELSE BEGIN
; Nfile wasn't specified, so we need to find it.
; Assume Nfile will be of the form *_6XXX.fits.
    spawn,"ls "+idir+"*_6???.fits",Nlist

    IF N_ELEMENTS(Nlist) GT 1 THEN BEGIN
      PRINT,"ERROR in sky_calibration: Multiple combined narrow-band images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the NFILE keyword explicitly."
      RETURN
    ENDIF

    IF NOT FILE_TEST(Nlist[0]) THEN BEGIN
      PRINT,"ERROR in sky_calibration: No combined narrow-band images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the NFILE keyword explicitly."
      RETURN
    ENDIF

    Nfile = STRMID(Nlist[0],STRLEN(idir),STRLEN(Nlist[0])-STRLEN(idir))
    PRINT,"Narrow-band image used: ",Nfile
  ENDELSE
  filter = STRMID(Nfile,STRLEN(Nfile)-9,4)
  IF NOT KEYWORD_SET(Nplfile) THEN $
    Nplfile = STRMID(Nfile,0,STRLEN(Nfile)-5)+".pl.fits"

  IF KEYWORD_SET(Sfile) THEN BEGIN $
    IF NOT FILE_TEST(idir+Sfile) THEN BEGIN
      PRINT,"ERROR in sky_calibration: Subtracted file not found. ",Sfile
      RETURN
    ENDIF
  ENDIF ELSE BEGIN
; Sfile wasn't specified, so we need to find it.
; Assume Sfile will be of the form *_Rsub.fits.
    spawn,"ls "+idir+"*_?sub.fits",Slist

    IF N_ELEMENTS(Slist) GT 1 THEN BEGIN
      PRINT,"ERROR in sky_calibration: Multiple combined subtracted images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the SFILE keyword explicitly."
      RETURN
    ENDIF

    IF NOT FILE_TEST(Slist[0]) THEN BEGIN
      PRINT,"ERROR in sky_calibration: No combined subtracted images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the SFILE keyword explicitly."
      RETURN
    ENDIF

    Sfile = STRMID(Slist[0],STRLEN(idir),STRLEN(Slist[0])-STRLEN(idir))
    PRINT,"Subtracted image used: ",Sfile
  ENDELSE
  IF NOT KEYWORD_SET(Splfile) THEN $
    Splfile = STRMID(Sfile,0,STRLEN(Sfile)-5)+".pl.fits"

  IF NOT KEYWORD_SET(ellipse) THEN ellipse=object+"_ellipse.dat"
  IF NOT FILE_TEST(odir+ellipse) THEN BEGIN
    PRINT,"ERROR in sky_calibration: Ellipse file not found. ",ellipse
    RETURN
  ENDIF

; Now, check for mask files to rule out some stars entirely.
  IF NOT KEYWORD_SET(nomask) THEN BEGIN
    spawn,"ls "+idir+"*_?sub.fits",Slist
    multflag = N_ELEMENTS(Slist) GT 1
    IF multflag THEN Rbase = STRMID(Rfile,0,STRLEN(Rfile)-5) $
                ELSE Rbase = STRMID(Rfile,0,STRLEN(Rfile)-7)

    IF NOT KEYWORD_SET(Rmaskfile) THEN Rmaskfile = Rbase+'_mask.fits'

    IF NOT FILE_TEST(odir+Rmaskfile) THEN BEGIN
      PRINT,"ERROR in sky_calibration: R-band mask file does not exist ",Rmaskfile
      RETURN
    ENDIF

    fits_read,odir+Rmaskfile,Rmaskimg,mhd,/data_only

    IF NOT KEYWORD_SET(Nmaskfile) THEN Nmaskfile = Rmaskfile

    IF NOT FILE_TEST(odir+Nmaskfile) THEN BEGIN
      PRINT,"ERROR in sky_calibration: narrow-band mask file does not exist ",Nmaskfile
      RETURN
    ENDIF

    fits_read,odir+Nmaskfile,Nmaskimg,mhd,/data_only

    IF NOT KEYWORD_SET(Smaskfile) THEN $
         Smaskfile = STRMID(Sfile,0,STRLEN(Sfile)-5)+'_mask.fits'

    IF NOT FILE_TEST(odir+Smaskfile) THEN BEGIN
      PRINT,"ERROR in sky_calibration: subtracted mask file does not exist ",Smaskfile
      RETURN
    ENDIF

    fits_read,odir+Smaskfile,Smaskimg,mhd,/data_only
  ENDIF

; STEP 4: Read the image and header information.  For the most part, what we 
; REALLY want from the header is the exposure time.

  fits_read, idir+Rfile, Rimg, Rhd
  fits_read, idir+Nfile, Nimg, Nhd
  fits_read, idir+Sfile, Simg, Shd

; Since so little info was needed, I just bypassed the whole read_header
; routine and just grabbed the specific things I needed.
  Rtime = SXPAR(Rhd,'EXPTIME')
  Ntime = SXPAR(Nhd,'EXPTIME')
  Stime = SXPAR(Shd,'EXPTIME',count=matches)
  IF matches LT 1 THEN Stime = Ntime
; Some times, for whatever reason, runalard forgets to write exposure time.
; Note that this is TOTAL exposure time, the sum of all combined images.
; If you want average time, divide by N

; Buffer is the number of pixels the image is padded along each edge
  buffer = SXPAR(Rhd,'BUFFER',count=matches)
  IF matches LT 1 THEN buffer = 0
  buffN = SXPAR(Nhd,'BUFFER',count=matchN)
  IF matchN NE 0 AND buffN NE buffer THEN BEGIN
    PRINT,"ERROR in sky_calibration; can't mix different edge buffer sizes",$
          buffer,buffN
    RETURN
  ENDIF

; Determine whether the image will be photometric.
  refR = SXPAR(Rhd,'FLUXREF',count=Rrcount)
  refN = SXPAR(Nhd,'FLUXREF',count=Nrcount)
  IF Rrcount NE 1 OR Nrcount NE 1 THEN BEGIN
    PRINT,'ERROR in sky_calibration: flux reference images not defined ',Rrcount,Nrcount
    RETURN
  ENDIF
  Rrnum = LONG(STRMID(refR,3,7))
  Nrnum = LONG(STRMID(refN,3,7))

  datfile = !singgdir+'nonphot.dat'
  readcol_new,datfile,objstart,objend,FORMAT='A,A',COMMENT='#',/SILENT
  refind = WHERE((LONG(objstart) LE Rrnum AND LONG(objend) GE Rrnum),refcount)
  IF refcount GT 0 THEN Rphot = 'N' ELSE Rphot = 'Y'
  refind = WHERE((LONG(objstart) LE Nrnum AND LONG(objend) GE Nrnum),refcount)
  IF refcount GT 0 THEN Nphot = 'N' ELSE Nphot = 'Y'
  IF Rphot EQ 'Y' AND Nphot EQ 'Y' THEN Sphot = 'Y' ELSE Sphot = 'N'

; STEP 5: Correct the input images for the exposure time.  By default, input 
; images are already in total counts/sec of all combined images, and you 
; wouldn't need any change.  But, the old IRAF combine algorithm gave images 
; in "Average Total Counts"; that is, total counts for a single image with the
; average exposure time

  IF countflag THEN BEGIN
    Rnum = SXPAR(Rhd,'NCOMBINE')
    Nnum = SXPAR(Nhd,'NCOMBINE')
    Snum = Nnum ; There are no actual images being combined for _Rsub

    Rimg = Rimg * Rnum / Rtime
    Nimg = Nimg * Nnum / Ntime
    Simg = Simg * Snum / Stime
  ENDIF

; STEP 6: Define the ellipse.
  read_ellipse_file,odir+ellipse,n_ellipses,refname,Dx,Dy,Px,Py,pa, $
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
  getrot,Rhd,rot,cdelt
  cdelt = ABS(cdelt)*3600.
  as_pix = cdelt[0] ; arcsec per pixel
  scale = a*as_pix

; Find the RA and Dec of the center point
  xyad,Rhd,Dx,Dy,racen,deccen
  diamaj = as_pix*a/60.0
  diamin = as_pix*b/60.0

; STEP 7: Create the coarse mask
  zmin = 0.5
  zmax = 3.0
; If the area would be less than ~200 boxes before mask, increase AreaZ
  AreaZ = 500.0*bxw^2/(!pi*a*b) > (zmax^2 - zmin^2)

  dr = (SQRT((diamaj+diamin)^2 + 4*AreaZ*diamaj*diamin) - $
        diamaj-diamin)/2.0

  PRINT,"Creating coarse mask"
; Read the pixel files
  IF FILE_TEST(idir+Rplfile) THEN Rplimg = readfits(idir+Rplfile,/SILENT) $
                             ELSE Rplimg = readfits(idir+Rplfile+'.gz',/SILENT)
  IF FILE_TEST(idir+Nplfile) THEN Nplimg = readfits(idir+Nplfile,/SILENT) $
                             ELSE Nplimg = readfits(idir+Nplfile+'.gz',/SILENT)
  IF FILE_TEST(idir+Splfile) THEN Splimg = readfits(idir+Splfile,/SILENT) $
                             ELSE Splimg = readfits(idir+Splfile+'.gz',/SILENT)

; Initialize the coarse mask, using the R image as a baseline.
  mask = mask_ellipse_ann3(Rimg,Rhd,racen, deccen, $
                           (2.0*diamaj*zmin),(2.0*diamin*zmin),$
                           dr,pa,goodmask=1b,/amflag)
; If there's more than one galaxy in the frame, this'll include bands around
; them all.

; If a pixel wasn't used in either the R or narrow-band image, reject it
  Rmask = mask OR Rplimg LT 0.5
  Nmask = mask OR Nplimg LT 0.5
  Smask = mask OR Splimg LT 1.5
; mask at this point is 1b for the squares we want to reject

  IF NOT KEYWORD_SET(nomask) THEN BEGIN
    Rmask = (Rmask OR Rmaskimg)
    Nmask = (Nmask OR Nmaskimg)
    Smask = (Smask OR Smaskimg)
  ENDIF

; STEP 8: Box the images
  PRINT,"Boxing the R image"
  IF countflag THEN BEGIN
    mysky,(Rimg*Rtime),Rimsky,Rimsig,mask=Rmask,/silent
  ENDIF ELSE BEGIN
    mysky,Rimg,Rimsky,Rimsig,mask=Rmask,/silent
  ENDELSE
  Rskyres = box_sky(Rimg,Rmask,[Rimsky,Rimsig],bxw,Rboxdata,REJFRAC=rejfrac)
  IF countflag THEN BEGIN
    Rboxdata[0,*] = Rboxdata[0,*] / Rtime
    Rskyres = Rskyres / Rtime
    Rimsky = Rimsky / Rtime
    Rimsig = Rimsig / Rtime
  ENDIF

  PRINT,"Boxing the Narrow-band image"
  IF countflag THEN BEGIN
    mysky,(Nimg*Ntime),Nimsky,Nimsig,mask=Nmask,/silent
  ENDIF ELSE BEGIN
    mysky,Nimg,Nimsky,Nimsig,mask=Nmask,/silent
  ENDELSE
  Nskyres = box_sky(Nimg,Nmask,[Nimsky,Nimsig],bxw,Nboxdata,REJFRAC=rejfrac)
  IF countflag THEN BEGIN
    Nboxdata[0,*] = Nboxdata[0,*] / Ntime
    Nskyres = Nskyres / Ntime
    Nimsky = Nimsky / Ntime
    Nimsig = Nimsig / Ntime
  ENDIF

  PRINT,"Boxing the Subtracted image"
; Occasionally, due to subtraction problems, the mask will be different for the
; subtracted image.  For now, assume the trimming is symmetric; it SHOULD use 
; the header, but that won't have been modified to fit the new FOV.  
; Realistically, the image should have been fixed in the first place, it'll 
; screw up flux calibrations otherwise.
  masksize = SIZE(Smask)
  imsize = SIZE(Simg)
  IF masksize[4] NE imsize[4] THEN BEGIN ; element 4 is total number of pixels
    tempmask = Smask
    xshift = (masksize[1]-imsize[1])/2
    yshift = (masksize[2]-imsize[2])/2
    Smask = BYTARR(imsize[1],imsize[2])
    FOR ii = 0, imsize[1]-1 DO BEGIN
      FOR jj = 0, imsize[2]-1 DO BEGIN
        Smask[ii,jj] = tempmask[ii+xshift,jj+yshift]
      ENDFOR
    ENDFOR
  ENDIF

  IF countflag THEN BEGIN
    mysky,(Simg*Stime),Simsky,Simsig,mask=Smask,/silent
  ENDIF ELSE BEGIN
    mysky,Simg,Simsky,Simsig,mask=Smask,/silent
  ENDELSE
  Sskyres = box_sky(Simg,Smask,[Simsky,Simsig],bxw,Sboxdata,REJFRAC=rejfrac)
  IF countflag THEN BEGIN
    Sboxdata[0,*] = Sboxdata[0,*] / Stime
    Sskyres = Sskyres / Stime
    Simsky = Simsky / Stime
    Simsig = Simsig / Stime
  ENDIF

; STEP 9: Set up the plotting, if it's needed.  That is, if we're in
; interactive mode, or noninteractive-but-with-pretty-pictures mode, open 
; and clear the plot window.
; Feed boxdata to a routine that bins and plots the stuff
  zbinsize=0.02
  Zbin=FINDGEN(LONG(zmax/zbinsize)+1)*zbinsize

  plotflag = NOT fastflag OR MIN(a) LT 0.0 OR MIN(b) LT 0.0

  IF plotflag THEN BEGIN
    setplotcolors
    DEVICE, RETAIN=2, DECOMPOSED=0
    WINDOW,XSIZE=800,YSIZE=800
    !P.MULTI=[0,0,3,0,0]
  ENDIF

  sz = SIZE(Rimg)
  FOR ii = 0,n_ellipses-1 DO BEGIN
    bindata,Rboxdata,Rskyres,zmin,zmax,zbinsize,sz, $
            Dx[ii],Dy[ii],a[ii],b[ii],theta[ii],RZbox,RZbin,RSkybin
    bindata,Nboxdata,Nskyres,zmin,zmax,zbinsize,sz, $
            Dx[ii],Dy[ii],a[ii],b[ii],theta[ii],NZbox,NZbin,NSkybin
    bindata,Sboxdata,Sskyres,zmin,zmax,zbinsize,sz, $
            Dx[ii],Dy[ii],a[ii],b[ii],theta[ii],SZbox,SZbin,SSkybin

    IF ii EQ 0 THEN BEGIN
      RZbox0 = RZbox
      NZbox0 = NZbox
      SZbox0 = SZbox
      RSkybin0 = RSkybin
      NSkybin0 = NSkybin
      SSkybin0 = SSkybin
    ENDIF

    IF plotflag THEN BEGIN
      IF n_ellipses EQ 1 THEN BEGIN
        Rtitle = STRTRIM(Rfile,2)
        Ntitle = STRTRIM(Nfile,2)
        Stitle = STRTRIM(Sfile,2)
      ENDIF ELSE BEGIN
        Rtitle = STRTRIM(Rfile,2)+" ellipse #"+STRTRIM(STRING(ii+1),2)
        Ntitle = STRTRIM(Nfile,2)+" ellipse #"+STRTRIM(STRING(ii+1),2)
        Stitle = STRTRIM(Sfile,2)+" ellipse #"+STRTRIM(STRING(ii+1),2)
      ENDELSE
      plot_skydist,RZbox,Zbin,Rboxdata,RSkybin,Rskyres,bwflag,$
                   zmin,zmax,scale[ii],1.0,Ztemp[ii],Rtitle
      plot_skydist,NZbox,Zbin,Nboxdata,NSkybin,Nskyres,bwflag,$
                   zmin,zmax,scale[ii],1.0,Ztemp[ii],Ntitle
      plot_skydist,SZbox,Zbin,Sboxdata,SSkybin,Sskyres,bwflag,$
                   zmin,zmax,scale[ii],1.0,Ztemp[ii],Stitle
    ENDIF

; STEP 10: Choose Z', either from the file or interactively
    IF plotflag THEN BEGIN
; Enter interactive mode for choosing Z'
      PRINT,"Entering interactive mode"
; Note that if either of the above conditions were true, plotflag would be
; overridden, so we don't need to explicitly check it inside this IF block

      key = "R"
      Zold = Ztemp[ii]

      WHILE (STRUPCASE(key) EQ "R") DO BEGIN

        PRINT,"Select start of sky annulus"
        CURSOR,xcurs,ycurs,/DOWN
        PRINT,"The previous value was a=",Zold*a[ii]*as_pix," arcsec"

        IF xcurs GT (zmin*scale[ii]) THEN BEGIN
          PRINT,"a is now ",xcurs," arcsec"
          Ztemp[ii] = xcurs/scale[ii]
        ENDIF

        !P.MULTI=[0,0,3,0,0]
; we don't need to rerun bindata
        plot_skydist,RZbox,Zbin,Rboxdata,RSkybin,Rskyres,bwflag, $
                     zmin,zmax,scale[ii],1.0,Ztemp[ii],Rtitle
        plot_skydist,NZbox,Zbin,Nboxdata,NSkybin,Nskyres,bwflag, $
                     zmin,zmax,scale[ii],1.0,Ztemp[ii],Ntitle
        plot_skydist,SZbox,Zbin,Sboxdata,SSkybin,Sskyres,bwflag, $
                     zmin,zmax,scale[ii],1.0,Ztemp[ii],Stitle

; wait for keyprompt, then close window
        IF xcurs GT (zmin*scale[ii]) THEN BEGIN
          PRINT,"(A)ccept, (E)nter Z, (M)anual sky entry, (R)etry, (F)ail?"
          key = GET_KBRD(1)

          IF (STRUPCASE(key) EQ "F") THEN BEGIN
             PRINT,"Exiting program"
             RETURN
          ENDIF

          IF (STRUPCASE(key) EQ "E") THEN BEGIN
             READ,num,PROMPT="Enter value in arcsec: "
             Ztemp[ii] = FLOAT(num)/scale[ii]
          ENDIF

          IF (STRUPCASE(key) EQ "M") THEN BEGIN
             PRINT,"Manually entering sky values.  Z will be unchanged"
;           READ,skyval,PROMPT="Enter new value in counts/sec: "
             setflag = 1b
; If you do this, it'll override the normal result with this value.
          ENDIF
; Anything other than R/F will pass this loop, and E/M had you enter a value,
; so you don't actually have to hit A to accept, anything other than E/R/F/M
; will work for this purpose.
        ENDIF ELSE BEGIN
          key = "A"
        ENDELSE
      ENDWHILE

    ENDIF
  ENDFOR

; Note that while we solved for the ellipses around each galaxy, we only use
; the first one when subtracting sky levels.

; STEP 11: Create the final mask.
; modify the axis values
  diamaj = diamaj*Ztemp
  diamin = diamin*Ztemp
; If the resulting area would be less than ~100 boxes, increase AreaZ
  AreaZ = 250.0*bxw^2/(!pi*a*b) > 1.0

; AreaZ = 1.0 means the area between the ellipse and aperture equals
; the area inside the ellipse.

  dr = (SQRT((diamaj+diamin)^2.0 + 4.0*AreaZ*diamaj*diamin)-diamaj-diamin)/2.0

  PRINT,"Creating final mask"

; Now, narrow down the image to the donut we want, from Z' to SQRT(2)*Z', and
; ONLY around galaxy #0 (the one we want to subtract from the image).
  mask = mask_ellipse_ann3(Rimg,Rhd,racen[0],deccen[0], $
                           2.0*diamaj[0],2.0*diamin[0], $
                           dr[0],pa[0],goodmask=1b,/amflag)
  Rmask = mask OR Rplimg LT 0.5
  Nmask = mask OR Nplimg LT 0.5
  Smask = mask OR Splimg LT 1.5
; Again, mask=1b for the pixels we want to remove

  IF NOT KEYWORD_SET(nomask) THEN BEGIN
    Rmask = (Rmask OR Rmaskimg)
    Nmask = (Nmask OR Nmaskimg)
    Smask = (Smask OR Smaskimg)
  ENDIF

; STEP 12: Now that we have new masks, feed the old box data into the new mask
; to determine a new sky level for each image.  While you're at it, update the
; header information with the new variables, correct for exposure time, and
; then write an output .fits image.
  PRINT,"For object ",object," with filters = ",STRTRIM(filter,2)
  IF plotflag THEN !P.MULTI=[0,0,3,0,0]

  Rmaxpl = (MAX(Rplimg) < 3)
  Nmaxpl = (MAX(Nplimg) < 3)
  
  edge = 100
  edgemask = BYTARR(sz[1],sz[2])+1b
  edgemask[buffer+edge:sz[1]-buffer-1-edge, $
           buffer+edge:sz[2]-buffer-1-edge] = 0b
  Rgood = (1b-calc_outer_mask((Rplimg LT Rmaxpl),(-1*edge))) AND (Rmaskimg LT 0.5)
  Ngood = (1b-calc_outer_mask((Nplimg LT Nmaxpl),(-1*edge))) AND (Nmaskimg LT 0.5)
  Sgood = (1b-calc_outer_mask((Splimg LT 2),(-1*edge))) AND (Smaskimg LT 0.5)

  FOR jj = 0,n_ellipses-1 DO BEGIN
    ratio = a[jj]/b[jj]
    dist_ellipse,adist,[sz[1],sz[2]],Dx[jj],Dy[jj],ratio,pa[jj],/DOUBLE
    zdist = adist / a[jj]
    Rgood = Rgood AND (zdist GE 2.0 OR (adist/SQRT(ratio)) GE 900.0)
    Ngood = Ngood AND (zdist GE 2.0 OR (adist/SQRT(ratio)) GE 900.0)
    Sgood = Sgood AND (zdist GE 2.0 OR (adist/SQRT(ratio)) GE 900.0)
  ENDFOR

  Rskyind = WHERE(Rgood,Rskycount)
  Rdsky = box_sky(Rimg,(1b-Rgood),[Rimsky,Rimsig],bxw,boxjunk,REJFRAC=0.1)
  Rdelsky = MEAN(Rimg[Rskyind]) - Rdsky[0]

  Nskyind = WHERE(Ngood,Nskycount)
  Ndsky = box_sky(Nimg,(1b-Ngood),[Nimsky,Nimsig],bxw,boxjunk,REJFRAC=0.1)
  Ndelsky = MEAN(Nimg[Nskyind]) - Ndsky[0]

  Sskyind = WHERE(Sgood,Sskycount)
  Sdsky = box_sky(Simg,(1b-Sgood),[Simsky,Simsig],bxw,boxjunk,REJFRAC=0.1)
  Sdelsky = MEAN(Simg[Sskyind]) - Sdsky[0]

; R-BAND IMAGE
  IF dumpflag THEN BEGIN
    boxfile = odir+object+"_R.box"
    skyR = calc_box_sky(Rboxdata,Rmask,bxw,Dx[0],Dy[0],theta[0],a[0],b[0], $
                        boxfile=boxfile)
  ENDIF ELSE BEGIN
    skyR = calc_box_sky(Rboxdata,Rmask,bxw,Dx[0],Dy[0],theta[0],a[0],b[0])
  ENDELSE

  PRINT,skyR[0],skyR[1],$
        FORMAT='("R image sky is: ",F8.4," +/- ",F7.5," counts/sec")'
  IF setflag THEN BEGIN
    READ,skyval,PROMPT="  Enter new R-band sky value in counts/sec: "
    skyR[0] = skyval
  ENDIF
  IF plotflag THEN plot_skydist,RZbox0,Zbin,Rboxdata,RSkybin0,skyR, $
                  bwflag,zmin,zmax,scale[0],1.0,Ztemp[0],Rfile,MASK=Rmask

  Rimg = Rimg - FLOAT(skyR[0])
  IF fitsflag THEN $
      write_ssfits,odir+Rfile,Rimg,Rhd,Dx[0],Dy[0],Px[0],Py[0],skyR,Rimsig, $
                   Rdelsky,header_template,racen[0],deccen[0], $
                   diamin[0],diamaj[0],dr[0],pa[0],bxw,Rphot

; NARROW-BAND IMAGE:
  IF dumpflag THEN BEGIN
    boxfile = odir+object+"_"+filter+".box"
    skyN = calc_box_sky(Nboxdata,Nmask,bxw,Dx[0],Dy[0],theta[0],a[0],b[0], $
                        boxfile=boxfile)
  ENDIF ELSE BEGIN
    skyN = calc_box_sky(Nboxdata,Nmask,bxw,Dx[0],Dy[0],theta[0],a[0],b[0])
  ENDELSE
  PRINT,skyN[0],skyN[1],$
        FORMAT='("Narrow-band image sky is: ",F8.5," +/- ",F8.6," counts/sec")'
  IF setflag THEN BEGIN
    READ,skyval,PROMPT="  Enter new narrow-band sky value in counts/sec: "
    skyN[0] = skyval
  ENDIF
  IF plotflag THEN plot_skydist,NZbox0,Zbin,Nboxdata,NSkybin0,skyN, $
                   bwflag,zmin,zmax,scale[0],1.0,Ztemp[0],Nfile,MASK=Nmask

  Nimg = Nimg - FLOAT(skyN[0])
  IF fitsflag THEN $
      write_ssfits,odir+Nfile,Nimg,Nhd,Dx[0],Dy[0],Px[0],Py[0],skyN,Nimsig, $
                   Ndelsky,header_template,racen[0],deccen[0], $
                   diamin[0],diamaj[0],dr[0],pa[0],bxw,Nphot

; SUBTRACTED IMAGE:
  IF dumpflag THEN BEGIN
    boxfile = odir+object+"_Rsub.box"
    skyS = calc_box_sky(Sboxdata,Smask,bxw,Dx[0],Dy[0],theta[0],a[0],b[0], $
                        boxfile=boxfile)
  ENDIF ELSE BEGIN
    skyS = calc_box_sky(Sboxdata,Smask,bxw,Dx[0],Dy[0],theta[0],a[0],b[0])
  ENDELSE
  PRINT,skyS[0],skyS[1],$
        FORMAT='("Subtracted image sky is: ",F9.6," +/- ",F8.6," counts/sec")'
  IF setflag THEN BEGIN
    READ,skyval,PROMPT="  Enter new subtracted-image sky value in counts/sec: "
    skyS[0] = skyval
  ENDIF
  IF plotflag THEN plot_skydist,SZbox0,Zbin,Sboxdata,SSkybin0,skyS, $
                   bwflag,zmin,zmax,scale[0],1.0,Ztemp[0],Sfile,MASK=Smask

  Simg = Simg - FLOAT(skyS[0])
  IF fitsflag THEN $
      write_ssfits,odir+Sfile,Simg,Shd,Dx[0],Dy[0],Px[0],Py[0],skyS,Simsig, $
                   Sdelsky,header_template,racen[0],deccen[0], $
                   diamin[0],diamaj[0],dr[0],pa[0],bxw,Sphot

; Write the .jpg file
  IF plotflag THEN BEGIN
    jpgfile = STRMID(Sfile,0,STRLEN(Sfile)-8)+"_sky.jpg"
    PRINT,"Writing image to ",jpgfile
    im = tvrd(true=3)
    write_jpeg,odir+jpgfile,im,true=3,quality=100
  ENDIF

; STEP 14: While you're at it, write the new sky table too
  IF KEYWORD_SET(output) THEN BEGIN
; First, see if the sky file exists.  It'll be one directory up from
; wherever the outputs are being dumped.
    slashpos = STRPOS(STRMID(odir,0,STRLEN(odir)-1),'/',/REVERSE_SEARCH)
    outfile = STRMID(odir,0,slashpos+1)+output
    IF FILE_TEST(outfile) THEN BEGIN
; Then, open the sky file to see if this object is already inside
      readcol_new,outfile,tRname,tRsky,tRbsig,tRpsig,$
                          tNname,tNsky,tNbsig,tNpsig,$
                          tSname,tSsky,tSbsig,tSpsig,$
                          FORMAT="A,F,F,F,A,F,F,F,A,F,F,F",COMMENT='#',/SILENT

; Match by name of the _6XXX.fits file, since for some objects multiple 
; narrow-band filters might be used
      updateline = WHERE(STRTRIM(tNname,2) EQ STRTRIM(Nfile,2),count)
      IF count LT 1 THEN updateline[0] = N_ELEMENTS(tNname)
    ENDIF ELSE BEGIN
      updateline = INTARR(1)
    ENDELSE

; Now, reopen the file for writing
    PRINT,"Now writing the output file for sky data"

; The important thing here is, if the galaxy is already listed in this file
; then only that galaxy will have its data edited.  If the galaxy is NOT in
; the file already, a new row will be added at the end.
    OPENW,unit,outfile,/GET_LUN

    PRINTF,unit,"# Format is (filename, boxsky, boxsigma, pixsigma)"
    PRINTF,unit,"# for R-band, narrow-band Halpha, and subtracted"
    FOR ii = 0,(N_ELEMENTS(tRname)-1) DO BEGIN
      IF ii NE updateline[0] THEN BEGIN
        PRINTF,unit,tRname[ii],tRsky[ii],tRbsig[ii],tRpsig[ii],$
                    tNname[ii],tNsky[ii],tNbsig[ii],tNpsig[ii],$
                    tSname[ii],tSsky[ii],tSbsig[ii],tSpsig[ii],$
                    FORMAT='(3(A," ",F11.7," ",F11.8," ",F11.8," "))'

      ENDIF ELSE BEGIN
        PRINTF,unit,Rfile,skyR[0],skyR[1],Rimsig,$
                    Nfile,skyN[0],skyN[1],Nimsig,$
                    Sfile,skyS[0],skyS[1],Simsig,$
                    FORMAT='(3(A," ",F11.7," ",F11.8," ",F11.8," "))'
      ENDELSE
    ENDFOR
    IF updateline[0] EQ N_ELEMENTS(tRname) THEN $
      PRINTF,unit,Rfile,skyR[0],skyR[1],Rimsig,$
                  Nfile,skyN[0],skyN[1],Nimsig,$
                  Sfile,skyS[0],skyS[1],Simsig,$
                  FORMAT='(3(A," ",F11.7," ",F11.8," ",F11.8," "))'
    CLOSE,unit
    FREE_LUN,unit
  ENDIF

; If we need to, update the ellipse file with new numbers.
  IF NOT fastflag THEN BEGIN
    write_ellipse_file,odir+ellipse,n_ellipses,refname,Dx,Dy, $
            Px,Py,buffer,pa,a_i,b_i,(z_s*Ztemp),z_f,z_c,/SILENT
  ENDIF

  PRINT,"Object "+object+" completed."
 
;  PRINT,"Press any key to exit."
;  key = GET_KBRD(1)
;  WDELETE

  CLOSE,/ALL

END
