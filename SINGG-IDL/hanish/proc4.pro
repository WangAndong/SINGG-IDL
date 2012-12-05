PRO proc4,RFILE=Rfile,NFILE=Nfile,SFILE=Sfile,ELLIPSE=ellipse, $
          INDIR=indir,OUTDIR=outdir,RUNID=runid,OBJ=obj, $
          OUTPUT=output,NSIG=nsig,GROW=grow, $
          SILENT=silent,FORCE=force,FULL=full
; Given non-sky-subtracted images, generate/edit the ellipse and mask files.
; OPTIONAL INPUTS:
; Rfile    name of R image (if not set, J???????_R.fits)
; Nfile    name of narrow-band image (if not set, J???????_6???.fits)
; Sfile    name of subtracted image (if not set, J???????_Rsub.fits)
; Ellipse  name of ellipse file (if not set, j???????_ellipse.dat)
; indir         Input directory (default is the current one)
; outdir        Output directory (default is the current one)
; /output  Creates three _sex.fits files, showing how each mask
;            applies to the input images.
; nsig     number of sigmas to use in the _Rsub mask
; grow     number of pixels to grow mask by (if not set, use seeing)
; /silent  Do not use interactive mode, just write the files
; /force   Perform SExtractor, even if the files already exist.
; /full    Use a smaller zoom with the edge regions included

  COMMON proc4text,textarr,n_tlines

  sat_thresh = 55000.0
  goodval = 0b
  badval  = 1b
  IF NOT KEYWORD_SET(nsig) THEN nsig = 1.5
  dummy = FLTARR(3)
  fullflag = KEYWORD_SET(full)
  IF fullflag THEN bxw = 3 ELSE bxw = 2 ; TV averages bxw x bxw to get image

  n_tlines = 3
  textarr = STRARR(n_tlines) ; Clear the text array.

  spawn,"pwd",cdir

; An easier way to specify directories:
  IF KEYWORD_SET(obj) AND KEYWORD_SET(runid) THEN BEGIN
    indir = STRTRIM(runid,2)+'/Proc3/'+STRTRIM(obj,2)+'/'
    outdir = STRTRIM(runid,2)+'/Proc4/'+STRTRIM(obj,2)+'/'
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

  rmin = -20.0
  rmax = -10.0
  nmax = -7.0

;----------------------------------------------------------
;| STEP 1: Figure out what our inputs are, and read them. |
;----------------------------------------------------------

  IF KEYWORD_SET(Rfile) THEN BEGIN $
    IF NOT FILE_TEST(idir+Rfile) THEN BEGIN
      PRINT,"ERROR in proc4: R file not found. ",Rfile
      RETURN
    ENDIF
  ENDIF ELSE BEGIN
; Rfile wasn't specified, so we need to find it.
; Assume Rfile will be of the form *_?.fits.
  spawn,"ls "+idir+"J*_?.fits",Rlist

    IF NOT FILE_TEST(Rlist[0]) THEN BEGIN
      PRINT,"ERROR in proc4: No combined continuum images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the RFILE keyword explicitly."
      RETURN
    ENDIF

    Rfile = STRMID(Rlist[0],STRLEN(idir),STRLEN(Rlist[0])-STRLEN(idir))
    PRINT,"R image used: ",Rfile
  ENDELSE
  Rplfile = STRMID(Rfile,0,STRLEN(Rfile)-5)+".pl.fits"

; For narrow-band, it's a bit more complex since there could be multiple
; narrow-band filters used for this object.
  IF KEYWORD_SET(Nfile) THEN BEGIN $
    IF NOT FILE_TEST(idir+Nfile) THEN BEGIN
      PRINT,"ERROR in proc4: Narrow-band file not found. ",Nfile
      RETURN
    ENDIF
  ENDIF ELSE BEGIN
; Nfile wasn't specified, so we need to find it.
; Assume Nfile will be of the form *_6XXX.fits.
    spawn,"ls "+idir+"J*_6???.fits",Nlist

    IF N_ELEMENTS(Nlist) GT 1 THEN BEGIN
      PRINT,"ERROR in proc4: Multiple combined narrow-band images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the NFILE keyword explicitly."
      RETURN
    ENDIF

    IF NOT FILE_TEST(Nlist[0]) THEN BEGIN
      PRINT,"ERROR in proc4: No combined narrow-band images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the NFILE keyword explicitly."
      RETURN
    ENDIF

    Nfile = STRMID(Nlist[0],STRLEN(idir),STRLEN(Nlist[0])-STRLEN(idir))
    PRINT,"Narrow-band image used: ",Nfile
  ENDELSE
  filter = STRMID(Nfile,STRLEN(Nfile)-9,4)
  Nplfile = STRMID(Nfile,0,STRLEN(Nfile)-5)+".pl.fits"

  spawn,"ls "+idir+"J*_?sub.fits",Slist
  multflag = N_ELEMENTS(Slist) GT 1

  IF KEYWORD_SET(Sfile) THEN BEGIN $
    IF NOT FILE_TEST(idir+Sfile) THEN BEGIN
      PRINT,"ERROR in proc4: Subtracted file not found. ",Sfile
      RETURN
    ENDIF
  ENDIF ELSE BEGIN
; Sfile wasn't specified, so we need to find it.
    IF NOT FILE_TEST(Slist[0]) THEN BEGIN
      PRINT,"ERROR in proc4: No combined subtracted images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the SFILE keyword explicitly."
      RETURN
    ENDIF

    Sfile = STRMID(Slist[0],STRLEN(idir),STRLEN(Slist[0])-STRLEN(idir))
    PRINT,"Subtracted image used: ",Sfile
  ENDELSE
  Splfile = STRMID(Sfile,0,STRLEN(Sfile)-5)+".pl.fits"

  ellflag = 0b ; Whether we have an ellipse file to fall back on
  IF KEYWORD_SET(ellipse) THEN BEGIN
    IF FILE_TEST(odir+ellipse) THEN BEGIN
      ellflag = 1b
    ENDIF ELSE BEGIN
      PRINT,"WARNING in proc4: specified ellipse file does not exist: ",ellipse
    ENDELSE
  ENDIF ELSE BEGIN
; No ellipse file was specified, so look for one
    spawn,"ls "+odir+"*_ellipse.dat",elllist
    n_file = N_ELEMENTS(elllist)
    IF n_file GT 1 THEN BEGIN
      PRINT,"ERROR in proc4: multiple ellipse files found, please specify"
      RETURN
    ENDIF ELSE BEGIN
      IF FILE_TEST(elllist[0]) THEN BEGIN
        ellipse = STRMID(elllist[0],STRLEN(odir),STRLEN(elllist[0])-STRLEN(odir))
        PRINT,"Ellipse file used: ",ellipse
        ellflag = 1b
      ENDIF ELSE BEGIN
        PRINT,"No ellipse files found."
        ellipse=STRMID(Rfile,0,STRPOS(Rfile,"_"))+"_ellipse.dat"
      ENDELSE
    ENDELSE
  ENDELSE

; First, create the _sex.fits images (non-sky subtracted, in raw
; counts)

  IF multflag THEN BEGIN
    Rsexfile = STRMID(Rfile,0,STRLEN(Rfile)-5)+'_ref'+filter+'_sex.fits'
  ENDIF ELSE BEGIN
    Rsexfile = STRMID(Rfile,0,STRLEN(Rfile)-5)+'_sex.fits'
  ENDELSE
  Rcat = STRMID(Rsexfile,0,STRLEN(Rsexfile)-5)+'.cat'
  Nsexfile = STRMID(Nfile,0,STRLEN(Nfile)-5)+'_sex.fits'
  Ncat = STRMID(Nsexfile,0,STRLEN(Nsexfile)-5)+'.cat'
  Ssexfile = STRMID(Sfile,0,STRLEN(Sfile)-5)+'_sex.fits'
  Scat = STRMID(Ssexfile,0,STRLEN(Ssexfile)-5)+'.cat'
  
; If this is one of the directories with multiple filter combinations
; (as evidenced by multiple subtracted images), the files need longer names.
  IF multflag THEN Rbase = STRMID(Rfile,0,STRLEN(Rfile)-5) $
              ELSE Rbase = STRMID(Rfile,0,STRLEN(Rfile)-7)

  segfile = Rbase+'_mask.seg.fits' 
  maskfile = Rbase+'_mask.fits'
  Smaskfile = STRMID(Sfile,0,STRLEN(Sfile)-5)+'_mask.fits'
  Imaskfile = Rbase+'_inc_mask.fits'
  overwrite = 0b ; whether to write the Override file.
  overfile = STRMID(Sfile,0,STRLEN(Sfile)-5)+'_override.fits'

  fits_read,idir+Rfile,Rimg_old,Rhd
  
  reftemp = STRTRIM(SXPAR(Rhd,'FLUXREF'),2)
  Rfluxref = idir+STRMID(reftemp,0,STRLEN(reftemp)-5)+'sh.fits'
  IF FILE_TEST(Rfluxref) THEN BEGIN
    junk = readfits(Rfluxref,refhdr,/SILENT)
    Rexptime = SXPAR(refhdr,'EXPTIME')
  ENDIF ELSE BEGIN
; It's probably just gzipped.
    zipfile = Rfluxref+".gz"
    IF FILE_TEST(zipfile) THEN BEGIN
      junk = readfits(zipfile,refhdr,/SILENT)
      Rexptime = SXPAR(refhdr,'EXPTIME')
    ENDIF ELSE BEGIN
      PRINT,'WARNING in proc4: reference image not found ',Rfluxref
      Rexptime = SXPAR(Rhd,'EXPTIME') / SXPAR(Rhd,'NCOMBINE')
    ENDELSE
  ENDELSE

  buffer = SXPAR(Rhd,"BUFFER")
  IF fullflag THEN buffer2=0 ELSE buffer2=buffer

; Read the pixel count images, and apply them.
  IF FILE_TEST(idir+Rplfile) THEN Rplimg = readfits(idir+Rplfile,/SILENT) $
                             ELSE Rplimg = readfits(idir+Rplfile+'.gz',/SILENT)
  IF FILE_TEST(idir+Nplfile) THEN Nplimg = readfits(idir+Nplfile,/SILENT) $
                             ELSE Nplimg = readfits(idir+Nplfile+'.gz',/SILENT)
  IF FILE_TEST(idir+Splfile) THEN Splimg = readfits(idir+Splfile,/SILENT) $
                             ELSE Splimg = readfits(idir+Splfile+'.gz',/SILENT)
  IF fullflag THEN plmask = (Rplimg LT 0.5) OR (Nplimg LT 0.5) $
              ELSE plmask = (Splimg LT 1.5)

  Rskysub = STRTRIM(SXPAR(Rhd,'SKYSUB',count=count),2)
  Rsky = 0.0
  IF count GT 0 AND Rskysub EQ 'T' THEN Rsky = SXPAR(Rhd,'SKYLEV')
  Rseeing = SXPAR(Rhd,'SEEING')
  magzpt = SXPAR(Rhd,"MAGZPT1")
; If the input image has been sky-subtracted, add the sky back in
  Rimg = (Rimg_old+Rsky) * Rexptime
  IF FILE_TEST (odir+Rsexfile) THEN spawn,"/bin/rm -f "+odir+Rsexfile
  fits_write,odir+Rsexfile,Rimg,Rhd

  sz = SIZE(Rimg)
  IF sz[1] GT 3100 THEN bxw = bxw + 1 ; NGC253 is just too big, even at bxw=3

  fits_read,idir+Nfile,Nimg_old,Nhd

  reftemp = STRTRIM(SXPAR(Nhd,'FLUXREF'),2)
  Nfluxref = idir+STRMID(reftemp,0,STRLEN(reftemp)-5)+'sh.fits'
  IF FILE_TEST(Nfluxref) THEN BEGIN
    junk = readfits(Nfluxref,refhdr,/SILENT)
    Nexptime = SXPAR(refhdr,'EXPTIME')
  ENDIF ELSE BEGIN
; It's probably just gzipped.
    zipfile = Nfluxref+".gz"
    IF FILE_TEST(zipfile) THEN BEGIN
      junk = readfits(zipfile,refhdr,/SILENT)
      Nexptime = SXPAR(refhdr,'EXPTIME')
    ENDIF ELSE BEGIN
      PRINT,'WARNING in proc4: reference image not found ',Nfluxref
      Nexptime = SXPAR(Nhd,'EXPTIME') / SXPAR(Nhd,'NCOMBINE')
    ENDELSE
  ENDELSE

  Nskysub = STRTRIM(SXPAR(Nhd,'SKYSUB',count=count),2)
  Nsky = 0.0
  IF count GT 0 AND Nskysub EQ 'T' THEN Nsky = SXPAR(Nhd,'SKYLEV')
  Nimg = (Nimg_old+Nsky) * Nexptime
  IF FILE_TEST (odir+Nsexfile) THEN spawn,"/bin/rm -f "+odir+Nsexfile
  fits_write,odir+Nsexfile,Nimg,Nhd

  fits_read,idir+Sfile,Simg_old,Shd
  Sskysub = STRTRIM(SXPAR(Shd,'SKYSUB',count=count),2)
  Ssky = 0.0
  IF count GT 0 AND Sskysub EQ 'T' THEN Ssky = SXPAR(Shd,'SKYLEV')
  cntrat = SXPAR(Shd,"CNTRAT1")*Nexptime/Rexptime
  ecntrat = SXPAR(Shd,"ECNTRAT2")*Nexptime/Rexptime
  IF ecntrat GT 0.1*cntrat THEN ecntrat = 0.1*cntrat
  Simg = (Simg_old+Ssky) * Nexptime
  IF FILE_TEST (odir+Ssexfile) THEN spawn,"/bin/rm -f "+odir+Ssexfile
  fits_write,odir+Ssexfile,Simg,Shd

  setplotcolors
  ellcolor = [!purple,!green,!magenta,!dorange,!dyellow,!red,!dcyan,!blue]
  max_ellipses = N_ELEMENTS(ellcolor)
  IF ellflag THEN BEGIN
    read_ellipse_file,odir+ellipse,n_ellipses_old,refimage,Dx_old,Dy_old, $
                      Px_old,Py_old,pa_old,a_i,b_i,z_s_old,z_f_old,z_c_old
    IF n_ellipses_old GT max_ellipses THEN BEGIN
      PRINT,"ERROR in proc4: too many ellipses in file ",n_ellipses_old, max_ellipses
      RETURN
    ENDIF

    a_old = a_i * z_s_old
    b_old = b_i * z_s_old
    a_f_old = a_i * z_f_old
    b_f_old = b_i * z_f_old

    Px_old = Px_old + buffer
    Py_old = Py_old + buffer
    Dx_old = Dx_old + buffer
    Dy_old = Dy_old + buffer
    theta_old = (pa_old-90.0)*!dtor

    n_ellipses = n_ellipses_old
    IF n_ellipses EQ max_ellipses THEN BEGIN
      a = a_old
      b = b_old
      a_f = a_f_old
      b_f = b_f_old
      z_s = z_s_old
      z_f = z_f_old
      z_c = z_c_old
      Px = Px_old
      Py = Py_old
      Dx = Dx_old
      Dy = Dy_old
      theta = theta_old
      pa = pa_old
    ENDIF ELSE BEGIN
      a = [a_old,FLTARR(max_ellipses-n_ellipses)]
      b = [b_old,FLTARR(max_ellipses-n_ellipses)]
      a_f = [a_f_old,FLTARR(max_ellipses-n_ellipses)]
      b_f = [b_f_old,FLTARR(max_ellipses-n_ellipses)]
      z_s = [z_s_old,FLTARR(max_ellipses-n_ellipses)]
      z_f = [z_f_old,FLTARR(max_ellipses-n_ellipses)]
      z_c = [z_c_old,FLTARR(max_ellipses-n_ellipses)]
      Px = [Px_old,FLTARR(max_ellipses-n_ellipses)]
      Py = [Py_old,FLTARR(max_ellipses-n_ellipses)]
      Dx = [Dx_old,FLTARR(max_ellipses-n_ellipses)]
      Dy = [Dy_old,FLTARR(max_ellipses-n_ellipses)]
      theta = [theta_old,FLTARR(max_ellipses-n_ellipses)]
      pa = [pa_old,FLTARR(max_ellipses-n_ellipses)]
    ENDELSE
  ENDIF ELSE BEGIN
    n_ellipses = 0
    refimage = SXPAR(Rhd,'POSREF')
    Px = FLTARR(max_ellipses)
    Py = FLTARR(max_ellipses)
    Dx = FLTARR(max_ellipses)
    Dy = FLTARR(max_ellipses)
    a = FLTARR(max_ellipses)
    b = FLTARR(max_ellipses)
    a_f = FLTARR(max_ellipses)
    b_f = FLTARR(max_ellipses)
    pa = FLTARR(max_ellipses)
    theta = FLTARR(max_ellipses)
    z_s = FLTARR(max_ellipses)+1.0
    z_f = FLTARR(max_ellipses)+1.0
    z_c = FLTARR(max_ellipses)+1.0
  ENDELSE

  getrot,Shd,rot,cdelt 
  cdelt  = abs(cdelt)*3600.
  as_pix = cdelt[0]

  IF NOT KEYWORD_SET(grow) THEN grow = MAX([Rseeing/as_pix,3.0])
  SXADDPAR,Nhd,"GROW",grow,' Mask growth radius'
  SXADDPAR,Shd,"GROW",grow,' Mask growth radius'

;----------------------------
;| STEP 2: Start SExtractor |
;----------------------------
  PRINT,"Beginning Source Extractor"

  RTfile = Rbase+"_check.fits"
  STfile = STRMID(Sfile,0,STRLEN(Sfile)-5)+"_check.fits"

; Cleanup old files
  IF FILE_TEST("./check.fits") THEN spawn,"/bin/rm -f check.fits"
  IF FILE_TEST("./test.cat") THEN spawn,"/bin/rm -f test.cat"

  IF NOT FILE_TEST(odir+Rcat) OR NOT FILE_TEST(odir+RTfile) OR $
     KEYWORD_SET(force) THEN BEGIN
    IF FILE_TEST(odir+Rcat) THEN spawn,"/bin/rm -f "+odir+Rcat
    IF FILE_TEST(odir+RTfile) THEN spawn,"/bin/rm -f "+odir+RTfile
    spawn,"sex "+odir+Nsexfile+" "+odir+Rsexfile+" -c "+!singgdir+ $
          "/play.par -PARAMETERS_NAME "+!singgdir+ $
          "/daofind.param -FILTER_NAME "+!singgdir+ $
          "/default.conv -STARNNW_NAME "+!singgdir+ $
          "/default.nnw -DETECT_THRESH 2 -ANALYSIS_THRESH 2 -DETECT_MINAREA 3"
    spawn,"mv -f test.cat "+odir+Rcat
    spawn,"mv -f check.fits "+odir+STRTRIM(RTfile,2)
  ENDIF
  fits_read,odir+RTfile,Timg,Thd,/data_only
; In some cases, it rolls over into big negatives.
  Timg = Timg * LONG(Timg GT -1000)

  IF NOT FILE_TEST(odir+Ncat) OR KEYWORD_SET(force) THEN BEGIN
    IF FILE_TEST(odir+Ncat) THEN spawn,"/bin/rm -f "+odir+Ncat
    spawn,"sex "+odir+Nsexfile+" "+odir+Nsexfile+" -c "+!singgdir+ $
          "/play.par -PARAMETERS_NAME "+!singgdir+ $
          "/daofind.param -FILTER_NAME "+!singgdir+ $
          "/default.conv -STARNNW_NAME "+!singgdir+ $
          "/default.nnw -DETECT_THRESH 2 -ANALYSIS_THRESH 2 -DETECT_MINAREA 3"
    spawn,"mv -f test.cat "+odir+Ncat
    spawn,"/bin/rm -f check.fits"
  ENDIF

  IF NOT FILE_TEST(odir+Scat) OR NOT FILE_TEST(odir+STfile) OR $
     KEYWORD_SET(force) THEN BEGIN
    IF FILE_TEST(odir+Scat) THEN spawn,"/bin/rm -f "+odir+Scat
    IF FILE_TEST(odir+STfile) THEN spawn,"/bin/rm -f "+odir+STfile
    spawn,"sex "+odir+Nsexfile+" "+odir+Ssexfile+" -c "+!singgdir+ $
          "/play.par -PARAMETERS_NAME "+!singgdir+ $
          "/daofind.param -FILTER_NAME "+!singgdir+ $
          "/default.conv -STARNNW_NAME "+!singgdir+ $
          "/default.nnw -DETECT_THRESH 2 -ANALYSIS_THRESH 2 -DETECT_MINAREA 3"
    spawn,"mv -f test.cat "+odir+Scat
    spawn,"mv -f check.fits "+odir+STRTRIM(STfile,2)
  ENDIF
  fits_read,odir+STfile,STimg,Thd,/data_only

  fmt  = '(L,F,F,F,I,F,F,F,F,F,F,F)'
  readcol_new,odir+Rcat,Rid,Rx,Ry,Rmag,Rflag,Ra,Rb,Rr,Rtheta,Rfwhm,Rback,Rclass, $
           FORMAT=fmt,COMMENT='#',/SILENT
  readcol_new,odir+Ncat,Nid,Nx,Ny,Nmag,Nflag,Na,Nb,Nr,Ntheta,Nfwhm,Nback,Nclass, $
           FORMAT=fmt,COMMENT='#',/SILENT
  readcol_new,odir+Scat,Sid,Sx,Sy,Smag,Sflag,Sa,Sb,Sr,Stheta,Sfwhm,Sback,Sclass, $
           FORMAT=fmt,COMMENT='#',/SILENT

; SExtractor uses 1-N for positions instead of 0-(N-1) for everything.  Adjust.
  Rx = Rx - 1.0
  Ry = Ry - 1.0
  Nx = Nx - 1.0
  Ny = Ny - 1.0
  Sx = Sx - 1.0
  Sy = Sy - 1.0

  num_SEx = N_ELEMENTS(Rx)
  IF num_SEx NE N_ELEMENTS(Nx) OR num_SEx NE N_ELEMENTS(Sx) THEN BEGIN
    PRINT,"ERROR in proc4: size mismatch on SExtractor arrays.  Delete files, or use the /force command."
    PRINT,N_ELEMENTS(Rx),N_ELEMENTS(Nx),N_ELEMENTS(Sx)
    RETURN
  ENDIF

  IF num_SEx GT 32767 THEN BEGIN
; Only used for NGC 253, I think, although we COULD use this logic for
; the others.

    padmask = calc_outer_mask((Splimg LT 1.5),10)

    goodind = WHERE(padmask(Rx,Ry) LT 0.5,goodcount)
    badind = WHERE(padmask(Rx,Ry) GT 0.5,badcount)

    stat = BYTARR(num_SEx)
    stat[Rid[goodind]] = 1b
; Fix Timg
    FOR xx = 0,sz[1]-1 DO BEGIN
      FOR yy = 0,sz[2]-1 DO BEGIN
        IF Timg[xx,yy] GT 0 THEN BEGIN
          IF NOT stat[Timg[xx,yy]] THEN Timg[xx,yy] = 0
        ENDIF
      ENDFOR
    ENDFOR

    PRINT,"OLD size: ",num_SEx,"   NEW size: ",goodcount
    num_SEx = goodcount
; Now, shrink the arrays down.
    Rid = Rid[goodind]
    Rx = Rx[goodind]
    Ry = Ry[goodind]
    Rmag = Rmag[goodind]
    Rflag = Rflag[goodind]
    Ra = Ra[goodind]
    Rb = Rb[goodind]
    Rr = Rr[goodind]
    Rtheta = Rtheta[goodind]
    Rfwhm = Rfwhm[goodind]
    Rback = Rback[goodind]
    Rclass = Rclass[goodind]

    Nid = Nid[goodind]
    Nx = Nx[goodind]
    Ny = Ny[goodind]
    Nmag = Nmag[goodind]
    Nflag = Nflag[goodind]
    Na = Na[goodind]
    Nb = Nb[goodind]
    Nr = Nr[goodind]
    Ntheta = Ntheta[goodind]
    Nfwhm = Nfwhm[goodind]
    Nback = Nback[goodind]
    Nclass = Nclass[goodind]

    Sid = Sid[goodind]
    Sx = Sx[goodind]
    Sy = Sy[goodind]
    Smag = Smag[goodind]
    Sflag = Sflag[goodind]
    Sa = Sa[goodind]
    Sb = Sb[goodind]
    Sr = Sr[goodind]
    Stheta = Stheta[goodind]
    Sfwhm = Sfwhm[goodind]
    Sback = Sback[goodind]
    Sclass = Sclass[goodind]
  ENDIF

  magvalid = (Rmag LT 90.0 AND Nmag LT 90.0)
  fluxratio = (10.0^((Rmag*FLOAT(Rmag LT 90.0)-Nmag*FLOAT(Nmag LT 90.0))/2.5)* $
              FLOAT(magvalid)) + (-99.0 * FLOAT(1b-magvalid))

  edgemask = make_array(sz[1],sz[2],/byte,value=badval)
  edgemask[buffer:(sz[1]-buffer-1),buffer:(sz[2]-buffer-1)] = goodval
  skymask = edgemask OR (Timg NE 0)

  mysky,Rimg,Rsky2,Rskysig,mask=skymask,/silent
  mysky,Nimg,Nsky2,Nskysig,mask=skymask,/silent
  mysky,Simg,Ssky2,Sskysig,mask=skymask,/silent

  IF cntrat LT 0.01 THEN BEGIN
; It wasn't set
    del_M = calc_mdiff(odir+Rsexfile,odir+Nsexfile,(50+buffer),sigma,nmatch, $
                       IMSKY=[Rsky2,Rskysig],REFSKY=[Nsky2,Nskysig])

    cntrat = 10.0^(-0.4*del_M)
    ecntrat = cntrat*ALOG(10.0)*0.4*sigma
    rmin = -2.5*ALOG10(5.0*nsig * Rskysig * SQRT(!pi) * MEAN(Rfwhm))
    PRINT,"New minimum magnitude = ",rmin
  ENDIF

;------------------------
;| STEP 3: Set up flags |
;------------------------
  PRINT,"Initializing flags"
; Now, mark everything that's definitely not the galaxy.
; 1> If the continuum scaling ratio is far from the mean, and this
;    isn't because of saturation, it must be a galaxy.
; 1a> If the R mag is invalid (99) but the N mag is better than -7,
; it's a galaxy.

  galcntcheck = ((fluxratio-cntrat) GT 1.0*nsig*ecntrat) AND $
                 ((Rimg[Rx,Ry] LT sat_thresh AND magvalid) OR $
                  (Nmag LT nmax AND Rmag GT 90.0))
  starcntcheck = ((fluxratio-cntrat) LT 0.0*nsig*ecntrat) AND $
                  (Rmag LT rmax AND magvalid)
; If fluxratio is well below cntrat, it can't be one of our objects.
; If mag2 is 99.0 there was an error, don't hold that against it.

; 2> Check flags.
; FLAGS: 1 = "object has neighbors, close enough to bias MAG_AUTO"
;        2 = "object was originally blended with another one"
; Both 1 and 2 are common for galaxies, but they're also occasional
; for stars.  So, if either of these is set, don't let FLAGS be used
; to determine whether it's a star or not.
;        4 = "at least one pixel is saturated"
;        8 = "object is truncated with edge of image"
; Both 4 and 8 are common for stars.
; 16 and 32 are corrupted data flags, 64 and 128 are memory overflow flags.

   flagcheck = (Rflag MOD 16 GE 4 AND Rflag MOD 4 EQ 0) OR $
               (Nflag MOD 16 GE 4 AND Nflag MOD 4 EQ 0) OR $
               (Sflag MOD 16 GE 4 AND Sflag MOD 4 EQ 0)

; 3> If the FWHM is less than the seeing (or it's really round and
;    only slightly higher than the seeing), assume it's a star.

  sizetol = 0.8
  sizecheck = (Rfwhm LE (Rseeing/as_pix)*sizetol OR $
               Ra LE (0.5*Rseeing/as_pix)*sizetol OR $
               (Rfwhm LE (1.5*Rseeing/as_pix)*sizetol AND Rr LT 1.2))

; 4> If the center is saturated in all images, assume it's a star.

  maxcenter = FLTARR(num_SEx)
  FOR ii = LONG(0),LONG(num_SEx-1) DO BEGIN
    maxcenter[ii] = MAX([Rimg[Rx[ii],Ry[ii]], $
                         Nimg[Nx[ii],Ny[ii]], $
                         Simg[Sx[ii],Sy[ii]]])
  ENDFOR
  satcheck = maxcenter GT sat_thresh

; 5> If it's near the edge, it can't be the galaxy.  In most cases
;     we'll ignore anything outside of our central area, but if you've
;     told the program to use the whole area, it will.

  edge = buffer2 + 10
  edgecheck = (Rx LE edge OR Ry LE edge OR $
               Rx GE ((sz[1]-1)-edge) OR Ry GE ((sz[2]-1)-edge))

; 6> If the magnitude's greater than the brightest limit (rmin), it
;    has to be a star

  magcheck = (Rmag LT rmin)

; 7> Use SExtractor's automatic "CLASS_STAR" determination; if it's
;    0.2 or higher, assume it's a star.

  meanclass = (Rclass + Nclass + Sclass)/3.0
  classcheck1 = meanclass GT 0.2
  classcheck2 = meanclass GT 0.8

; 8> If the fraction of the area within the FWHM is very small, it's
;     a star.  If it's very large, it's a galaxy.
; 9> If the S image value is less than the noise (from sky and
;     cntrat), it's definitely a star.

  Sarea = !pi*Sa*Sb
  Spix = LONARR(num_SEx)
  Sstarcheck = BYTARR(num_SEx)

  Stot = DBLARR(num_SEx)
  Rtot = DBLARR(num_SEx)

  FOR xx = buffer2,sz[1]-buffer2-1 DO BEGIN
    FOR yy = buffer2,sz[2]-buffer2-1 DO BEGIN
      IF STimg[xx,yy] NE 0 THEN BEGIN
        ind = WHERE(Sid EQ STimg[xx,yy],count)
        IF count GT 0 THEN BEGIN
          Spix[ind] = Spix[ind] + 1
          Stot[ind] = Stot[ind] + Simg[xx,yy]
          Rtot[ind] = Rtot[ind] + Rimg[xx,yy]
        ENDIF
      ENDIF
    ENDFOR
  ENDFOR

  Sstarcheck = (Stot/Spix - Ssky2) LT (Sskysig + (Rtot/Spix - Rsky2)*ecntrat)

  galareacheck = (Sarea/Spix GT 0.20) ; Galaxy
  starareacheck = (Sarea/Spix LT 0.05) ; Maybe

; Now, put it all together
  star = (edgecheck OR classcheck2)
  maybe = (sizecheck OR classcheck1 OR flagcheck OR starcntcheck OR $
           satcheck OR magcheck OR Sstarcheck OR starareacheck)
  galaxy = (galcntcheck OR galareacheck)

  bad = WHERE(star OR (maybe AND 1b-galaxy),badcount)
; The "1b-" should be "NOT"s, but IDL takes 0b/1b and changes to 255b/254b
  good = WHERE(galaxy AND 1b-(star OR maybe),goodcount)

  PRINT,"Objects: ",STRTRIM(STRING(num_SEx),2)
  PRINT,"  Stars (exclude): ",STRTRIM(STRING(badcount),2)
  PRINT,"  Galaxy (include): ",STRTRIM(STRING(goodcount),2)

; Masklist is integer: -1 means mask, +1 means include, 0 means don't care.
  masklist = INTARR(num_SEx)
  IF goodcount GT 0 THEN masklist[good] = 1
  IF badcount GT 0 THEN masklist[bad] = -1
  useradj = BYTARR(num_SEx)
  initmask = masklist ; store original values, in case we want to reset.

; See if the mask list file exists to set initial states.
  listfile = STRMID(Ssexfile,0,STRLEN(Ssexfile)-9)+'_mask.list'
  listflag = 0b
  num_custom = 0
  IF FILE_TEST(odir+listfile) THEN BEGIN
    IF KEYWORD_SET(force) THEN BEGIN
      spawn,"/bin/rm -f "+odir+listfile
    ENDIF ELSE BEGIN
      readcol_new,odir+listfile,listid,newlist,baselist, $
                  FORMAT='(L,I,I)',COMMENT='#',/SILENT
      ind = WHERE(listid GT 0,count)
      ind2 = WHERE(listid LT 0,num_custom)
      IF count EQ num_SEx THEN BEGIN
; Only if the baselist matches EXACTLY will you come in here.
        IF TOTAL(baselist[ind] NE masklist) THEN BEGIN
          PRINT,"WARNING: Old initial mask doesn't match new one.  Definitions may have changed.",TOTAL(baselist[ind] NE masklist)
        ENDIF
        listflag = 1b
        PRINT,"Overriding with values stored in mask list file"
        useradj = (newlist[ind] EQ masklist)
        masklist = newlist[ind]

        IF num_custom GT 0 THEN BEGIN
          useradj = [useradj,BYTARR(num_custom)]
          masklist = [masklist,newlist[ind2]]
          Rid = [Rid,-1*(INDGEN(num_custom)+1)]
        ENDIF
      ENDIF ELSE BEGIN
        PRINT,"Mask list file outdated.  Ignoring its contents."
;        PRINT,count,num_SEx
;        FORPRINT,listid[ind],Rid,baselist[ind],masklist
      ENDELSE
    ENDELSE
  ENDIF

; Set up the basic masks.
  mask = make_array(sz[1], sz[2], /byte, value=goodval)
  incmask = make_array(sz[1],sz[2], /byte, value=goodval)
  IF FILE_TEST(odir+overfile) THEN BEGIN
    fits_read,odir+overfile,Omask,junk,/data_only
    ocheck = WHERE(Omask GT 0,overcount)
    overflux = TOTAL(Simg[ocheck]-Ssky2)/Nexptime
    Oimg = FIX(Omask)*0
    FOR jj = 1,MAX(Omask) DO BEGIN
      ocheck = WHERE(ABS(Omask-jj) LT 0.5,overcount)
      Oimg[ocheck] = -1*(jj+1)
    ENDFOR
  ENDIF ELSE BEGIN
    Omask = INTARR(sz[1],sz[2])
    Oimg = INTARR(sz[1],sz[2])
    overcount = LONG(0)
    overflux = DOUBLE(0)
  ENDELSE
  num_O_custom = 0
  FOR xx = buffer2,sz[1]-buffer2-1 DO BEGIN
    FOR yy = buffer2,sz[2]-buffer2-1 DO BEGIN
      IF Timg[xx,yy] NE 0 THEN BEGIN
        k = WHERE(Rid EQ Timg[xx,yy],nk)
        IF nk GT 0 THEN BEGIN
          mask[xx,yy] = (masklist[k[0]] EQ -1)
          incmask[xx,yy] = (masklist[k[0]] EQ 1)
        ENDIF
      ENDIF
    ENDFOR
  ENDFOR

  IF NOT (KEYWORD_SET(silent) AND ellflag) THEN BEGIN
; Now that we've got the basic bad-image detection done, let's allow
; the user to adjust it.  If there were no ellipses declared, we HAVE
; to do this.
    PRINT,"Entering interactive mode"
;------------------------------------
;| STEP 4: Set up interactive mode. |
;------------------------------------
    xsize = FIX((sz[1]-2*buffer2)/bxw) ; 1174 or 1024
    ysize = FIX((sz[2]-2*buffer2)/bxw)
    profsize = 560.0
;    profsize = 418.0 ; was 550
    bsize = 40.0+20.0*FLOAT(n_tlines)
    xmax = xsize+profsize
    ymax = ysize+bsize
    IF profsize LT 0 THEN BEGIN
      PRINT,"ERROR in proc4: window too small for plot",xsize,profsize
      RETURN
    ENDIF

; Set normalization factor:
    zoomnormal = FLOAT([xmax,ymax])
    xsep = FLOAT(xsize)/FLOAT(xmax)
    ysep = FLOAT(ysize)/FLOAT(ymax)
; Display window
    dw = LONG([buffer2,buffer2,(sz[1]-buffer2-1),(sz[2]-buffer2-1)])
    normal = [FLOAT(dw[2]-dw[0]+1)/xsep,FLOAT(dw[3]-dw[1]+1)/ysep]
    zoomscale = 1.0
; Set these just in case.
    xclick = xsize
    yclick = ymax-(bsize/3)

; Set up "highlighting" variables
    dmode = 1 ; start off in "dotted array" mode by default.
    dsize = [0.40,0.36,0.18,0.09] ; Symbol size in [intermittent,solid] mode
    dspace = [4*bxw,2*bxw,bxw,1] ; Mask overlay places one pixel every "dot" pixels
    dsym = [1,5,5,5] ; PSYM index
    dmodelist = [0,1]

; Set up Surface Brightness profile variables
; sb_box, the box size, should scale from 1 to 4
    IF ellflag THEN sb_box = 1+(FIX(SQRT(TOTAL(!pi*a*b/100000.))) < 3) $
               ELSE sb_box = 2
; This means 1-100k pixels is sb_box=1, 100k-400k is 2, 400k-900k is 3,
; 900k+ is 4

    mode = 0

    charsz = 1.25
    textsize = 0.016*charsz
    boxsize = FIX(0.4 * FLOAT(profsize))
    minisize = FIX(boxsize/4)

    textbox = textsize * 12
    plotmin = 1.0 - FLOAT(boxsize)/FLOAT(ymax)
    SBmax = plotmin - 2.0*textsize
    SBmin = textbox
    textmin = 2.0*textsize
    mainclip = [0.0,0.0,xsep,ysep]
    Rclip = [xsep,(SBmax+SBmin)/2.0,1.0,SBmax]
    Nclip = [xsep,SBmin,1.0,(SBmax+SBmin)/2.0]
    dataclip = [xsep,textmin,1.0,SBmin]
    buttonline = (FLOAT(n_tlines)+2.0*ysep)/FLOAT(n_tlines+2)
    buttonclip = [0.0,buttonline,xsep,1.0]
    textclip = [0.0,ysep,xsep,buttonline]

    sigmode = 0
    sigmin = [-2.0,-4.0,-8.0,-16.0]
    sigmax = [10.0,20.0,40.0,80.0]
    plotsig = [sigmin[sigmode],sigmax[sigmode]]
    n_sig_modes = N_ELEMENTS(sigmin) + 1

    fade = [0.8,0.7,0.6,0.5] ; How much the TVmode button alters the plot; 1.0 means it's black if it's outside

    singg_tvplot,Rimg[buffer2:sz[1]-buffer2-1,buffer2:sz[2]-buffer2-1], $
                 Nimg[buffer2:sz[1]-buffer2-1,buffer2:sz[2]-buffer2-1], $
                 Simg[buffer2:sz[1]-buffer2-1,buffer2:sz[2]-buffer2-1], $
                 [Rsky2,Rskysig],[Nsky2,Nskysig],[Ssky2,Sskysig],bxw,cntrat, $
                 POSITION=mainclip,OUTPUT=tvbase,PLOTSIG=plotsig
    tvdisp = SSQRT(tvbase) ; this puts tvdisp as the FOURTH root of the image, since tvbase is already the ssqrt.
    tvmode = 0
    zmode = 0

    Pcolor = !red
    ABCGcolor = !dgreen
    DEFcolor = !blue

    filtstr = '['+STRTRIM(SXPAR(Rhd,'FILTNAME'),2)+' : '+STRTRIM(SXPAR(Nhd,'FILTNAME'),2)+']'
    targstr = 'Targets: '+STRTRIM(SXPAR(Shd,'TARGLIST'),2)

    XYOUTS,xsep+(1.0-xsep)*[0.01,0.01],[(plotmin-0.6*textsize),(SBmax+0.4*textsize)], $
           [filtstr,targstr],CHARSIZE=charsz,COLOR=!black,ALIGNMENT=0,CHARTHICK=1.0,/NORMAL

    XYOUTS,xsep+(1.0-xsep)*[0.4,0.8,0.9],SBmax+(textsize * [1.4,1.4,0.4]), $
           [STRTRIM(Rfile,2),STRTRIM(Sfile,2),'('+STRTRIM(SXPAR(Shd,'TARGTYPE'),2)+')'], $
           CHARSIZE=charsz,COLOR=!black,ALIGNMENT=0.5,CHARTHICK=1.0,/NORMAL

    XYOUTS,xsep+(1.0-xsep)*0.5,textsize*[0.25,1.25], $
           ["Star = RED or (BLUE but not GREEN)","Galaxy = GREEN and not (RED or BLUE)"], $
           CHARSIZE=charsz,COLOR=!black,ALIGNMENT=0.5,CHARTHICK=1.0,/NORMAL

    binsize = 0.01

; Add separators between the plots, SB profiles, and buttons
    PLOTS,[xsep,1.0],plotmin*[1.0,1.0],COLOR=!black,/NORMAL
    PLOTS,[xsep,1.0],SBmax*[1.0,1.0],COLOR=!black,/NORMAL
    PLOTS,[xsep,1.0],(SBmax+SBmin)/2.0*[1.0,1.0],COLOR=!black,/NORMAL
    PLOTS,[xsep,1.0],SBmin*[1.0,1.0],COLOR=!black,/NORMAL
    PLOTS,[xsep,1.0],textmin*[1.0,1.0],COLOR=!black,/NORMAL
    PLOTS,xsep*[1.0,1.0],[0.0,1.0],COLOR=!black,/NORMAL
    PLOTS,[0.0,xsep],ysep*[1.0,1.0],COLOR=!black,/NORMAL
    PLOTS,[0.0,xsep],buttonclip[1]*[1.0,1.0],COLOR=!black,/NORMAL

; Add the buttons
    buttons = [["Write","Ellipse","Add","DS9","Zoom","Scale","Highlight","TV mode","Abort"], $
               ["Clear","Override","Add","Modify","Zoom","Scale","Highlight","TV mode","Reset"], $
               ["Inclusion","Mask","Add","DS9","Zoom","Scale","Highlight","TV mode","Reset"]]
    bcolors =  [[!dgreen,!blue,!blue,!lpurple,!blue,!blue,!blue,!blue,!red], $
                [!red,!blue,!blue,!blue,!blue,!blue,!blue,!blue,!lpurple], $
                [!blue,!blue,!blue,!lpurple,!blue,!blue,!blue,!blue,!lpurple]]
    bsz = SIZE(buttons)
    n_buttons = bsz[1]
    num_modes = bsz[2]
    button_x = xsep*(FINDGEN(n_buttons)+0.5)/FLOAT(n_buttons)
    button_y = buttonclip[1] + (buttonclip[3]-buttonclip[1])*(FLTARR(n_buttons)+0.5) - textsize/2.0
    FOR ii = 0,n_buttons-2 DO BEGIN
      PLOTS,buttonclip[0]+buttonclip[2]*FLOAT(ii+1)/FLOAT(n_buttons)*[1.0,1.0],[buttonclip[1],buttonclip[3]], $
            COLOR=!black,/NORMAL
    ENDFOR

    IF N_ELEMENTS(buttons) NE N_ELEMENTS(bcolors) THEN BEGIN
      PRINT,"ERROR in proc4: size mismatch on button arrays",bsz[1:2]
      RETURN
    ENDIF
    barea = [ROUND(FLOAT(xmax)*(buttonclip[2]-buttonclip[0])/FLOAT(n_buttons))-2,$
             ROUND(FLOAT(ymax)*(buttonclip[3]-buttonclip[1]))-2]
    buttonclear = INTARR(barea[0],barea[1]) + !white

    textarea = [ROUND(FLOAT(xmax)*(textclip[2]-textclip[0]))-2,$
                ROUND(FLOAT(ymax)*(textclip[3]-textclip[1]))-2]

; Add the "data" readout names
    datavars = [["SExtractor ID","X","Y","a [pix]","b [pix]","theta [deg]","a/b","CLASS","RA [h:m:s]","DEC [d:m:s]","Center (max)"], $
                ["Mag (R)","Mag (N)","N/R flux ratio","FLAGS (R)","FLAGS (N)","FLAGS (S)","FWHM/Seeing","FWHM area","Classification","CLASS > 0.8","Near edge"], $
                ["N/A","Area > 0.20","ratio > cntrat","Area < 0.05","ratio < cntrat","FLAGS -> star","CLASS > 0.2","FWHM < seeing","Saturated","R mag < "+STRMID(STRTRIM(STRING(rmin),2),0,5),"S mean < error"]]
    datatype = [["I","F7","F7","F5","F5","F5","F5","F5","S","S","F7"], $
                ["F6","F6","F5","I","I","I","F5","F5","S","I","I"], $
                ["I","I","I","I","I","I","I","I","I","I","I"]]
    Iind = WHERE(STRTRIM(datatype,2) EQ "I")
    Find = WHERE(STRMID(STRTRIM(datatype,2),0,1) EQ "F")
    Flen = STRMID(STRTRIM(datatype[Find],2),1,1) ; Number past decimal
    Sind = WHERE(STRTRIM(datatype,2) EQ "S")
    datacolors = [[!black,!black,!black,!black,!black,!black,!black,!black,!ddgray,!ddgray,!ddgray], $
                  [!black,!black,!ddgray,!black,!black,!black,!ddgray,!ddgray,!purple,!red,!red], $
                  [!dgreen,!dgreen,!dgreen,!blue,!blue,!blue,!blue,!blue,!blue,!blue,!blue]]
    datasz = SIZE(datavars)
    datavals = STRARR(datasz[1],datasz[2])
    ndcol = datasz[2]

; When we switch to ellipse mode, the data variables change.
    datavars_ell = [["Index","Isophote X","Isophote Y","Brightness X", $
                     "Brightness Y"],["a [pix]","b [pix]","(a/b)", $
                     "pa [deg]",""],["RA [h:m:s]","DEC [d:m:s]","","",""]]
    datacolors_ell = [[!black,!blue,!blue,!red,!red], $
                  [!dgreen,!dgreen,!blue,!ddgray,!white], $
                  [!blue,!blue,!white,!white,!white]]
    datasz_ell = SIZE(datavars_ell) 
    datavals_ell = STRARR(datasz_ell[1],datasz_ell[2])

; And when we switch to override mode, we don't need much.
    datavars_over = [["Object","Pixels","Flux"],["Tot. pixels","Tot. flux","SNR"],["Source","",""]]
    datacolors_over = [[!black,!blue,!blue],[!ddgray,!ddgray,!ddgray],[!black,!white,!white]]
    datasz_over = SIZE(datavars_over)
    datavals_over = STRARR(datasz_over[1],datasz_over[2])

    ellmask = BYTARR(sz[1],sz[2])
    zcheck = FLTARR(max_ellipses,sz[1],sz[2])+999.0
    FOR ell = 0,n_ellipses-1 DO BEGIN
      dist_ellipse,tempmask,[sz[1],sz[2]],Dx[ell],Dy[ell],(a[ell]/b[ell]),pa[ell]
      zcheck[ell,0:sz[1]-1,0:sz[2]-1] = tempmask/a[ell]
      ellmask = ellmask OR (tempmask LT a_f[ell])
    ENDFOR
    ellmask_old = ellmask
    zcheck_old = zcheck

    zmax = FLTARR(max_ellipses)+1.5

    FOR ii = 0,n_ellipses-1 DO BEGIN
; Find z to the centers of the other ellipses from the current one.
      IF ii EQ 0 THEN ztest = 999.0 $
                 ELSE ztest = zcheck[0:(ii-1),ROUND(Dx[ii]),ROUND(Dy[ii])] - 2.0*binsize*SQRT(a[0:(ii-1)]*b[0:(ii-1)]/(a[ii]*b[ii]))

      zcorner = zcheck[ii,[buffer2+10,sz[1]-buffer2-11],[buffer2+10,sz[2]-buffer2-11]]
      zmax[ii] = MIN([ztest,MAX(zcorner),1.5])
    ENDFOR
    zmax_old = zmax

    pointstr = ['Major axis','Major axis','Minor axis','Minor axis','Brightness peak']
    pointrad = 5.0

    datwidth = profsize/8
    datanameclear = INTARR(FIX(FLOAT(profsize)/FLOAT(ndcol))-datwidth-2, $
                           (SBmin-textmin)*ymax-2) + !white
    dataclear = INTARR(datwidth-2,(SBmin-textmin)*ymax-2) + !white
    xdat = FLTARR(ndcol)
    ydat = dataclip[3] - (FINDGEN(datasz[1])+1.0)/FLOAT(datasz[1]+1)*(dataclip[3]-dataclip[1]) - textsize/4.0
    ydat_ell = dataclip[3] - (FINDGEN(datasz_ell[1])+1.0)/FLOAT(datasz_ell[1]+1)*(dataclip[3]-dataclip[1]) - textsize/4.0
    ydat_over = dataclip[3] - (FINDGEN(datasz_over[1])+1.0)/FLOAT(datasz_over[1]+1)*(dataclip[3]-dataclip[1]) - textsize/4.0

    FOR colnum = 0,ndcol-1 DO BEGIN
      xpos = xsep + (1.0-xsep)*FLOAT(colnum)/FLOAT(ndcol)
      PLOTS,xpos*[1.0,1.0],[textmin,SBmin],COLOR=!black,/NORMAL
      xdat[colnum] = xpos + (1.0-xsep)*(FLOAT(1)/FLOAT(ndcol)-FLOAT(datwidth)/FLOAT(profsize))
      PLOTS,xdat[colnum]*[1.0,1.0],[textmin,SBmin],COLOR=!black,/NORMAL
    ENDFOR

; Overlay the dotted mask on the image.
    dotclr = [!red,!blue,!dgreen,!gray] ; Star, unknown, galaxy, off override
    IF dmode GT 0 THEN proc4_dot,Rid,Timg,dw, $
                       masklist,Omask,Oimg,mode,dotclr,ellcolor, $
                       dspace[dmodelist[dmode-1]], $
                       dsize[dmodelist[dmode-1]]*zoomscale, $
                       dsym[dmodelist[dmode-1]],normal,mainclip

; Add the ellipses to the plot, if the file exists.
    plot_ellipse,Dx-dw[0],Dy-dw[1],theta,a,b, $
                 ellcolor,barclr=!gray,normal=normal,clip=mainclip,symscale=zoomscale
    FOR ii = 0,n_ellipses-1 DO BEGIN
      IF ABS(z_f[ii] - 1.0) GT 0.01 THEN $
           plot_ellipse,Dx[ii]-dw[0],Dy[ii]-dw[1],theta[ii], $
           (a_f[ii]),(b_f[ii]),ellcolor[ii], $
           barclr=!dgray,normal=normal,clip=mainclip,symscale=zoomscale
    ENDFOR

    PLOTS,(Px-dw[0])/normal[0],(Py-dw[1])/normal[1],PSYM=SYM(13), $
        COLOR=ellcolor,SYMSIZE=2.0*zoomscale,/NORMAL

    ellmod = 0b

    Rscale = [MAX([2.0*Rsky2,3.0*Rskysig]),MAX([0.0,Rsky2-Rskysig])]
    Nscale = [MAX([2.0*Nsky2,3.0*Nskysig]),MAX([0.0,Nsky2-Nskysig])]
    Sscale = [Ssky2+5.0*Sskysig,Ssky2-2.0*Sskysig]

    proc4_text,'Setting up SB curve',!black,charsz,xmax,ymax,textclip

    PRINT,"Setting up SB curve, size="+STRTRIM(STRING(sb_box),2)
    proc4_sbgen,sb_box,zcheck,zmax,binsize,buffer2, $
                sz,n_ellipses,mask,Rimg,Nimg,Rsky2,Nsky2, $
                bin_num,bin_Rtot,bin_Ntot
    proc4_sbplot,binsize,bin_num,bin_Rtot,bin_Ntot,ellcolor, $
                 Rclip,Nclip,zoomnormal,2.0*charsz, $
                 a,zmax,Rscale,Nscale,Rsky2,Nsky2

; Store the old values, just in case.
    bin_num_old = bin_num
    bin_Rtot_old = bin_Rtot
    bin_Ntot_old = bin_Ntot

; One last setup, depending on which mode we're starting in.
    IF ellflag THEN BEGIN
      proc4_text,'Select objects to mask/unmask, choose semi-major distance to display, or choose another option.',!dgreen,charsz,xmax,ymax,textclip
      mode = 0
      FOR colnum = 0,ndcol-1 DO BEGIN
        xpos = xsep + (1.0-xsep)*FLOAT(colnum)/FLOAT(ndcol)
        XYOUTS,xpos+textsize/4.0,ydat,datavars[*,colnum],CHARSIZE=charsz, $
               COLOR=datacolors[*,colnum],ALIGNMENT=0,CHARTHICK=1.0, $
               CLIP=dataclip,NOCLIP=0,/NORMAL
      ENDFOR
    ENDIF ELSE BEGIN
      proc4_text,'Ellipse not initialized, must do this before proceeding; Select approximate center of first ellipse.',!dgreen,charsz,xmax,ymax,textclip
      mode = 1
      FOR colnum = 0,ndcol-1 DO BEGIN
        xpos = xsep + (1.0-xsep)*FLOAT(colnum)/FLOAT(ndcol)
        XYOUTS,xpos+textsize/4.0,ydat_ell,datavars_ell[*,colnum], $
               CHARSIZE=charsz,COLOR=datacolors_ell[*,colnum], $
               ALIGNMENT=0,CHARTHICK=1.0,CLIP=dataclip,NOCLIP=0,/NORMAL
      ENDFOR
    ENDELSE
    XYOUTS,button_x,button_y,buttons[*,mode],CHARSIZE=charsz*2.0,COLOR=bcolors[*,mode], $
           ALIGNMENT=0.5,CHARTHICK=2.0,CLIP=buttonclip,NOCLIP=0,/NORMAL

    WHILE mode GE 0 DO BEGIN
; Get user input
      CURSOR,xval,yval,/DOWN,/NORMAL

      index = 0
      IF (xval GE mainclip[0] AND xval LE mainclip[2]) AND $
         (yval GE mainclip[1] AND yval LE mainclip[3]) THEN BEGIN
        FOR colnum = 0,ndcol-1 DO BEGIN
; Clear the area for each column of numbers
          tv,dataclear,FIX(xdat[colnum]*xmax)+2,(textmin*ymax)+2
        ENDFOR

        xclick = (xval-mainclip[0])*normal[0] + dw[0]
        yclick = (yval-mainclip[1])*normal[1] + dw[1]

        CASE mode OF
        0: BEGIN
;----------------------------------------------------------
;| STEP 5a: Mask mode, clicking selects/unselects objects. |
;----------------------------------------------------------
          Tval = Timg[xclick,yclick]

          IF Tval GT 0 THEN BEGIN
; You clicked on an object directly...
            index = WHERE(Rid EQ Tval,numind)
            IF numind NE 1 THEN BEGIN
              PRINT,"ERROR in proc4: multiple ID matches in star file", $
                    Tval,numind
              RETURN
            ENDIF
          ENDIF ELSE BEGIN
; No object was directly selected, so find all the ones near this
; location
            IF Tval EQ 0 THEN BEGIN
              rad = 3 ; in pixels
              matcharr = BYTARR(num_SEx)
              FOR jj = MAX([FIX(xclick-rad),0]),MIN([FIX(xclick+rad),sz[1]-1]) DO BEGIN
                FOR kk = MAX([FIX(yclick-rad),0]),MIN([FIX(yclick+rad),sz[2]-1]) DO BEGIN
                  IF Timg[jj,kk] NE 0 THEN BEGIN
                    areaindex = WHERE(Rid EQ Timg[jj,kk],numarea)
                    IF numarea NE 1 THEN BEGIN
                      PRINT,"ERROR in proc4: multiple area ID matches in star file",Timg[jj,kk]
                      RETURN
                    ENDIF
                    matcharr[areaindex] = 1b
                  ENDIF
                ENDFOR
              ENDFOR
; Remake the index array
              index = WHERE(matcharr EQ 1b,numind)
            ENDIF ELSE BEGIN
; Custom objects have negative values of Timg.
              index = WHERE(Rid EQ Tval,numind)
            ENDELSE
          ENDELSE

          IF numind EQ 1 AND Tval GE 0 THEN BEGIN
; Only fill in data tables if one and only one object was selected,
; and if it wasn't a user-created object.
; Display the data values
            ind = index[0]

            Ivals = [Rid[ind],Rflag[ind],Nflag[ind],Sflag[ind], $
                     classcheck2[ind],edgecheck[ind], $
                     0,galareacheck[ind],galcntcheck[ind], $
                     starareacheck[ind],starcntcheck[ind],flagcheck[ind], $
                     classcheck1[ind],sizecheck[ind],satcheck[ind], $
                     magcheck[ind],Sstarcheck[ind]]
            Fvals = [Rx[ind],Ry[ind],Ra[ind],Rb[ind], $
                     ((Rtheta[ind]+360.0) MOD 180.0),Rr[ind], $
                     meanclass[ind],maxcenter[ind], $
                     Rmag[ind],Nmag[ind],fluxratio[ind]/cntrat, $
                     (Rfwhm[ind]/(Rseeing/as_pix)),(Sarea[ind]/Spix[ind])]
            xyad,Rhd,Rx[ind],Ry[ind],racen,deccen
            CASE initmask[ind] OF
              1: classtext = "GALAXY"
              0: classtext = "Unknown"
              -1: classtext = "STAR"
              ELSE: BEGIN
                      PRINT,"ERROR in proc4: invalid mask code ",masklist[ind],Rid[ind]
                      RETURN
                    END
            ENDCASE
            Svals = [degsexi(racen,/ra),degsexi(deccen),classtext]

            datavals[Iind] = STRTRIM(STRING(Ivals),2)
            FOR ii = 0,N_ELEMENTS(Fvals)-1 DO BEGIN
              datavals[Find[ii]] = STRMID(STRTRIM(STRING(Fvals[ii]),2),0,Flen[ii])
            ENDFOR
            datavals[Sind] = STRTRIM(Svals,2)

            FOR colnum = 0,ndcol-1 DO BEGIN
              XYOUTS,xdat[colnum]+0.5*(FLOAT(datwidth)/FLOAT(xmax)), $
                     ydat,datavals[*,colnum],CHARSIZE=charsz, $
                     COLOR=datacolors[*,colnum],ALIGNMENT=0.5,CHARTHICK=1.0, $
                     CLIP=dataclip,NOCLIP=0,/NORMAL

            ENDFOR
          ENDIF ELSE BEGIN
; If numind=0, do nothing.
            IF Tval LT 0 THEN proc4_text,'WARNING: Custom object selected; cannot display information.',!red,charsz,xmax,ymax,textclip
            IF numind GT 1 THEN proc4_text,'WARNING: Multiple objects selected; cannot display information.',!red,charsz,xmax,ymax,textclip
; Put the X,Y locations up anyway, though.  These are stored at
; [0:2,0] in the arrays.
            Svals = ["N/A",STRMID(STRTRIM(STRING(xclick),2),0,Flen[0]), $
                           STRMID(STRTRIM(STRING(yclick),2),0,Flen[1])]
            XYOUTS,xdat[0]+0.5*(FLOAT(datwidth)/FLOAT(xmax)), $
                   ydat[0:2],Svals,CHARSIZE=charsz, $
                   COLOR=datacolors[0:2,0],ALIGNMENT=0.5,CHARTHICK=1.0, $
                   CLIP=dataclip,NOCLIP=0,/NORMAL
          ENDELSE

; Start filling the plot windows
          proc4_miniplot,xclick,yclick,sz,buffer2,minisize,xsep,plotmin, $
                         tvbase,dw,zoomscale,Rimg,Simg,plotsig,boxsize, $
                         index,Timg,Rid,xsize,ysize,bsize,Rx,Ry,Rtheta, $
                         Ra,Rb,zoomnormal,normal,Rscale,Sscale,mode,mask,Rsky2

; Now, flip states.
          FOR ii = 0,numind-1 DO BEGIN
; As long as numind isn't 0 and we're not in ellipse mode, change the
; object to the next state.
            k = WHERE(Timg EQ Rid[index[ii]],nk)
            y = FIX(k/sz[1])
            x = k - y*sz[1]

; Loop through the three options instead of flipping.
            masklist[index[ii]] = ((masklist[index[ii]]+2) MOD 3)-1
            CASE masklist[index[ii]] OF
            -1: BEGIN
                  proc4_text,'  Excluding object #'+STRTRIM(STRING(Rid[index[ii]]),2),!blue,charsz,xmax,ymax,textclip
                  mask[k] = badval
                  incmask[k] = goodval
                END
             0: BEGIN
                  proc4_text,'  Unmasking object #'+STRTRIM(STRING(Rid[index[ii]]),2),!blue,charsz,xmax,ymax,textclip
                  mask[k] = goodval
                  incmask[k] = goodval
                END
             1: BEGIN
                  proc4_text,'  Including object #'+STRTRIM(STRING(Rid[index[ii]]),2),!blue,charsz,xmax,ymax,textclip
                  mask[k] = goodval
                  incmask[k] = badval
                END
            ENDCASE
            plotclr = dotclr[masklist[index[ii]]+1]
            IF Tval GE 0 THEN useradj[index[ii]] = (masklist[index[ii]] NE initmask[index[ii]])

; Erase the old line:
            proc4_sbplot,binsize,bin_num,bin_Rtot,bin_Ntot,!gray, $
                         Rclip,Nclip,zoomnormal,charsz*2.0, $
                         a,zmax,Rscale,Nscale,Rsky2,Nsky2,/noerase
            FOR jj = 0,n_ellipses-1 DO BEGIN
              FOR kk = LONG(0),LONG(nk-1) DO BEGIN
                IF (x[kk] MOD sb_box EQ (sb_box-1)/2) AND $
                   (y[kk] MOD sb_box EQ (sb_box-1)/2) THEN BEGIN
                  zbin = zcheck[*,x[kk],y[kk]]
                  bindex = FIX(zbin/binsize)

                  IF zbin[jj] LT zmax[jj] THEN BEGIN
                    delnum = LONG(sb_box^2)
                    delR = (Rimg[x[kk],y[kk]]-Rsky2)*delnum
                    delN = (Nimg[x[kk],y[kk]]-Nsky2)*delnum

                    valindex = WHERE(zbin-zmax LT 0.0,num_valid)
                    valindex2 = WHERE(zbin LT 1.0,num_valid2)
                    IF (num_valid EQ 1) OR (num_valid2 EQ 1 AND jj EQ valindex2[0]) OR (num_valid2 GT 1 AND jj EQ MAX(valindex2)) OR (num_valid2 EQ 0 AND jj EQ valindex[num_valid-1]) THEN BEGIN
                      IF masklist[index[ii]] LT 0 THEN BEGIN
                        bin_num[jj,bindex]  = bin_num[jj,bindex] - delnum
                        bin_Rtot[jj,bindex] = bin_Rtot[jj,bindex] - delR
                        bin_Ntot[jj,bindex] = bin_Ntot[jj,bindex] - delN
                      ENDIF ELSE BEGIN
                        IF masklist[index[ii]] EQ 0 THEN BEGIN
                          bin_num[jj,bindex]  = bin_num[jj,bindex] + delnum
                          bin_Rtot[jj,bindex] = bin_Rtot[jj,bindex] + delR
                          bin_Ntot[jj,bindex] = bin_Ntot[jj,bindex] + delN
                        ENDIF
; If masklist = 1 then we switched it from normal to Include, which
; doesn't actually affect our SB profiles.
                      ENDELSE
                    ENDIF
                  ENDIF
                ENDIF
              ENDFOR
; Draw the new line
            ENDFOR
            proc4_sbplot,binsize,bin_num,bin_Rtot,bin_Ntot,ellcolor, $
                         Rclip,Nclip,zoomnormal,charsz*2.0, $
                         a,zmax,Rscale,Nscale,Rsky2,Nsky2,/noerase

            IF dmode GT 0 THEN BEGIN
              FOR xx = FIX(MIN(FIX(x-dw[0])> 0)/dspace[dmodelist[dmode-1]])-1, $
                       FIX(MAX(FIX(x-dw[0])<(dw[2]-dw[0]))/dspace[dmodelist[dmode-1]]) DO BEGIN
                FOR yy = FIX(MIN(FIX(y-dw[1])>0)/dspace[dmodelist[dmode-1]])-1, $
                         FIX(MAX(FIX(y-dw[1])<(dw[3]-dw[1]))/dspace[dmodelist[dmode-1]]) DO BEGIN
                  xpos = xx*dspace[dmodelist[dmode-1]] + FIX(dspace[dmodelist[dmode-1]]/2.0)
                  ypos = yy*dspace[dmodelist[dmode-1]] + FIX(dspace[dmodelist[dmode-1]]/2.0)
                  IF mode LE 1 THEN BEGIN
                    IF Timg[xpos+dw[0],ypos+dw[1]] EQ Rid[index[ii]] THEN $
                      PLOTS,FLOAT(xpos)/normal[0],FLOAT(ypos)/normal[1], $
                            COLOR=plotclr,PSYM=SYM(dsym[dmodelist[dmode-1]]), $
                            SYMSIZE=dsize[dmodelist[dmode-1]]*zoomscale, $
                            CLIP=mainclip,NOCLIP=0,/NORMAL
                  ENDIF ELSE BEGIN
                    IF Omask[xpos+dw[0],ypos+dw[1]] GT 0 THEN $
                      PLOTS,FLOAT(xpos)/normal[0],FLOAT(ypos)/normal[1], $
                            COLOR=dotclr[3],PSYM=SYM(dsym[dmodelist[dmode-1]]), $
                            SYMSIZE=dsize[dmodelist[dmode-1]]*zoomscale, $
                            CLIP=mainclip,NOCLIP=0,/NORMAL
                  ENDELSE
                ENDFOR
              ENDFOR
            ENDIF
          ENDFOR
        END
;---------------------------------------------------------
;| STEP 5b: Ellipse mode, clicking defines new ellipses. |
;---------------------------------------------------------
        1: BEGIN
          IF n_ellipses EQ max_ellipses THEN BEGIN
            PRINT,"ERROR in proc4: exceeded limit on ellipses ",max_ellipses
            RETURN
          ENDIF

          proc4_miniplot,xclick,yclick,sz,buffer2,minisize,xsep,plotmin, $
                         tvbase,dw,zoomscale,Rimg,Simg,plotsig,boxsize, $
                         index,Timg,Rid,xsize,ysize,bsize,Rx,Ry,Rtheta, $
                         Ra,Rb,zoomnormal,normal,Rscale,Sscale,mode,mask,Rsky2

          proc4_text,'Select brightness peak for ellipse # '+STRTRIM(STRING(n_ellipses+1),2)+ $
                     ':',!dgreen,charsz,xmax,ymax,textclip
          CURSOR,tempPx,tempPy,/DOWN,/NORMAL
          PLOTS,tempPx,tempPy,PSYM=SYM(13),COLOR=Pcolor,SYMSIZE=4.0*zoomscale,/NORMAL
          Px[n_ellipses] = tempPx*normal[0] + dw[0]
          Py[n_ellipses] = tempPy*normal[1] + dw[1]

          proc4_text,'Select two endpoints of major axis:',!dgreen,charsz,xmax,ymax,textclip
          CURSOR,tempAx,tempAy,/DOWN,/NORMAL
          PLOTS,tempAx,tempAy,PSYM=SYM(12),COLOR=ABCGcolor,SYMSIZE=4.0*zoomscale,/NORMAL
          Ax=tempAx*normal[0] + dw[0]
          Ay=tempAy*normal[1] + dw[1]

          CURSOR,tempBx,tempBy,/DOWN,/NORMAL
          PLOTS,tempBx,tempBy,PSYM=SYM(12),COLOR=ABCGcolor,SYMSIZE=4.0*zoomscale,/NORMAL
          Bx=tempBx*normal[0] + dw[0]
          By=tempBy*normal[1] + dw[1]

          PLOTS,[tempAx,tempBx],[tempAy,tempBy],COLOR=ABCGcolor,/NORMAL

          proc4_text,'Select two endpoints of minor axis:',!dgreen,charsz,xmax,ymax,textclip
          CURSOR,tempCx,tempCy,/DOWN,/NORMAL
          PLOTS,tempCx,tempCy,PSYM=SYM(12),COLOR=ABCGcolor,SYMSIZE=4.0*zoomscale,/NORMAL
          Cx=tempCx*normal[0] + dw[0]
          Cy=tempCy*normal[1] + dw[1]

          CURSOR,tempGx,tempGy,/DOWN,/NORMAL
          PLOTS,tempGx,tempGy,PSYM=SYM(12),COLOR=ABCGcolor,SYMSIZE=4.0*zoomscale,/NORMAL
          Gx=tempGx*normal[0] + dw[0]
          Gy=tempGy*normal[1] + dw[1]

          PLOTS,[tempCx,tempGx],[tempCy,tempGy],COLOR=ABCGcolor,/NORMAL

; Define the ellipse, and make sure it's actually valid.
          define_ellipse,Ax,Ay,Bx,By,Cx,Cy,Gx,Gy,tempa,tempb,th, $
                         tempDx,tempDy,Ex,Ey,Fx,Fy
          IF tempa GE tempb THEN BEGIN
            theta[n_ellipses] = th
            a[n_ellipses] = tempa
            b[n_ellipses] = tempb
          ENDIF ELSE BEGIN
            theta[n_ellipses] = (th+(!dpi/2.d0)) MOD (2.d0*!dpi)
            a[n_ellipses] = tempb
            b[n_ellipses] = tempa
          ENDELSE
          pa[n_ellipses] = ((theta[n_ellipses]*!radeg)+90.0) MOD 180.0
          Dx[n_ellipses] = tempDx
          Dy[n_ellipses] = tempDy
          a_f[n_ellipses] = a[n_ellipses]
          b_f[n_ellipses] = b[n_ellipses]
          z_s[n_ellipses] = 1.0
          z_f[n_ellipses] = 1.0
          z_c[n_ellipses] = 1.0
          dist_ellipse,tempmask,[sz[1],sz[2]],Dx[n_ellipses],Dy[n_ellipses], $
                       (a[n_ellipses]/b[n_ellipses]),pa[n_ellipses]
          zcheck[n_ellipses,0:sz[1]-1,0:sz[2]-1] = tempmask/a[n_ellipses]
          ellmask = ellmask OR (tempmask LT a_f[n_ellipses])

          okayflag = 1b
          IF tempPx LT 0.0 OR tempPx GT 1.0 OR $
             tempPy LT 0.0 OR tempPy GT 1.0 OR $
             tempDx LT buffer2 OR tempDx GT (sz[1]-buffer2-1) OR $
             tempDy LT buffer2 OR tempDy GT (sz[2]-buffer2-1) THEN BEGIN
            proc4_text,'Invalid ellipse defined.  Values will not be stored.  '+ $
                       'Press RESET to restore values, or select region of next ellipse.', $
                       !red,charsz,xmax,ymax,textclip
            okayflag = 0b
          ENDIF ELSE BEGIN
            proc4_text,'Defining ellipse',!black,charsz,xmax,ymax,textclip
            PLOTS,(Dx[n_ellipses]-dw[0])/normal[0],(Dy[n_ellipses]-dw[1])/normal[1], $
                  PSYM=SYM(11),COLOR=DEFcolor,SYMSIZE=5.0*zoomscale,/NORMAL
            PLOTS,([Ex,Fx]-dw[0])/normal[0],([Ey,Fy]-dw[1])/normal[1],PSYM=-SYM(11), $
                  SYMSIZE=3.0*zoomscale,COLOR=DEFcolor,/NORMAL

            datavals_ell[0,0] = STRTRIM(STRING(n_ellipses+1),2)
            datavals_ell[1:4,0] = STRMID(STRTRIM(STRING([Dx[n_ellipses],Dy[n_ellipses],Px[n_ellipses],Py[n_ellipses]]),2),0,6)
            datavals_ell[0:3,1] = STRMID(STRTRIM(STRING([a[n_ellipses],b[n_ellipses],(a[n_ellipses]/b[n_ellipses]),pa[n_ellipses]]),2),0,6)
            xyad,Rhd,Dx[n_ellipses],Dy[n_ellipses],racen,deccen
            datavals_ell[0:1,2] = STRTRIM([degsexi(racen,/ra),degsexi(deccen)],2)
            FOR colnum = 0,ndcol-1 DO BEGIN
              XYOUTS,xdat[colnum]+0.5*(FLOAT(datwidth)/FLOAT(xmax)), $
                     ydat_ell,datavals_ell[*,colnum],CHARSIZE=charsz, $
                     COLOR=datacolors_ell[*,colnum],ALIGNMENT=0.5,CHARTHICK=1.0, $
                     CLIP=dataclip,NOCLIP=0,/NORMAL
            ENDFOR

; Add a new element to zmax
            zcorner = zcheck[n_ellipses,[buffer2+10,sz[1]-buffer2-11],[buffer2+10,sz[2]-buffer2-11]]
            ztest = FLTARR(max_ellipses)+1.5
            IF n_ellipses GE 1 THEN BEGIN
; If this is the first galaxy, there's no need for this check.
              FOR jj = 0,n_ellipses-1 DO BEGIN
                ztest[jj] = zcheck[n_ellipses,ROUND(Dx[jj]),ROUND(Dy[jj])] $
                         - 2.0*binsize*SQRT(a[jj]*b[jj]/(a[n_ellipses]*b[n_ellipses]))
              ENDFOR

;              ztest = zcheck[*,ROUND(Dx[n_ellipses]),ROUND(Dy[n_ellipses])] $
;                       - 2.0*binsize*SQRT(a*b/(a[n_ellipses]*b[n_ellipses]))

; We SHOULD reset the maximum z for this galaxy such that it doesn't reach the
; center of any previous ones.

; was zmax[n_ellipses] = ztest[0:n_ellipses-1] < zmax[n_ellipses]
              zmax[n_ellipses] = MIN([ztest[0:n_ellipses-1],zmax[n_ellipses]])
;              FOR ell = 0,n_ellipses-1 DO BEGIN
;                zmax[ell] = MIN([ztest[ell],zmax[ell]])
;              ENDFOR
            ENDIF
            zmax[n_ellipses] = MIN([ztest,MAX(zcorner)])
; Okay, we've defined the new ellipse.  Remake the plot.
            n_ellipses = n_ellipses + 1
            ellmod = 1b
          ENDELSE

          proc4_plot,tvdisp,mainclip,plotsig,dmode,Rid,Timg,dw,masklist, $
                     Omask,Oimg,mode,dotclr,dmodelist,dspace,dsize,zoomscale,dsym,$
                     normal,n_ellipses,Dx,Dy,Px,Py,theta,a,b,a_f,b_f,ellcolor

          IF okayflag THEN BEGIN
; Recalculate and redraw the SB profiles
            proc4_text,'Recalculating SB profiles',!black,charsz,xmax,ymax,textclip
            proc4_sbgen,sb_box,zcheck,zmax,binsize,buffer2, $
                        sz,n_ellipses,mask,Rimg,Nimg,Rsky2,Nsky2, $
                        bin_num,bin_Rtot,bin_Ntot
            proc4_sbplot,binsize,bin_num,bin_Rtot,bin_Ntot,ellcolor, $
                         Rclip,Nclip,zoomnormal,charsz*2.0, $
                         a,zmax,Rscale,Nscale,Rsky2,Nsky2

            proc4_text,'Ellipse #'+STRTRIM(STRING(n_ellipses),2)+' complete.  Select region of ellipse #'+STRTRIM(STRING(n_ellipses+1),2)+', RESET to restore original values, or Mask mode (through the Override button) to write files',!black,charsz,xmax,ymax,textclip
          ENDIF
        END
        2: BEGIN
;---------------------------------------------------------
;| STEP 5c: Override mode, clicking defines new regions. |
;---------------------------------------------------------
; Find the clicked polygon, and toggle it.
           Oval = Oimg[ROUND(xclick),ROUND(yclick)]

           IF Oval NE 0 THEN BEGIN
; Toggle state
             area = WHERE(Oimg EQ Oval,count)
             IF count GT 0 THEN BEGIN
               delflux = TOTAL(Simg[area]-Ssky2)/Nexptime
               delsnr = delflux/(TOTAL(Rimg[area]-Rsky2)/Nexptime*ecntrat)
               old_o = Omask[ROUND(xclick),ROUND(yclick)]
               new_o = ((old_o + 1) MOD (n_ellipses+1))
               Omask[area] = new_o
               IF new_o EQ 0 THEN BEGIN
                 proc4_text,'Disabling override on object #'+STRTRIM(STRING(Oval),2),!blue,charsz,xmax,ymax,textclip
                 overcount = overcount - count
                 overflux = overflux - delflux
               ENDIF ELSE BEGIN
                 proc4_text,'Allocating override on object #'+STRTRIM(STRING(Oval),2)+' to source #'+STRTRIM(STRING(new_o),2),!blue,charsz,xmax,ymax,textclip
                 overcount = overcount + count
                 overflux = overflux + delflux
               ENDELSE

               datavals_over[0,0] = STRTRIM(STRING(Oval),2)
               datavals_over[1,0] = STRTRIM(STRING(count),2)
               datavals_over[2,0] = STRMID(STRTRIM(STRING(delflux),2),0,6)
               datavals_over[0,1] = STRTRIM(STRING(overcount),2)
               datavals_over[1,1] = STRMID(STRTRIM(STRING(overflux),2),0,6)
               datavals_over[2,1] = STRMID(STRTRIM(STRING(delsnr),2),0,6)
               datavals_over[0,2] = STRTRIM(STRING(new_o),2)

               FOR colnum = 0,ndcol-1 DO BEGIN
                 XYOUTS,xdat[colnum]+0.5*(FLOAT(datwidth)/FLOAT(xmax)), $
                        ydat_over,datavals_over[*,colnum],CHARSIZE=charsz, $
                        COLOR=datacolors_over[*,colnum],ALIGNMENT=0.5,CHARTHICK=1.0, $
                        CLIP=dataclip,NOCLIP=0,/NORMAL
               ENDFOR

               y = FIX(area/sz[1])
               x = area - y*sz[1]

; Update dot plot.
               minX = MIN(LONG(x-dw[0])>0)
               maxX = MAX(LONG(x-dw[0])<(dw[2]-dw[0]))
               minY = MIN(LONG(y-dw[1])>0)
               maxY = MAX(LONG(y-dw[1])<(dw[3]-dw[1]))

               IF dmode GT 0 THEN BEGIN
                 FOR xx = FIX(minX/dspace[dmodelist[dmode-1]])-1, $
                          FIX(maxX/dspace[dmodelist[dmode-1]])+1 DO BEGIN
                   FOR yy = FIX(minY/dspace[dmodelist[dmode-1]])-1, $
                            FIX(maxY/dspace[dmodelist[dmode-1]])+1 DO BEGIN
                     xpos = xx*dspace[dmodelist[dmode-1]] + FIX(dspace[dmodelist[dmode-1]]/2.0)
                     ypos = yy*dspace[dmodelist[dmode-1]] + FIX(dspace[dmodelist[dmode-1]]/2.0)
                     IF Oimg[xpos+dw[0],ypos+dw[1]] NE 0 THEN BEGIN
                       Omval = Omask[xpos+dw[0],ypos+dw[1]]
                       IF Omval GT 0 THEN BEGIN
                         PLOTS,FLOAT(xpos)/normal[0],FLOAT(ypos)/normal[1], $
                               COLOR=ellcolor[Omval-1],PSYM=SYM(dsym[dmodelist[dmode-1]]), $
                               SYMSIZE=dsize[dmodelist[dmode-1]]*zoomscale,/NORMAL, $
                               CLIP=mainclip,NOCLIP=0
                       ENDIF ELSE BEGIN
                         PLOTS,FLOAT(xpos)/normal[0],FLOAT(ypos)/normal[1], $
                               COLOR=dotclr[3],PSYM=SYM(dsym[dmodelist[dmode-1]]), $
                               SYMSIZE=dsize[dmodelist[dmode-1]]*zoomscale,/NORMAL, $
                               CLIP=mainclip,NOCLIP=0
                       ENDELSE
                     ENDIF
                   ENDFOR
                 ENDFOR
               ENDIF
             ENDIF
           ENDIF

; Update mini plots
           proc4_miniplot,xclick,yclick,sz,buffer2,minisize,xsep,plotmin, $
                          tvbase,dw,zoomscale,Rimg,Simg,plotsig,boxsize, $
                          index,Timg,Rid,xsize,ysize,bsize,Rx,Ry,Rtheta, $
                          Ra,Rb,zoomnormal,normal,Rscale,Sscale,mode,mask,Rsky2

           END
        ENDCASE
      ENDIF ELSE BEGIN
        IF (xval GT mainclip[2] OR xval LT mainclip[0]) THEN BEGIN
;----------------------------------------------
;| STEP 6a: Clicking on plots shows ellipses. |
;----------------------------------------------
; The user clicked somewhere above the buttons, presumably in the SB 
; profiles.  Show the ellipses corresponding to this position.
          IF yval GT SBmin AND yval LT SBmax THEN BEGIN
            apos = ((xval-xsep)/(1.0 - xsep)) * (MAX(zmax*a))
            proc4_text,'Displaying ellipses at semimajor distance '+STRTRIM(STRING(apos),2),!blue,charsz,xmax,ymax,textclip
            plot_ellipse,Dx-dw[0],Dy-dw[1],theta,apos*(a/a),apos*(b/a),$
                         !yellow,normal=normal,clip=mainclip,symscale=zoomscale
          ENDIF ELSE BEGIN
; If you click in the mini plots up top, recenter them
            IF yval GE plotmin THEN BEGIN
              cenX = MIN([MAX([FIX(xclick-buffer2),minisize]), $
                               (sz[1]-2*buffer2-1)-minisize])
              cenY = MIN([MAX([FIX(yclick-buffer2),minisize]), $
                               (sz[2]-2*buffer2-1)-minisize])
              IF xval LT (xsep+0.2*(1.0-xsep)) THEN BEGIN
; Small plot; xpos and ypos range from -0.25 to +0.25
                xpos = (xval-xsep)/(0.4*(1.0-xsep)) - 0.25
                ypos = ((yval-plotmin)/(1.0-plotmin) MOD 0.5) - 0.25
              ENDIF ELSE BEGIN
; Large plot; xpos and ypos range from -0.5 to +0.5
                xpos = ((xval-(xsep+0.2*(1.0-xsep)))/(0.4*(1.0-xsep)) MOD 1.0) - 0.5
                ypos = (yval-plotmin)/(1.0-plotmin) - 0.5
              ENDELSE
              xclick = xpos*boxsize + cenX + buffer2
              yclick = ypos*boxsize + cenY + buffer2

              proc4_miniplot,xclick,yclick,sz,buffer2,minisize,xsep,plotmin, $
                             tvbase,dw,zoomscale,Rimg,Simg,plotsig,boxsize, $
                             index,Timg,Rid,xsize,ysize,bsize,Rx,Ry,Rtheta, $
                             Ra,Rb,zoomnormal,normal,Rscale,Sscale,mode,mask,Rsky2
; For now, instead of recentering the main plot, just put a tick mark
; on it so you can find whatever was clicked on.
              PLOTS,(xclick - dw[0])/normal[0],(yclick - dw[1])/normal[1], $
                    PSYM=SYM(11),COLOR=!dyellow,SYMSIZE=1.0*zoomscale, $
                    NOCLIP=0,CLIP=mainclip,/NORMAL
            ENDIF ELSE BEGIN
; If you click in the data table, do nothing for now.
            ENDELSE
          ENDELSE
        ENDIF ELSE BEGIN
;------------------------------------------------
;| STEP 6b: Clicking on buttons does... stuff. |
;------------------------------------------------
          IF (yval GT buttonclip[1] AND yval LE buttonclip[3]) THEN BEGIN
; You clicked a button.  Find out which one.
          bnum = FIX((xval-buttonclip[0])/(buttonclip[2]-buttonclip[0]) * FLOAT(n_buttons))
          CASE bnum OF
            0: BEGIN ; Clicked "Write","Clear", or "Inclusion"
                 CASE mode OF
                 0: BEGIN
; If we were in Mask mode, clicking "Write" exits the program by way
; of the mask creation algorithms.
                    proc4_text,'Writing outputs',!red,charsz,xmax,ymax,textclip
                    mode = -1
                    END
                 1: BEGIN
; If we were in Ellipse mode, the Clear button empties the ellipse arrays
                    ellmod = 1b
                    proc4_text,'Clearing arrays.  Select region of ellipse #1:',!dgreen,charsz,xmax,ymax,textclip
                    n_ellipses = 0
                    ellmask = BYTARR(sz[1],sz[2])
                    zcheck = FLTARR(max_ellipses,sz[1],sz[2])+999.0
                    Px = FLTARR(max_ellipses)
                    Py = FLTARR(max_ellipses)
                    Dx = FLTARR(max_ellipses)
                    Dy = FLTARR(max_ellipses)
                    a = FLTARR(max_ellipses)
                    b = FLTARR(max_ellipses)
                    pa = FLTARR(max_ellipses)
                    theta = FLTARR(max_ellipses)
                    z_s = FLTARR(max_ellipses)+1.0
                    z_f = FLTARR(max_ellipses)+1.0
                    z_c = FLTARR(max_ellipses)+1.0
                    zmax = FLTARR(max_ellipses)+1.5
                    END
                 2: BEGIN
; And if we were in Override mode, the Write button writes the
; Override file.
; We're in Override mode, so instead of a "Write" button, there's an
; "Inclusion" button.
                    proc4_text,'Adding inclusion mask, and growing by '+STRTRIM(STRING(grow),2)+' pixels',!black,charsz,xmax,ymax,textclip
                    IF MAX(incmask) EQ MIN(incmask) THEN BEGIN
                      incmasko = incmask
                    ENDIF ELSE BEGIN
; grow the "include" mask by grow, not 2*grow
                      incmask1 = incmask AND (Timg GT 0)
                      grow_mask, incmask1, incmasko, grow, goodval=badval, badval=goodval
; Don't grow user-created objects
                      incmasko = incmasko OR (incmask AND (Timg LT 0))
                    ENDELSE
                    ind = WHERE(incmasko,count)
                    IF count GT 0 THEN BEGIN
                      Oimg[ind] = -1
                      Omask = Omask*FIX(incmasko)
; We don't need to replot everything; we've only increased the number
; of included pixels, so just replot the dots.  This'll overwrite the
; ellipses and everything, though.
                      IF dmode GT 0 THEN BEGIN
                        proc4_dot,Rid,Timg,dw, $
                            masklist,Omask,Oimg,mode,dotclr,ellcolor, $
                            dspace[dmodelist[dmode-1]], $
                            dsize[dmodelist[dmode-1]]*zoomscale, $
                            dsym[dmodelist[dmode-1]],normal,mainclip
                      ENDIF
                    ENDIF
                    proc4_text,'Inclusion mask added; select new objects for overriding',!dgreen,charsz,xmax,ymax,textclip
                    END
                 ENDCASE
               END
            1: BEGIN ; clicked "Ellipse", "Override", or "Mask"
                 oldmode = mode
                 mode = (mode + 1) MOD num_modes

                 FOR jj = 0,n_buttons-1 DO BEGIN
; If a button is different than it was in the previous mode,
; replace it with the new text.
                   IF STRTRIM(buttons[jj,oldmode],2) NE $
                      STRTRIM(buttons[jj,mode],2) THEN BEGIN
                     tv,buttonclear,FIX(FLOAT(xmax)*button_x[jj])+1-(barea[0]/2), $
                                    FIX(FLOAT(ymax)*(button_y[jj]+textsize/2.0))+1-(barea[1]/2)
                     XYOUTS,button_x[jj],button_y[jj],buttons[jj,mode],CHARSIZE=charsz*2.0, $
                            COLOR=bcolors[jj,mode],ALIGNMENT=0.5,CHARTHICK=2.0, $
                            CLIP=buttonclip,NOCLIP=0,/NORMAL
                   ENDIF
                 ENDFOR
; Clear the data area
                 FOR colnum = 0,ndcol-1 DO BEGIN
                   tv,datanameclear,xsize+FIX(FLOAT(colnum*profsize)/FLOAT(ndcol))+2, $
                      (textmin*ymax)+2
                   tv,dataclear,FIX(xdat[colnum]*xmax)+2, $
                      (textmin*ymax)+2
                 ENDFOR

                 CASE mode OF
                   0: BEGIN
                      proc4_text,'Returning to Mask/Unmask mode.  Select objects to mask/unmask, or RESET to restore original values.',!black,charsz,xmax,ymax,textclip
                      FOR colnum = 0,ndcol-1 DO BEGIN
                        xpos = xsep + (1.0-xsep)*FLOAT(colnum)/FLOAT(ndcol)
                        XYOUTS,xpos+textsize/4.0,ydat,datavars[*,colnum],CHARSIZE=charsz, $
                               COLOR=datacolors[*,colnum],ALIGNMENT=0,CHARTHICK=1.0, $
                               CLIP=dataclip,NOCLIP=0,/NORMAL
                      ENDFOR
                      END
                   1: BEGIN
                      proc4_text,'Switching to Ellipse mode.  Select region of ellipse #'+STRTRIM(STRING(n_ellipses+1),2)+', CLEAR to empty arrays, or RESET to restore/add to previous definitions.',!black,charsz,xmax,ymax,textclip
                      FOR colnum = 0,ndcol-1 DO BEGIN
                        xpos = xsep + (1.0-xsep)*FLOAT(colnum)/FLOAT(ndcol)
                        XYOUTS,xpos+textsize/4.0,ydat_ell,datavars_ell[*,colnum],CHARSIZE=charsz, $
                               COLOR=datacolors_ell[*,colnum],ALIGNMENT=0,CHARTHICK=1.0, $
                               CLIP=dataclip,NOCLIP=0,/NORMAL
                      ENDFOR
                      END
                   2: BEGIN
                      proc4_text,'Switching to Override mode.  Draw objects to add to masks, MASK to switch to mask mode (and write files) or RESET to remove all override regions.',!black,charsz,xmax,ymax,textclip
; All we needed was to change mode and re-plot.
                      FOR colnum = 0,ndcol-1 DO BEGIN
                        xpos = xsep + (1.0-xsep)*FLOAT(colnum)/FLOAT(ndcol)
                        XYOUTS,xpos+textsize/4.0,ydat_over,datavars_over[*,colnum],CHARSIZE=charsz, $
                               COLOR=datacolors_over[*,colnum],ALIGNMENT=0,CHARTHICK=1.0, $
                               CLIP=dataclip,NOCLIP=0,/NORMAL
                      ENDFOR
                      END
                   ELSE: BEGIN
                        PRINT,"ERROR in proc4: invald mode ",mode
                        RETURN
                         END
                 ENDCASE
; Plot it.  In the case of Ellipse mode, the ellipses will have been removed.
                 proc4_plot,tvdisp,mainclip,plotsig,dmode,Rid,Timg,dw, $
                            masklist,Omask,Oimg,mode,dotclr,dmodelist,dspace,$
                            dsize,zoomscale,dsym,normal,n_ellipses, $
                            Dx,Dy,Px,Py,theta,a,b,a_f,b_f,ellcolor
               END
            2: BEGIN ; Clicked "Add"
                 proc4_text,'Manual object entry; select first point (or click outside area to finish), or select single point for circular aperture',!dgreen,charsz,xmax,ymax,textclip
                 doneflag = 0b
                 numpoints = 0
                 maxpoints = 100
                 poly = FLTARR(maxpoints,2)
                 xpoly = FLTARR(maxpoints)
                 ypoly = FLTARR(maxpoints)
                 WHILE NOT doneflag DO BEGIN
                   CURSOR,xval,yval,/DOWN,/NORMAL
                   IF xval GT xsep THEN BEGIN
                     doneflag = 1b
                   ENDIF ELSE BEGIN
                     IF numpoints EQ 0 THEN BEGIN
                       xclick = xval*normal[0] + dw[0]
                       yclick = yval*normal[1] + dw[1]
                       proc4_miniplot,xclick,yclick,sz,buffer2,minisize,xsep,plotmin, $
                                      tvbase,dw,zoomscale,Rimg,Simg,plotsig,boxsize, $
                                      index,Timg,Rid,xsize,ysize,bsize,Rx,Ry,Rtheta, $
                                      Ra,Rb,zoomnormal,normal,Rscale,Sscale,mode,mask,Rsky2
                     ENDIF
                     xpoly[numpoints] = xval
                     ypoly[numpoints] = yval

                     poly[numpoints,0] = (xval/xsep)*FLOAT(dw[2]-dw[0]+1)+dw[0] + 1.0
                     poly[numpoints,1] = (yval/ysep)*FLOAT(dw[3]-dw[1]+1)+dw[1] + 1.0

; In main plot window:
                     PLOTS,xpoly[0:numpoints],ypoly[0:numpoints], $
                           COLOR=!dorange,PSYM=-SYM(1),SYMSIZE=0.5,THICK=1.0
; In mini plot windows:
                     cenX = MIN([MAX([FIX(xclick),boxsize/2]),sz[1]-boxsize/2-1])
                     cenY = MIN([MAX([FIX(yclick),boxsize/2]),sz[2]-boxsize/2-1])

                     xmini = 0.4*(1.0-xsep)*((FLOAT(poly[0:numpoints,0]-(cenX-boxsize/2))/FLOAT(boxsize+1) > 0.0) < 1.0)
                     ymini = (1.0-plotmin)*((FLOAT(poly[0:numpoints,1]-(cenY-boxsize/2))/FLOAT(boxsize+1) > 0.0) < 1.0)

                     PLOTS,xsep+0.2*(1.0-xsep) + xmini,plotmin + ymini, $
                           COLOR=!dorange,PSYM=-SYM(1),SYMSIZE=0.5,THICK=1.0
                     PLOTS,xsep+0.6*(1.0-xsep) + xmini,plotmin + ymini, $
                           COLOR=!dorange,PSYM=-SYM(1),SYMSIZE=0.5,THICK=1.0

                     numpoints = numpoints + 1
                     IF numpoints GE maxpoints THEN BEGIN
                       proc4_text,'ERROR: too many polygon points.',!red,charsz,xmax,ymax,textclip
                       doneflag = 1b
                     ENDIF
                   ENDELSE
                 ENDWHILE
; Now that we've defined the polygon, work through the possibilities.
                 IF numpoints EQ 0 THEN BEGIN
                   proc4_text,'No polygon added',!red,charsz,xmax,ymax,textclip
                 ENDIF ELSE BEGIN
                   IF numpoints EQ 1 THEN BEGIN
; Point mode; use "grow" radius
                     PLOTS,xpoly[0],ypoly[0],COLOR=!dorange,PSYM=SYM(6),SYMSIZE=1.0*zoomscale
                     dist_circle,dist,[sz[1],sz[2]],FIX(poly[0,0]),FIX(poly[0,1])
                     area = WHERE(dist LT 2.0*grow,areacount)
                   ENDIF ELSE BEGIN
                     IF numpoints GT 2 THEN BEGIN
; Polygon mode; connect the final side.
                       PLOTS,[xpoly[numpoints-1],xpoly[0]], $
                             [ypoly[numpoints-1],ypoly[0]], $
                             COLOR=!dorange,PSYM=-SYM(1),SYMSIZE=0.5,THICK=1.0
                       PLOTS,xsep+0.2*(1.0-xsep) + [xmini[0],xmini[numpoints-1]], $
                             plotmin + [ymini[0],ymini[numpoints-1]], $
                             COLOR=!dorange,PSYM=-SYM(1),SYMSIZE=0.5,THICK=1.0
                       PLOTS,xsep+0.6*(1.0-xsep) + [xmini[0],xmini[numpoints-1]], $
                             plotmin + [ymini[0],ymini[numpoints-1]], $
                             COLOR=!dorange,PSYM=-SYM(1),SYMSIZE=0.5,THICK=1.0

                       area = polyfillv(FIX(poly[0:numpoints-1,0]), $
                                        FIX(poly[0:numpoints-1,1]),sz[1],sz[2])
                       areacount = N_ELEMENTS(area)
                     ENDIF ELSE BEGIN
                       proc4_text,'Cannot create 2-point polygons.  Ignoring.',!red,charsz,xmax,ymax,textclip
                     ENDELSE
                   ENDELSE
                   y = FIX(area/sz[1])
                   x = area - y*sz[1]

                   IF mode LE 1 THEN BEGIN
; Overwrite old data, setting to -N on all pixels
                     num_custom = num_custom + 1
                     proc4_text,'Adding custom object #'+STRTRIM(STRING(num_custom),2),!blue,charsz,xmax,ymax,textclip
                     Rid = [Rid,(-1*num_custom)]
                     masklist = [masklist,0] ; start in "don't care" mode
                     Timg_old = Timg
                     Timg[area] = -1*num_custom
                     mask[area] = 0b
                     incmask[area] = 0b

; Update the SB profiles
                     proc4_sbplot,binsize,bin_num,bin_Rtot,bin_Ntot,!gray, $
                                  Rclip,Nclip,zoomnormal,charsz*2.0, $
                                  a,zmax,Rscale,Nscale,Rsky2,Nsky2
                     FOR jj = 0,n_ellipses-1 DO BEGIN
                       FOR kk = LONG(0),LONG(areacount-1) DO BEGIN
                         IF (x[kk] MOD sb_box EQ (sb_box-1)/2) AND $
                            (y[kk] MOD sb_box EQ (sb_box-1)/2) THEN BEGIN
                           IF Timg_old[x[kk],y[kk]] NE 0 THEN BEGIN
                             index = WHERE(Rid EQ Timg_old[x[kk],y[kk]],indcount)
                             IF masklist[index[0]] EQ -1 THEN BEGIN
; Since we're setting masklist[custom] = 0, we only need to change the
; binned data if it was -1 before.
                               zbin = zcheck[*,x[kk],y[kk]]
                               bindex = FIX(zbin/binsize)

                               IF zbin[jj] LT zmax[jj] THEN BEGIN
                                 delnum = LONG(sb_box^2)
                                 delR = (Rimg[x[kk],y[kk]]-Rsky2)*delnum
                                 delN = (Nimg[x[kk],y[kk]]-Nsky2)*delnum

                                 valindex = WHERE(zbin-zmax LT 0.0,num_valid)
                                 valindex2 = WHERE(zbin LT 1.0,num_valid2)
                                 IF (num_valid EQ 1) OR (num_valid2 EQ 1 AND jj EQ valindex2[0]) OR (num_valid2 GT 1 AND jj EQ MAX(valindex2)) OR (num_valid2 EQ 0 AND jj EQ valindex[num_valid-1]) THEN BEGIN
                                   bin_num[jj,bindex]  = bin_num[jj,bindex] + delnum
                                   bin_Rtot[jj,bindex] = bin_Rtot[jj,bindex] + delR
                                   bin_Ntot[jj,bindex] = bin_Ntot[jj,bindex] + delN
                                 ENDIF
                               ENDIF
                             ENDIF
                           ENDIF
                         ENDIF
                       ENDFOR
                     ENDFOR
                     proc4_sbplot,binsize,bin_num,bin_Rtot,bin_Ntot,ellcolor, $
                                  Rclip,Nclip,zoomnormal,charsz*2.0, $
                                  a,zmax,Rscale,Nscale,Rsky2,Nsky2
                   ENDIF ELSE BEGIN
                     num_O_custom = num_O_custom + 1
                     proc4_text,'Adding custom Override object #'+STRTRIM(STRING(num_O_custom),2)+' to source#1',!blue,charsz,xmax,ymax,textclip
                     Omask[area] = 1
                     Oimg[area] = num_O_custom
                     ocheck = WHERE(Omask GT 0,overcount)
                     delflux = TOTAL(Simg[area]-Ssky2)/Nexptime
                     delsnr = delflux / (TOTAL(Rimg[area]-Rsky2)/Nexptime*ecntrat)
                     IF overcount GT 0 THEN BEGIN
                       overflux = TOTAL(Simg[ocheck]-Ssky2)/Nexptime
                     ENDIF ELSE BEGIN
                       overflux = 0.0
                     ENDELSE
                     datavals_over[0,0] = STRTRIM(STRING(num_O_custom),2)
                     datavals_over[1,0] = STRTRIM(STRING(areacount),2)
                     datavals_over[2,0] = STRMID(STRTRIM(STRING(delflux),2),0,6)
                     datavals_over[0,1] = STRMID(STRING(overcount),2)
                     datavals_over[1,1] = STRMID(STRTRIM(STRING(overflux),2),0,6)
                     datavals_over[2,1] = STRMID(STRTRIM(STRING(delsnr),2),0,6)
                     datavals_over[0,2] = '1'

                     FOR colnum = 0,ndcol-1 DO BEGIN
                       tv,dataclear,FIX(xdat[colnum]*xmax)+2,(textmin*ymax)+2
                       XYOUTS,xdat[colnum]+0.5*(FLOAT(datwidth)/FLOAT(xmax)), $
                              ydat_over,datavals_over[*,colnum],CHARSIZE=charsz, $
                              COLOR=datacolors_over[*,colnum],ALIGNMENT=0.5,CHARTHICK=1.0, $
                              CLIP=dataclip,NOCLIP=0,/NORMAL
                     ENDFOR

                   ENDELSE

                   IF dmode GT 0 THEN BEGIN
                     FOR xx = FIX(MIN(LONG(x-dw[0])>0)/dspace[dmodelist[dmode-1]])-1, $
                              FIX(MAX(LONG(x-dw[0])<(dw[2]-dw[0]))/dspace[dmodelist[dmode-1]])+1 DO BEGIN
                       FOR yy = FIX(MIN(LONG(y-dw[1])>0)/dspace[dmodelist[dmode-1]])-1, $
                                FIX(MAX(LONG(y-dw[1])<(dw[3]-dw[1]))/dspace[dmodelist[dmode-1]])+1 DO BEGIN
                         xpos = xx*dspace[dmodelist[dmode-1]] + FIX(dspace[dmodelist[dmode-1]]/2.0)
                         ypos = yy*dspace[dmodelist[dmode-1]] + FIX(dspace[dmodelist[dmode-1]]/2.0)
                         IF mode LE 1 THEN BEGIN
                           IF Timg[xpos+dw[0],ypos+dw[1]] EQ -1*num_custom THEN $
                             PLOTS,FLOAT(xpos)/normal[0],FLOAT(ypos)/normal[1], $
                                   COLOR=dotclr[1],PSYM=SYM(dsym[dmodelist[dmode-1]]), $
                                   SYMSIZE=dsize[dmodelist[dmode-1]]*zoomscale,/NORMAL, $
                                   CLIP=mainclip,NOCLIP=0
                         ENDIF ELSE BEGIN
                           IF Oimg[xpos+dw[0],ypos+dw[1]] NE 0 THEN BEGIN
                             Omval = Omask[xpos+dw[0],ypos+dw[1]]
                             IF Omval GT 0 THEN BEGIN
                               PLOTS,FLOAT(xpos)/normal[0],FLOAT(ypos)/normal[1], $
                                     COLOR=ellcolor[Omval-1],PSYM=SYM(dsym[dmodelist[dmode-1]]), $
                                     SYMSIZE=dsize[dmodelist[dmode-1]]*zoomscale,/NORMAL, $
                                     CLIP=mainclip,NOCLIP=0
                             ENDIF ELSE BEGIN
                               PLOTS,FLOAT(xpos)/normal[0],FLOAT(ypos)/normal[1], $
                                     COLOR=dotclr[3],PSYM=SYM(dsym[dmodelist[dmode-1]]), $
                                     SYMSIZE=dsize[dmodelist[dmode-1]]*zoomscale,/NORMAL, $
                                     CLIP=mainclip,NOCLIP=0
                             ENDELSE
                           ENDIF
                         ENDELSE
                       ENDFOR
                     ENDFOR
                   ENDIF
                   proc4_text,'New object added, select objects as usual.',!dgreen,charsz,xmax,ymax,textclip
                 ENDELSE
               END
            3: BEGIN ; Clicked "DS9" or "Modify"
                 IF mode NE 1 THEN BEGIN
                   proc4_text,'Spawning ds9 session in new window',!black,charsz,xmax,ymax,textclip
                   command = "ds9 -zscale -zoom 0.5 -geometry 950x950 "+ $
                            idir+Rfile+" "+idir+Nfile+" "+idir+Sfile+" -single &"
                   spawn,command
                 ENDIF ELSE BEGIN
                   proc4_text,'Modifying ellipses; drag points',!dgreen,charsz,xmax,ymax,textclip
                   doneflag_mod = 0b
                   WHILE NOT doneflag_mod DO BEGIN
                     xmod = FLTARR(5,n_ellipses)
                     ymod = FLTARR(5,n_ellipses)

                     xmod[0,0:n_ellipses-1] = Dx[0:n_ellipses-1] + a[0:n_ellipses-1]*COS(theta[0:n_ellipses-1]) ; A
                     xmod[1,0:n_ellipses-1] = Dx[0:n_ellipses-1] - a[0:n_ellipses-1]*COS(theta[0:n_ellipses-1]) ; B
                     xmod[2,0:n_ellipses-1] = Dx[0:n_ellipses-1] - b[0:n_ellipses-1]*SIN(theta[0:n_ellipses-1]) ; C
                     xmod[3,0:n_ellipses-1] = Dx[0:n_ellipses-1] + b[0:n_ellipses-1]*SIN(theta[0:n_ellipses-1]) ; G
                     xmod[4,0:n_ellipses-1] = Px[0:n_ellipses-1]

                     ymod[0,0:n_ellipses-1] = Dy[0:n_ellipses-1] + a[0:n_ellipses-1]*SIN(theta[0:n_ellipses-1])
                     ymod[1,0:n_ellipses-1] = Dy[0:n_ellipses-1] - a[0:n_ellipses-1]*SIN(theta[0:n_ellipses-1])
                     ymod[2,0:n_ellipses-1] = Dy[0:n_ellipses-1] + b[0:n_ellipses-1]*COS(theta[0:n_ellipses-1])
                     ymod[3,0:n_ellipses-1] = Dy[0:n_ellipses-1] - b[0:n_ellipses-1]*COS(theta[0:n_ellipses-1])
                     ymod[4,0:n_ellipses-1] = Py[0:n_ellipses-1]

                     CURSOR,oldX,oldY,/DOWN,/NORMAL
                     IF oldX GT mainclip[0] AND oldY GT mainclip[1] AND $
                        oldX LT mainclip[2] AND oldY LT mainclip[3] THEN BEGIN
                       OtempX = oldX*normal[0] + dw[0]
                       OtempY = oldY*normal[1] + dw[1]
                       ind = WHERE(ABS(OtempX - xmod) LT pointrad AND $
                                   ABS(OtempY - ymod) LT pointrad,count)
                       IF count EQ 0 THEN BEGIN
                         proc4_text,'No point selected',!red,charsz,xmax,ymax,textclip
                       ENDIF ELSE BEGIN
                         snum = FIX(ind[0]/5)
                         pnum = ind[0] - snum*5
                         proc4_text,'Point selected: '+pointstr[pnum]+' of Source#'+STRTRIM(STRING(snum+1),2),!blue,charsz,xmax,ymax,textclip
                         CURSOR,newX,newY,/UP,/NORMAL
                         NtempX = newX*normal[0] + dw[0]
                         NtempY = newY*normal[1] + dw[1]

                         CASE pnum OF
                           0: BEGIN
                                Dx[snum] = (NtempX+xmod[1,snum])/2.0
                                Dy[snum] = (NtempY+ymod[1,snum])/2.0
                                ydist = NtempY - ymod[1,snum]
                                xdist = NtempX - xmod[1,snum]
                                theta[snum] = ATAN(ydist,xdist)
                                a[snum] = SQRT(xdist^2 + ydist^2)/2.0
                              END
                           1: BEGIN
                                Dx[snum] = (NtempX+xmod[0,snum])/2.0
                                Dy[snum] = (NtempY+ymod[0,snum])/2.0
                                ydist = ymod[0,snum] - NtempY
                                xdist = xmod[0,snum] - NtempX
                                theta[snum] = ATAN(ydist,xdist)
                                a[snum] = SQRT(xdist^2 + ydist^2)/2.0
                              END
                           2: BEGIN
                                Dx[snum] = (NtempX+xmod[3,snum])/2.0
                                Dy[snum] = (NtempY+ymod[3,snum])/2.0
                                ydist = NtempY - ymod[3,snum]
                                xdist = NtempX - xmod[3,snum]
                                theta[snum] = ATAN(ydist,xdist)+!pi/2.0
                                b[snum] = SQRT(xdist^2 + ydist^2)/2.0
                              END
                           3: BEGIN
                                Dx[snum] = (NtempX+xmod[2,snum])/2.0
                                Dy[snum] = (NtempY+ymod[2,snum])/2.0
                                ydist = ymod[2,snum] - NtempY
                                xdist = xmod[2,snum] - NtempX
                                theta[snum] = ATAN(ydist,xdist)+!pi/2.0
                                b[snum] = SQRT(xdist^2 + ydist^2)/2.0
                              END
                           4: BEGIN
                                Px[snum] = NtempX
                                Py[snum] = NtempY
                              END
                         ENDCASE
                         a_f[snum] = a[snum]
                         b_f[snum] = b[snum]
                         z_s[snum] = 1.0
                         z_f[snum] = 1.0
                         z_c[snum] = 1.0
                         pa[snum] = ((theta[snum] * !radeg) + 90.0) MOD 180.0
                         dist_ellipse,tempmask,[sz[1],sz[2]],Dx[snum],Dy[snum], $
                                      (a[snum]/b[snum]),pa[snum]
                         zcheck[snum,0:sz[1]-1,0:sz[2]-1] = tempmask/a[snum]
                         ellmask = BYTARR(sz[1],sz[2])

                         FOR kk = 0,n_ellipses-1 DO BEGIN
                           ellmask = ellmask OR (tempmask LT a_f[snum])
                         ENDFOR

                         proc4_plot,tvdisp,mainclip,plotsig,dmode,Rid,Timg,dw,masklist, $
                                    Omask,Oimg,mode,dotclr,dmodelist,dspace,dsize,zoomscale,dsym,$
                                    normal,n_ellipses,Dx,Dy,Px,Py,theta,a,b,a_f,b_f,ellcolor

; Recalculate and redraw the SB profiles
                         proc4_text,'Recalculating SB profiles',!black,charsz,xmax,ymax,textclip
                         proc4_sbgen,sb_box,zcheck,zmax,binsize,buffer2, $
                                     sz,n_ellipses,mask,Rimg,Nimg,Rsky2,Nsky2, $
                                     bin_num,bin_Rtot,bin_Ntot
                         proc4_sbplot,binsize,bin_num,bin_Rtot,bin_Ntot,ellcolor, $
                                      Rclip,Nclip,zoomnormal,charsz*2.0, $
                                      a,zmax,Rscale,Nscale,Rsky2,Nsky2
                         proc4_text,'Select a new point to modify',!black,charsz,xmax,ymax,textclip
                       ENDELSE
                     ENDIF ELSE BEGIN
                       doneflag_mod = 1b
                       proc4_text,'Returning to ellipse mode.  Select the region of ellipse #'+STRTRIM(STRING(n_ellipses+1),2)+', CLEAR to empty arrays, or RESET to restore/add to previous definitions.',!dgreen,charsz,xmax,ymax,textclip
                     ENDELSE
                   ENDWHILE
                 ENDELSE
               END
            4: BEGIN ; Clicked "Zoom"
                 zmode = zmode + 1
                 proc4_text,'Select corner #1 of zoom area (select outside plot for full zoom)',!dgreen,charsz,xmax,ymax,textclip
                 CURSOR,xval1,yval1,/DOWN
                 IF xval1 GT xsep OR yval1 GT ysep THEN BEGIN
; Return to original plot area
                   proc4_text,'Restoring original x0.5 zoom',!black,charsz,xmax,ymax,textclip
                   dw = LONG([buffer2,buffer2,sz[1]-buffer2-1,sz[2]-buffer2-1])
                   zoomscale = 1.0
                 ENDIF ELSE BEGIN
                   xpos1 = xval1*normal[0] + dw[0]
                   ypos1 = yval1*normal[1] + dw[1]

                   proc4_text,'Select corner #2 of zoom area (select outside plot to zoom out by x2)',!dgreen,charsz,xmax,ymax,textclip
                   CURSOR,xval2,yval2,/DOWN
                   IF xval2 GT xsep OR yval2 GT ysep THEN BEGIN
; Second point was outside the plot area, return to original plot.
                     proc4_text,'Decreasing zoom by x2',!black,charsz,xmax,ymax,textclip
                     IF zoomscale GE 2.0 THEN BEGIN
                       dwhalf = [ABS(dw[2]-dw[0]),ABS(dw[3]-dw[1])]
                       cenX = (((dw[2]+dw[0])/2) < (sz[1]-buffer2-1-dwhalf[0])) > (buffer2 + dwhalf[0])
                       cenY = (((dw[3]+dw[1])/2) < (sz[2]-buffer2-1-dwhalf[1])) > (buffer2 + dwhalf[1])

                       dw = LONG([cenX-dwhalf[0],cenY-dwhalf[1], $
                                  cenX+dwhalf[0],cenY+dwhalf[1]])
                       zoomscale = (zoomscale/2.0)
                     ENDIF ELSE BEGIN
                       proc4_text,'Restoring original x0.5 zoom',!black,charsz,xmax,ymax,textclip
                       dw = LONG([buffer2,buffer2,sz[1]-buffer2-1,sz[2]-buffer2-1])
                       zoomscale = 1.0
                     ENDELSE
                   ENDIF ELSE BEGIN
                     xpos2 = xval2*normal[0] + dw[0]
                     ypos2 = yval2*normal[1] + dw[1]

                     zoomscale = MIN([FLOAT(sz[1]-2*buffer2)/ABS(xpos2-xpos1), $
                                      FLOAT(sz[2]-2*buffer2)/ABS(ypos2-ypos1)])
                     zoomtext = STRTRIM(STRING(zoomscale/FLOAT(bxw)),2)
                     proc4_text,'Zooming: new scale is x'+STRMID(zoomtext,0,10),!blue,charsz,xmax,ymax,textclip

                     xzoom = FIX(xsize / zoomscale * FLOAT(bxw)/2.0)
                     yzoom = FIX(ysize / zoomscale * FLOAT(bxw)/2.0)

                     cenX = ((xpos1+xpos2)/2 > (dw[0]+xzoom)) < (dw[2]-1-xzoom)
                     cenY = ((ypos1+ypos2)/2 > (dw[1]+yzoom)) < (dw[3]-1-yzoom)

                     dw = LONG([(cenX-xzoom+1),(cenY-yzoom+1),(cenX+xzoom),(cenY+yzoom)])
                   ENDELSE
                 ENDELSE

                 normal = [FLOAT(dw[2]-dw[0]+1)/xsep,FLOAT(dw[3]-dw[1]+1)/ysep]

                 tvdisp = SSQRT(tvbase[*,dw[0]-buffer2:dw[2]-buffer2,dw[1]-buffer2:dw[3]-buffer2])
                 IF tvmode GT 0 THEN BEGIN
                   IF mode LE 1 THEN goodmask = incmask OR (ellmask AND (NOT mask)) $
                                ELSE goodmask = (Omask GT 0)
                   IF tvmode EQ 2 THEN goodmask = 1b-goodmask

                   FOR ii = 0,2 DO BEGIN
; All masked pixels are replaced by -1000.0, since we cap the TV
; arrays at 999.
                     tvdisp[ii,*,*] = (tvdisp[ii,*,*]-SSQRT(plotsig[0])) * (fade[sigmode]*FLOAT(goodmask[dw[0]:dw[2],dw[1]:dw[3]])+(1.0-fade[sigmode])) + SSQRT(plotsig[0])
                   ENDFOR
                 ENDIF

; Adjust the availability of the "fine" highlight if you change zoom.
                 IF zoomscale GT 2.0 THEN dmodelist = [0,2] $
                                     ELSE dmodelist = [0,1]
                 IF zoomscale GT 4.0 THEN dmodelist = [0,3]
                 IF dmode GT N_ELEMENTS(dmodelist) THEN dmode=N_ELEMENTS(dmodelist)

                 proc4_plot,tvdisp,mainclip,plotsig,dmode,Rid,Timg,dw, $
                            masklist,Omask,Oimg,mode,dotclr,dmodelist,dspace,$
                            dsize,zoomscale,dsym,normal,n_ellipses, $
                            Dx,Dy,Px,Py,theta,a,b,a_f,b_f,ellcolor
               END
            5: BEGIN ; Clicked "Scale"
                 sigmode = (sigmode + 1) MOD n_sig_modes
                 IF sigmode EQ (n_sig_modes-1) THEN BEGIN
                   temp = ""
                   temp2 = ""

                   proc4_text,'Enter minimum plot sigmas (default is -2) in input window',!dgreen,charsz,xmax,ymax,textclip
                   READ,temp,FORMAT='(a)',PROMPT='Minimum: '

                   proc4_text,'Enter maximum plot sigmas (default is 10) in input window',!dgreen,charsz,xmax,ymax,textclip
                   READ,temp2,FORMAT='(a)',PROMPT='Maximum: '

                   IF STRLEN(temp) LT 1 THEN temp = -2.0 ELSE temp = FLOAT(temp)
                   IF STRLEN(temp2) LT 1 THEN temp2 = 10.0 ELSE temp2 = FLOAT(temp2)
                   IF temp2 GT temp THEN BEGIN
                     plotsig[0] = FLOAT(temp)
                     plotsig[1] = FLOAT(temp2)
                     proc4_text,'New scale is ['+STRTRIM(STRING(plotsig[0]),2)+','+STRTRIM(STRING(plotsig[1]),2)+']',!blue,charsz,xmax,ymax,textclip
                     proc4_plot,tvdisp,mainclip,plotsig,dmode,Rid,Timg,dw, $
                                masklist,Omask,Oimg,mode,dotclr,dmodelist,dspace,$
                                dsize,zoomscale,dsym,normal,n_ellipses, $
                                Dx,Dy,Px,Py,theta,a,b,a_f,b_f,ellcolor
                   ENDIF ELSE BEGIN
                     proc4_text,'ERROR: cannot have min scale less than max scale.  Scale is unchanged.',!red,charsz,xmax,ymax,textclip
                   ENDELSE
                 ENDIF ELSE BEGIN
                   plotsig = [sigmin[sigmode],sigmax[sigmode]]
                   proc4_text,'New scale is ['+STRTRIM(STRING(plotsig[0]),2)+','+STRTRIM(STRING(plotsig[1]),2)+']',!blue,charsz,xmax,ymax,textclip
                 ENDELSE

                 proc4_plot,tvdisp,mainclip,plotsig,dmode,Rid,Timg,dw, $
                            masklist,Omask,Oimg,mode,dotclr,dmodelist,dspace,$
                            dsize,zoomscale,dsym,normal,n_ellipses, $
                            Dx,Dy,Px,Py,theta,a,b,a_f,b_f,ellcolor
               END
            6: BEGIN ; Clicked "Highlight"
; Switch dmode to the next possibility
                 dmode = (dmode+1) MOD (1+N_ELEMENTS(dmodelist))
                 proc4_plot,tvdisp,mainclip,plotsig,dmode,Rid,Timg,dw, $
                            masklist,Omask,Oimg,mode,dotclr,dmodelist,dspace,$
                            dsize,zoomscale,dsym,normal,n_ellipses, $
                            Dx,Dy,Px,Py,theta,a,b,a_f,b_f,ellcolor

                 proc4_text,'Switched highlight mode; redraw complete, select objects as usual',!dgreen,charsz,xmax,ymax,textclip
               END
            7: BEGIN ; Clicked "TV mode"
; Crude version of good-pixel or bad-pixel maps (no grow applied)
; 0: full image
; 1: Only good pixels
; 2: Only bad pixels
                 tvmode = (tvmode + 1) MOD 3
                 tvdisp = SSQRT(tvbase[*,dw[0]-buffer2:dw[2]-buffer2,dw[1]-buffer2:dw[3]-buffer2])
                 IF tvmode GT 0 THEN BEGIN
                   IF mode LE 1 THEN goodmask = incmask OR (ellmask AND (NOT mask)) $
                                ELSE goodmask = (Omask GT 0)
                   IF tvmode EQ 2 THEN goodmask = 1b-goodmask

                   FOR ii = 0,2 DO BEGIN
                     tvdisp[ii,*,*] = (tvdisp[ii,*,*]-SSQRT(plotsig[0])) * (fade[sigmode]*FLOAT(goodmask[dw[0]:dw[2],dw[1]:dw[3]])+(1.0-fade[sigmode])) + SSQRT(plotsig[0])
                   ENDFOR
                 ENDIF

                 proc4_plot,tvdisp,mainclip,plotsig,dmode,Rid,Timg,dw, $
                            masklist,Omask,Oimg,mode,dotclr,dmodelist,dspace,$
                            dsize,zoomscale,dsym,normal,n_ellipses, $
                            Dx,Dy,Px,Py,theta,a,b,a_f,b_f,ellcolor

                 proc4_text,'Switched TV mode; redraw complete, select objects as usual',!dgreen,charsz,xmax,ymax,textclip
               END
            8: BEGIN ; Clicked "Reset" or "Abort"
; Since there's nothing else to do after this point, might as well leave.
                 CASE mode OF
                 0: BEGIN
                   proc4_text,'Aborting program.  Have a nice day!',!red,charsz,xmax,ymax,textclip
                   RETURN
                    END
                 1: BEGIN
                   IF ellflag THEN BEGIN
                     proc4_text,'Restoring original ellipse definitions.',!black,charsz,xmax,ymax,textclip
                     n_ellipses = n_ellipses_old
                     ellmask = ellmask_old
                     zcheck = zcheck_old
                     IF n_ellipses EQ max_ellipses THEN BEGIN
                       a = a_old
                       b = b_old
                       a_f = a_f_old
                       b_f = b_f_old
                       z_s = z_s_old
                       z_f = z_f_old
                       z_c = z_c_old
                       Px = Px_old
                       Py = Py_old
                       Dx = Dx_old
                       Dy = Dy_old
                       theta = theta_old
                       pa = pa_old
                       zmax = zmax_old
                     ENDIF ELSE BEGIN
                       a = [a_old,FLTARR(max_ellipses-n_ellipses)]
                       b = [b_old,FLTARR(max_ellipses-n_ellipses)]
                       a_f = [a_f_old,FLTARR(max_ellipses-n_ellipses)]
                       b_f = [b_f_old,FLTARR(max_ellipses-n_ellipses)]
                       z_s = [z_s_old,FLTARR(max_ellipses-n_ellipses)]
                       z_f = [z_f_old,FLTARR(max_ellipses-n_ellipses)]
                       z_c = [z_c_old,FLTARR(max_ellipses-n_ellipses)]
                       Px = [Px_old,FLTARR(max_ellipses-n_ellipses)]
                       Py = [Py_old,FLTARR(max_ellipses-n_ellipses)]
                       Dx = [Dx_old,FLTARR(max_ellipses-n_ellipses)]
                       Dy = [Dy_old,FLTARR(max_ellipses-n_ellipses)]
                       theta = [theta_old,FLTARR(max_ellipses-n_ellipses)]
                       pa = [pa_old,FLTARR(max_ellipses-n_ellipses)]
                       zmax = [zmax_old,FLTARR(max_ellipses-n_ellipses)]
                     ENDELSE
                     ellmod = 0b

; Overlay the dotted mask on the image.
                     IF dmode GT 0 THEN proc4_dot,Rid,Timg,dw, $
                            masklist,Omask,Oimg,mode,dotclr,ellcolor, $
                            dspace[dmodelist[dmode-1]], $
                            dsize[dmodelist[dmode-1]]*zoomscale, $
                            dsym[dmodelist[dmode-1]],normal,mainclip
                     proc4_text,'Resetting SB curve, size='+STRTRIM(STRING(sb_box),2),!black,charsz,xmax,ymax,textclip
                     bin_num = bin_num_old
                     bin_Rtot = bin_Rtot_old
                     bin_Ntot = bin_Ntot_old

; Since we already know what the curves look like, just redraw them.
                     proc4_sbplot,binsize,bin_num,bin_Rtot,bin_Ntot,ellcolor, $
                                  Rclip,Nclip,zoomnormal,charsz*2.0, $
                                  a,zmax,Rscale,Nscale,Rsky2,Nsky2

                     proc4_plot,tvdisp,mainclip,plotsig,dmode,Rid,Timg,dw, $
                                masklist,Omask,Oimg,mode,dotclr,dmodelist,dspace,$
                                dsize,zoomscale,dsym,normal,n_ellipses, $
                                Dx,Dy,Px,Py,theta,a,b,a_f,b_f,ellcolor
                     proc4_text,'Ellipse definitions reset.  Select region of ellipse #'+STRTRIM(STRING(n_ellipses+1),2),!black,charsz,xmax,ymax,textclip
                   ENDIF ELSE BEGIN
                     proc4_text,'No original ellipse definitions exist, clearing arrays.  Select region of ellipse #1:',!dgreen,charsz,xmax,ymax,textclip
                     ellmod = 1b
                     n_ellipses = 0
                     ellmask = BYTARR(sz[1],sz[2])
                     zcheck = FLTARR(max_ellipses,sz[1],sz[2])+999.0
                     Px = FLTARR(max_ellipses)
                     Py = FLTARR(max_ellipses)
                     Dx = FLTARR(max_ellipses)
                     Dy = FLTARR(max_ellipses)
                     a = FLTARR(max_ellipses)
                     b = FLTARR(max_ellipses)
                     pa = FLTARR(max_ellipses)
                     theta = FLTARR(max_ellipses)
                     z_s = FLTARR(max_ellipses)+1.0
                     z_f = FLTARR(max_ellipses)+1.0
                     z_c = FLTARR(max_ellipses)+1.0
                     zmax = FLTARR(max_ellipses)+1.5
                   ENDELSE
                    END
                 2: BEGIN
                   proc4_text,'Resetting override mask',!black,charsz,xmax,ymax,textclip
                   Omask = INTARR(sz[1],sz[2])
                   Oimg = INTARR(sz[1],sz[2])
                   num_O_custom = 0
                   proc4_plot,tvdisp,mainclip,plotsig,dmode,Rid,Timg,dw, $
                              masklist,Omask,Oimg,mode,dotclr,dmodelist,dspace,$
                              dsize,zoomscale,dsym,normal,n_ellipses, $
                              Dx,Dy,Px,Py,theta,a,b,a_f,b_f,ellcolor
                    END
                 ENDCASE
               END
            ELSE: BEGIN
                    PRINT,"ERROR in proc4: invalid button number ",bnum
                    RETURN
                  END
          ENDCASE
          ENDIF ELSE BEGIN
; You clicked the text window.  Do nothing.
            proc4_text,'STOP CLICKING THAT!!!',!red,charsz,xmax,ymax,textclip
          ENDELSE
        ENDELSE
      ENDELSE
    ENDWHILE
    
; We're done with everything user-adjustable.  Only write the new Timg
; here, to include any user-created regions.
    fits_write,odir+RTfile,Timg,Nhd

    IF n_ellipses EQ 0 THEN BEGIN
      PRINT,"ERROR in proc4: no ellipses have been specified, cannot proceed."
      RETURN
    ENDIF

    IF ellmod THEN BEGIN
      PRINT,"Writing new values to ellipse file ",ellipse
      ztemp = FLTARR(n_ellipses)+1.0
      write_ellipse_file,odir+ellipse,n_ellipses,refimage,Dx,Dy, $
                         Px,Py,buffer,pa,a,b,ztemp,ztemp,ztemp
    ENDIF

  ENDIF

; Mask array is now set.  Translate to mask images.
;------------------------------------
;| STEP 7: Create segmentation mask |
;------------------------------------

  IF MAX(Omask) GT 0 THEN BEGIN
    fits_write,odir+overfile,Omask,Nhd
  ENDIF

  PRINT,"Creating segmentation mask file"
; Set up segmentation mask, showing WHY each object was rejected
  segval = 1*FIX(useradj) + $
           2*FIX(edgecheck) + $
           4*FIX(classcheck2) + $
           8*FIX(sizecheck) + $
           16*FIX(classcheck1) + $
           32*FIX(flagcheck) + $
           64*FIX(starcntcheck) + $
           128*FIX(satcheck) + $
           256*FIX(magcheck) + $
           512*FIX(Sstarcheck) + $
           1024*FIX(starareacheck) + $
           2048*FIX(galcntcheck) + $
           4096*FIX(galareacheck)

  proc4_write,masklist,Timg,Rid,initmask,odir,grow,mask, $
              segval,segfile,listfile,maskfile,Imaskfile,Smaskfile,ellipse, $
              Nhd,incmask,plmask,ecntrat,Rsexfile,Ssexfile

; Update our QA plots with new values.
  PRINT,"Creating QA plots"
  qa_plot,Rfile,Nfile,Sfile,RTfile,maskfile,Smaskfile,Imaskfile,(Omask GT 0), $
          (Dx[0:n_ellipses-1]-buffer),(Dy[0:n_ellipses-1]-buffer), $
          pa[0:n_ellipses-1],a_f[0:n_ellipses-1],b_f[0:n_ellipses-1], $
          indir=idir,outdir=odir,sky_rad=(a[0:n_ellipses-1]*z_s[0:n_ellipses-1])

  IF KEYWORD_SET(output) THEN BEGIN
; We want to keep the outputs, so update them, and fill in the
; "masked" pixels
    FOR xx = 0,sz[1]-1 DO BEGIN
      FOR yy = 0,sz[2]-1 DO BEGIN
        IF Timg[xx,yy] GT 0 THEN BEGIN
          k = WHERE(Rid EQ Timg[xx,yy],nk)
          IF nk GT 0 THEN BEGIN
            Rimg[xx,yy] = Rback[k[0]]
            Nimg[xx,yy] = Nback[k[0]]
            Simg[xx,yy] = Sback[k[0]]
          ENDIF
        ENDIF ELSE BEGIN
          IF Timg[xx,yy] LT 0 THEN BEGIN
            Rimg[xx,yy] = Rsky2
            Nimg[xx,yy] = Nsky2
            Simg[xx,yy] = Ssky2
          ENDIF
        ENDELSE
      ENDFOR
    ENDFOR
    fits_write,odir+Rsexfile,Rimg,Rhd
    fits_write,odir+Nsexfile,Nimg,Nhd
    fits_write,odir+Ssexfile,(Simg*Nexptime),Shd
  ENDIF ELSE BEGIN
; We DON'T want to keep the outputs, so remove what's already there.
    spawn,"/bin/rm -f "+odir+Rsexfile
    spawn,"/bin/rm -f "+odir+Nsexfile
    spawn,"/bin/rm -f "+odir+Ssexfile
  ENDELSE

  RETURN

END
