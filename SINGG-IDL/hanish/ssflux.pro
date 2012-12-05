PRO ssflux,RFILE=Rfile,NFILE=Nfile,SFILE=Sfile,ELLIPSE=ellipse, $
           INDIR=indir,OUTDIR=outdir,RUNID=runid,OBJ=obj, $
           RMASKFILE=Rmaskfile,NMASKFILE=Nmaskfile,SMASKFILE=Smaskfile, $
           NOEXTRA=noextra,IMASKFILE=Imaskfile,BW=bw,TV=tv, $
           FAST=fast,NOSNR=nosnr,OVERRIDE=override,UNITS=units
; Like the other scripts, this is run once per directory, and processes all
; three input files at once.
; OPTIONAL INPUTS
; Rfile         Name of combined R image  (if not set, J???????_R_ss.fits)
; Nfile         Name of narrow-band image (if not set, J???????_6???_ss.fits)
; Sfile         Name of subtracted image  (if not set, J???????_Rsub_ss.fits)
;                 The only time these should be different is when multiple 
;                 filters are used on the same object, in which case this
;                 script must be run multiple times, which usually requires
;                 multiple lines in the catalog file.  While Rfile and Nfile
;                 can stay the same for the different filters, the Sfile needs
;                 clarification.
; indir         Input directory (default is the current one)
; outdir        Output directory (default is the current one)
; ellipse       Name of the ellipse parameter file (located in indir)
; Rmaskfile     Optional mask files to remove stars from calculations.
; Nmaskfile     
; Smaskfile      
; /noextra      Don't auto-detect HII regions outside the apertures
; Imaskfile     Inclusion mask file
; /bw           Plot in black and white
; /tv           Use TV to show the final results
; /fast         Non-interactive, just skip the step where the user
;               selects the flux radius, and keep z_f = MIN(z_c,z_s).
; /nosnr        Skips z_c
; override      If set, uses the specified override file instead of
;                 the default one for FLUX_O determination.
; /units        If the physical units weren't set by singg_fluxcal, abort.

  bwflag = KEYWORD_SET(bw)
  extraflag = NOT KEYWORD_SET(noextra)
  fastflag = KEYWORD_SET(fast)

  spawn,"pwd",cdir

; An easier way to specify directories:
  IF KEYWORD_SET(obj) AND KEYWORD_SET(runid) THEN BEGIN
    indir = STRTRIM(runid,2)+'/Proc4/'+STRTRIM(obj,2)+'/'
    outdir = STRTRIM(runid,2)+'/Proc4/'+STRTRIM(obj,2)+'/'
  ENDIF

; indir = Run*/Proc4/Jwhatever
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

; STEP 1: Make sure the input files actually exist.
  IF KEYWORD_SET(Rfile) THEN BEGIN $
    IF NOT FILE_TEST(idir+Rfile) THEN BEGIN
      PRINT,"ERROR in ssflux: R file not found. ",Rfile
      RETURN
    ENDIF
  ENDIF ELSE BEGIN
; Rfile wasn't specified, so we need to find it.
; Assume Rfile will be of the form *_R.fits or *_C.fits.
    spawn,"ls "+idir+"*_?_ss.fits",Rlist

    IF N_ELEMENTS(Rlist) GT 1 THEN BEGIN
      PRINT,"ERROR in ssflux: Multiple combined continuum images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the RFILE keyword explicitly."
      RETURN
    ENDIF

    IF NOT FILE_TEST(Rlist[0]) THEN BEGIN
      PRINT,"ERROR in ssflux: No combined continuum images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the RFILE keyword explicitly."
      RETURN
    ENDIF

    Rfile = STRMID(Rlist[0],STRLEN(idir),STRLEN(Rlist[0])-STRLEN(idir))
    PRINT,"R image used: ",Rfile
  ENDELSE

; For narrow-band, it's a bit more complex since there could be multiple
; narrow-band filters used for this object.
  IF KEYWORD_SET(Nfile) THEN BEGIN $
    IF NOT FILE_TEST(idir+Nfile) THEN BEGIN
      PRINT,"ERROR in ssflux: Narrow-band file not found. ",Nfile
      RETURN
    ENDIF
  ENDIF ELSE BEGIN
; Nfile wasn't specified, so we need to find it.
; Assume Nfile will be of the form *_6XXX.fits.
    spawn,"ls "+idir+"*_6???_ss.fits",Nlist

    IF N_ELEMENTS(Nlist) GT 1 THEN BEGIN
      PRINT,"ERROR in ssflux: Multiple combined narrow-band images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the NFILE keyword explicitly."
      RETURN
    ENDIF

    IF NOT FILE_TEST(Nlist[0]) THEN BEGIN
      PRINT,"ERROR in ssflux: No combined narrow-band images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the NFILE keyword explicitly."
      RETURN
    ENDIF

    Nfile = STRMID(Nlist[0],STRLEN(idir),STRLEN(Nlist[0])-STRLEN(idir))
    PRINT,"Narrow-band image used: ",Nfile
  ENDELSE

  IF KEYWORD_SET(Sfile) THEN BEGIN $
    IF NOT FILE_TEST(idir+Sfile) THEN BEGIN
      PRINT,"ERROR in ssflux: Subtracted file not found. ",Sfile
      RETURN
    ENDIF
  ENDIF ELSE BEGIN
; Sfile wasn't specified, so we need to find it.
; Assume Sfile will be of the form *_Rsub.fits or *_Csub.fits.
    spawn,"ls "+idir+"*_?sub_ss.fits",Slist

    IF N_ELEMENTS(Slist) GT 1 THEN BEGIN
      PRINT,"ERROR in ssflux: Multiple combined subtracted images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the SFILE keyword explicitly."
      RETURN
    ENDIF

    IF NOT FILE_TEST(Slist[0]) THEN BEGIN
      PRINT,"ERROR in ssflux: No combined subtracted images"
      PRINT,"  were found in the directory.  Please run again, specifying"
      PRINT,"  the SFILE keyword explicitly."
      RETURN
    ENDIF

    Sfile = STRMID(Slist[0],STRLEN(idir),STRLEN(Slist[0])-STRLEN(idir))
    PRINT,"Subtracted image used: ",Sfile
  ENDELSE

  IF NOT KEYWORD_SET(ellipse) THEN $
    ellipse=STRTRIM(STRMID(Rfile,0,STRLEN(Rfile)-10),2)+"_ellipse.dat"
  IF NOT FILE_TEST(idir+ellipse) THEN BEGIN
    PRINT,"ERROR in ssflux: Ellipse file not found. ",ellipse
    RETURN
  ENDIF

  read_ellipse_file,idir+ellipse,n_ellipses,refimage,Dx,Dy,Px,Py,pa, $
                    a_i,b_i,z_s,z_f,z_c

  IF N_ELEMENTS(a_i) LT 1 THEN BEGIN
    PRINT,"Invalid ellipse file ",ellipse
    RETURN
  ENDIF

  a = a_i * z_s
  b = b_i * z_s
  a_s = a_i * z_s

; STEP 2: Read the input images and their header information.
  fits_read, idir+Rfile, Rimg, Rhd
  fits_read, idir+Nfile, Nimg, Nhd
  fits_read, idir+Sfile, Simg, Shd

  buffer = SXPAR(Rhd,'BUFFER',count=matches)
  IF matches LT 1 THEN buffer = 0
  buffN = SXPAR(Nhd,'BUFFER',count=matchN)
  IF matchN NE 0 AND buffN NE buffer THEN BEGIN
    PRINT,"ERROR in ssflux; can't mix different edge buffer sizes",$
          buffer,buffN
    RETURN
  ENDIF
  
; The only modification to the files themselves is that we need to set
; the DELSUB flag to indicate that we're doing the DELSKY correction
; in this routine, now.
  SXADDPAR,Rhd,'DELSUB','Y',' Secondary sky model correction complete?'
  SXADDPAR,Nhd,'DELSUB','Y',' Secondary sky model correction complete?'
  SXADDPAR,Shd,'DELSUB','Y',' Secondary sky model correction complete?'
  fits_write, idir+Rfile, Rimg, Rhd
  fits_write, idir+Nfile, Nimg, Nhd
  fits_write, idir+Sfile, Simg, Shd


  Px = Px + buffer
  Py = Py + buffer
  Dx = Dx + buffer
  Dy = Dy + buffer

  goodval = 0b
  badval  = 1b
  dummy = FLTARR(3)

  phot = SXPAR(Rhd,'PHOTFLAM',count=Rmatch)
  IF Rmatch GT 0 THEN BEGIN
    Rscale = phot[0]
    Runits = "erg/cm^2/Angstrom/sec"
  ENDIF ELSE BEGIN
    Rscale = 1.0
    Runits = "DN/sec"
    IF KEYWORD_SET(units) THEN BEGIN
      PRINT,'ERROR in ssflux: units not set ',Rfile
      RETURN
    ENDIF
  ENDELSE

  phot = SXPAR(Nhd,'PHOTFLAM',count=Nmatch)
  IF Nmatch GT 0 THEN BEGIN
    Nscale = phot[0]
    Nunits = "erg/cm^2/Angstrom/sec"
  ENDIF ELSE BEGIN
    Nscale = 1.0
    Nunits = "DN/sec"
    IF KEYWORD_SET(units) THEN BEGIN
      PRINT,'ERROR in ssflux: units not set ',Nfile
      RETURN
    ENDIF
  ENDELSE

; Note that while the R and narrow-band images use PHOTFLAM, the Rsub uses
; PHOTFLUX for the unit response variable.  The difference is in the units;
; PHOTFLUX isn't per Angstrom
  phot = SXPAR(Shd,'PHOTFLUX',count=Smatch)
  IF Smatch GT 0 THEN BEGIN
    Sscale = phot[0]
    Sunits = "erg/cm^2/sec"
  ENDIF ELSE BEGIN
    Sscale = 1.0
    Sunits = "DN/sec"
    IF KEYWORD_SET(units) THEN BEGIN
      PRINT,'ERROR in ssflux: units not set ',Sfile
      RETURN
    ENDIF
  ENDELSE

;  spawn,"ls "+idir+"*_?sub_ss.fits",Slist
;  multflag = N_ELEMENTS(Slist) GT 1
  multflag = (STRPOS(Sfile,'_6') GT 0)
  IF multflag THEN Rbase = STRMID(Rfile,0,STRLEN(Rfile)-8) $
              ELSE Rbase = STRMID(Rfile,0,STRLEN(Rfile)-10)
  IF NOT KEYWORD_SET(Rmaskfile) THEN Rmaskfile = Rbase+'_mask.fits'
  IF NOT FILE_TEST(idir+Rmaskfile) THEN BEGIN
    PRINT,"ERROR in ssflux: R-band mask file does not exist ",Rmaskfile
    RETURN
  ENDIF

  fits_read,idir+Rmaskfile,Rmask,mhd,/data_only

  IF NOT KEYWORD_SET(Nmaskfile) THEN Nmaskfile = Rmaskfile
  IF NOT FILE_TEST(idir+Nmaskfile) THEN BEGIN
    PRINT,"ERROR in ssflux: narrow-band mask file does not exist ",Nmaskfile
    RETURN
  ENDIF

  fits_read,idir+Nmaskfile,Nmask,mhd,/data_only

  IF NOT KEYWORD_SET(Smaskfile) THEN $
       Smaskfile = STRMID(Sfile,0,STRLEN(Sfile)-8)+'_mask.fits'
  IF NOT FILE_TEST(idir+Smaskfile) THEN BEGIN
    PRINT,"ERROR in ssflux: subtracted mask file does not exist ",Smaskfile
    RETURN
  ENDIF

  fits_read,idir+Smaskfile,Smask,mhd,/data_only

  IF extraflag THEN BEGIN
    IF NOT KEYWORD_SET(Imaskfile) THEN Imaskfile = Rbase+'_inc_mask.fits'
    IF NOT FILE_TEST(idir+Imaskfile) THEN BEGIN
      PRINT,"ERROR in ssflux: Inclusion mask file does not exist ",Imaskfile
      RETURN
    ENDIF
  ENDIF

  IF NOT KEYWORD_SET(override) THEN override = STRMID(Sfile,0,STRLEN(Sfile)-8)+'_override.fits'
  overflag = FILE_TEST(idir+override)

  getrot,Shd,rot,cdelt 
  cdelt  = abs(cdelt)*3600.
  as_pix = cdelt[0]

  bxw = 35

  PRINT,"Scale (arcsec/pixel) = ",as_pix

; STEP 2: Convert to units more convenient for the processing
; diamaj and diamin are in arcseconds
  diamaj = a * as_pix
  diamin = b * as_pix
  ratio = a / b

; Theta is in radians, measured counterclockwise from Left, not Up like PA
  theta = (pa-90.0)*!dtor
; We also want to convert PA to a WCS-based system (measured from North)
  del_pa_x = ATAN(SXPAR(Shd,"CD1_2")/SXPAR(Shd,"CD1_1"))*!radeg
  del_pa_y = ATAN(-SXPAR(Shd,"CD2_1")/SXPAR(Shd,"CD2_2"))*!radeg
  del_pa = (del_pa_x + del_pa_y)/2

  zmin = 0.0
  zmax = 1.2

  sz = SIZE(Simg)
  xyad,Rhd,Dx,Dy,racen,deccen

; The Rsub image has the worst seeing; use that to determine bins.
  seeing = SXPAR(Shd,"SEEING")
; Calculate bin size; originally this was just hard-coded to a tiny number,
; now it corresponds to the seeing along the semimajor axis, with the tiny
; number used as a minimum cap.
  zbinsize = seeing/MAX(diamaj) > 0.001
  PRINT,"Z increment = ",zbinsize

; At z<1.0, we just use a constant zbinsize
  nlow = LONG((1.0-zmin)/zbinsize) > 0

; At z>1.0, the zbinsize increases by around 5% after each bin.
; Math note: SUM(A^x) from 0 to N = (A^(N+1) - 1.0)/(A-1)
  mult = 1.05 ; Expansion ratio for z>1
  nhigh = LONG(ALOG((MAX([(zmax-1.0),0.0])/zbinsize)*(mult-1.0) + 1.0)/ALOG(mult))

; binZ is Z for a given bin, which is already multiplied by Z'
  num_bins = (nlow + nhigh + 2)
  binZ = FLTARR(num_bins)
  FOR ii = 0,nlow-1 DO binZ[ii] = zmin + ii*zbinsize
  FOR ii = 0,nhigh+1 DO binZ[nlow+ii] = 1.0 + (mult^ii-1.0)*zbinsize/(mult-1.0)

  cntrat = SXPAR(Shd,'CNTRAT1')
  ecntrat = SXPAR(Shd,"ECNTRAT1")
  IF ecntrat LT 0.000001 THEN ecntrat = SXPAR(Shd,"ECNTRAT2")*cntrat/SXPAR(Shd,'CNTRAT2')
  IF ecntrat GT 0.1*cntrat THEN ecntrat = 0.1*cntrat

; If cntrat1 and cntrat2 aren't very similar, we want to make sure
; that the FRACTIONAL error in cntrat1 gets transferred.

; Check to make sure the images are actually sky-subtracted!
  IF NOT SXPAR(Rhd,"SKYSUB",'R-band image missing SKYSUB') THEN $
        PRINT,"ERROR in ssflux: R-band image has not been sky-subtracted"
  IF NOT SXPAR(Nhd,"SKYSUB",'Narrow-band image missing SKYSUB') THEN $
        PRINT,"ERROR in ssflux: Narrow-band image has not been sky-subtracted"
  IF NOT SXPAR(Shd,"SKYSUB",'Subtracted image missing SKYSUB') THEN $
        PRINT,"ERROR in ssflux: Subtracted image has not been sky-subtracted"

  IF n_ellipses GT 1 THEN BEGIN
; There's more than one galaxy to work around, so we have a little setup to do.
; Set up the mask to calculate sky near each galaxy.  We COULD do one mask for
; each ellipse, but there's a chance another galaxy would fall into that
; area, so we'll do this the hard way.
    
    AreaZ = 250.0*bxw^2/(!pi*a*b) > 2.0

    dr = (SQRT((diamaj+diamin)^2.0 + $
         4.0*AreaZ*diamaj*diamin)-diamaj-diamin)/2.0

; First, get the BIG mask, the one that includes all of the objects.
    tempmask = mask_ellipse_ann3(Rimg,Rhd,racen,deccen, $
                                 diamaj/30.0,diamin/30.0, $
                                 dr/60.0,pa,goodmask=1b,/amflag)

    mysky,Rimg,Rimsky,Rimsig,mask=(tempmask OR Rmask),/silent
    mysky,Nimg,Nimsky,Nimsig,mask=(tempmask OR Nmask),/silent
    mysky,Simg,Simsky,Simsig,mask=(tempmask OR Smask),/silent

    PRINT,"Boxing the R image"
    Rtemp=box_sky(Rimg,(tempmask OR Rmask),[Rimsky,Rimsig],bxw,Rboxdata,REJFRAC=rejfrac)

    PRINT,"Boxing the Narrow-band image"
    Ntemp=box_sky(Nimg,(tempmask OR Nmask),[Nimsky,Nimsig],bxw,Nboxdata,REJFRAC=rejfrac)

    PRINT,"Boxing the Subtracted image"
    Stemp=box_sky(Simg,(tempmask OR Smask),[Simsky,Simsig],bxw,Sboxdata,REJFRAC=rejfrac)

; Rtemp, Ntemp, and Stemp are the average box sky levels for the entire image,
; not around any one galaxy.  This isn't useful to us, we only want boxdata..
  ENDIF

  skymult = (0.233 + (21.6/(as_pix*SQRT(a*b)))^3.0 ) < 2.0
  Rskybase = SXPAR(Rhd,"SKYLEV")
  Nskybase = SXPAR(Nhd,"SKYLEV")
  Sskybase = SXPAR(Shd,"SKYLEV")
  Rsigbx = SXPAR(Rhd,"SKYSIGBX") * skymult[0]
  Nsigbx = SXPAR(Nhd,"SKYSIGBX") * skymult[0]
  Ssigbx = SXPAR(Shd,"SKYSIGBX") * skymult[0]
  Rsigpx = SXPAR(Rhd,"SKYSIG")
  Nsigpx = SXPAR(Nhd,"SKYSIG")
  Ssigpx = SXPAR(Shd,"SKYSIG")
  Rdelsky = SXPAR(Rhd,'DELSKY')
  Ndelsky = SXPAR(Nhd,'DELSKY')
  Sdelsky = SXPAR(Shd,'DELSKY')
  Rsky = FLTARR(3,n_ellipses)
  Nsky = FLTARR(3,n_ellipses)
  Ssky = FLTARR(3,n_ellipses)

  FOR ii = 0,(n_ellipses - 1) DO BEGIN
    PRINT,"Semi-major axis (in pixels) = ",a[ii]
    PRINT,"            (in arcseconds) = ",diamaj[ii]
    PRINT,"Semi-minor axis (in pixels) = ",b[ii]
    PRINT,"            (in arcseconds) = ",diamin[ii]
    PRINT,"Axial ratio = ",ratio[ii]

    IF ii EQ 0 THEN BEGIN
; It's the primary galaxy, so the sky determination has already been done
; over in sky_calibration, we can just extract the necessary info from the
; headers.
      Rsky[*,ii]=[Rskybase+Rdelsky,Rsigbx,Rsigpx]
      Nsky[*,ii]=[Nskybase+Ndelsky,Nsigbx,Nsigpx]
      Ssky[*,ii]=[Sskybase+Sdelsky,Ssigbx,Ssigpx]
    ENDIF ELSE BEGIN
; Since we're on the later galaxies, extract the correct stuff from the boxxed
; data we made earlier.

; First, make a smaller mask that only gives the annulus around one galaxy.
      tempmask = mask_ellipse_ann3(Rimg,Rhd,racen[ii],deccen[ii], $
                                   diamaj[ii]/30.0,diamin[ii]/30.0, $
                                   dr[ii]/60.0,pa[ii],goodmask=1b,/amflag)

      mysky,Rimg,Rimsky,Rimsig,mask=(tempmask OR Rmask),/silent
      mysky,Nimg,Nimsky,Nimsig,mask=(tempmask OR Nmask),/silent
      mysky,Simg,Simsky,Simsig,mask=(tempmask OR Smask),/silent

      Rsky[2,ii] = Rimsig
      Nsky[2,ii] = Nimsig
      Ssky[2,ii] = Simsig

      skyres = calc_box_sky(Rboxdata,(tempmask OR Rmask),bxw,Dx[ii],Dy[ii], $
                            theta[ii],a[ii],b[ii])
      Rsky[0,ii] = skyres[0] + Rskybase + Rdelsky
      Rsky[1,ii] = skyres[1] * skymult[ii]

      skyres = calc_box_sky(Nboxdata,(tempmask OR Nmask),bxw,Dx[ii],Dy[ii], $
                            theta[ii],a[ii],b[ii])
      Nsky[0,ii] = skyres[0] + Nskybase + Ndelsky
      Nsky[1,ii] = skyres[1] * skymult[ii]

      skyres = calc_box_sky(Sboxdata,(tempmask OR Smask),bxw,Dx[ii],Dy[ii], $
                            theta[ii],a[ii],b[ii])
      Ssky[0,ii] = skyres[0] + Sskybase + Sdelsky
      Ssky[1,ii] = skyres[1] * skymult[ii]
    ENDELSE
  ENDFOR

; Find the curves of growth.  This is done all in one shot.
  PRINT,"Calculating isophote curves"
  PRINT,"  R-band"
  calc_growth,Rimg,Rmask,binZ,Dx,Dy,(Rsky[0,*]-Rskybase),theta,a,b, $
              RINgood,RINbad,RFI,RFgI,RSBI,RSBsigI
  PRINT,"  Narrow-band"
  calc_growth,Nimg,Nmask,binZ,Dx,Dy,(Nsky[0,*]-Nskybase),theta,a,b, $
              NINgood,NINbad,NFI,NFgI,NSBI,NSBsigI
  PRINT,"  Continuum-subtracted"
  calc_growth,Simg,Smask,binZ,Dx,Dy,(Ssky[0,*]-Sskybase),theta,a,b, $
              SINgood,SINbad,SFI,SFgI,SSBI,SSBsigI

  PRINT,"Calculating brightness peak curves"
  PRINT,"  R-band"
  calc_growth,Rimg,Rmask,binZ,Px,Py,(Rsky[0,*]-Rskybase),theta,a,b, $
              RBNgood,RBNbad,RFB,RFgB,RSBB,RSBsigB
  PRINT,"  Narrow-band"
  calc_growth,Nimg,Nmask,binZ,Px,Py,(Nsky[0,*]-Nskybase),theta,a,b, $
              NBNgood,NBNbad,NFB,NFgB,NSBB,NSBsigB
  PRINT,"  Continuum-subtracted"
  calc_growth,Simg,Smask,binZ,Px,Py,(Ssky[0,*]-Sskybase),theta,a,b, $
              SBNgood,SBNbad,SFB,SFgB,SSBB,SSBsigB

  DEVICE, RETAIN=2, DECOMPOSED=0
  set_plot,'X'
  setplotcolors
  setbgfg,!white,!black
  WINDOW,XSIZE=1000,YSIZE=1000
  !P.MULTI=[0,0,4,0,0]

  a_c = FLTARR(n_ellipses)
  a_f = FLTARR(n_ellipses)

  FOR ii = 0,(n_ellipses - 1) DO BEGIN
; binA is in "pixels along the semimajor axis"
    binA = binZ * a[ii]

    IF KEYWORD_SET(nosnr) THEN BEGIN
      z_c[ii] = z_s[ii]
    ENDIF ELSE BEGIN
      skynoise = (Ssky[1,ii]/skymult[ii])*!pi*a[ii]*b[ii]*binZ^2
      z_c[ii] = findzero(((SFI[*,ii]/skynoise)-3.0),binZ,thresh=0.1,/noext,/last)
    ENDELSE

; Our initial endpoint will be where the SNR drops below 3.0, but not
; further out than z=1 (since that's where we set the sky).
    IF z_c[ii] GT 0.0 THEN BEGIN
      z_c[ii] = z_c[ii]*z_s[ii]
      a_c[ii] = z_c[ii]*a_i[ii]
      a_f[ii] = MIN([a_s[ii],a_c[ii]])
    ENDIF ELSE BEGIN
      a_c[ii] = -999.0
      a_f[ii] = a_s[ii]
    ENDELSE

    plot_fluxcurve,binA*as_pix,Rsky[1,ii]/as_pix^2,ratio[ii],bwflag,as_pix, $
                   RFI[*,ii],RFB[*,ii],dummy,dummy, $
                   Runits,Rscale,a_f[ii]*as_pix,a_s[ii]*as_pix,Rfile
    plot_fluxcurve,binA*as_pix,Nsky[1,ii]/as_pix^2,ratio[ii],bwflag,as_pix, $
                   NFI[*,ii],NFB[*,ii],dummy,dummy, $
                   Nunits,Nscale,a_f[ii]*as_pix,a_s[ii]*as_pix,Nfile
    plot_fluxcurve,binA*as_pix,Ssky[1,ii]/as_pix^2,ratio[ii],bwflag,as_pix, $
                   SFI[*,ii],SFB[*,ii],dummy,dummy, $
                   Sunits,Sscale,a_f[ii]*as_pix,a_s[ii]*as_pix,Sfile, $
                   CNTSIGI=(RFI[*,ii]*ecntrat), $
                   CNTSIGB=(RFB[*,ii]*ecntrat)
    plot_fluxcurve,binA*as_pix,Ssky[1,ii]/as_pix^2,ratio[ii],bwflag,as_pix, $
                   SSBI[*,ii],SSBB[*,ii],dummy,dummy, $
                   Sunits,Sscale/(as_pix)^2, $
                   a_f[ii]*as_pix,a_s[ii]*as_pix,Sfile,/SB

; Ask where it levels off.  Find the flux at that Z in all three cases 
; (reg,Hi,Lo), then take F/2, find what Z that corresponds to, to get the
; half-light "radius" and its uncertainty

; Enter a_f, in pixels
    IF NOT fastflag THEN BEGIN
      PRINT,"Select the location of final flux (click outside the plot area to keep current value)"
      CURSOR,val,y,/DOWN
      IF (val GT 0 AND val LT binA[num_bins-1]) THEN a_f[ii]=val/as_pix
    ENDIF
  ENDFOR

  z_f = a_f / a_i
; Update the ellipse file with the latest values for z_f and z_c
  write_ellipse_file,odir+ellipse,n_ellipses,refimage,Dx,Dy,Px,Py,buffer,pa, $
                     a_i,b_i,z_s,z_f,z_c,/SILENT

; Update the sky uncertainties to our new model.

; At this point, we've set up our initial arrays and let the user
; select his values for a_f.  If needed, find the "external" flux,
; and start the data dumping.

  RFoutI = DBLARR(3,n_ellipses)
  RFoutB = DBLARR(3,n_ellipses)
  NFoutI = DBLARR(3,n_ellipses)
  NFoutB = DBLARR(3,n_ellipses)
  SFoutI = DBLARR(3,n_ellipses)
  SFoutB = DBLARR(3,n_ellipses)

  IF extraflag THEN BEGIN
    fits_read,idir+Imaskfile,Imask,junk,/data_only
    
    ZvalI = BYTARR(sz[1],sz[2]) + 10.0
    ZvalB = BYTARR(sz[1],sz[2]) + 10.0
    FOR ii = 0,n_ellipses-1 DO BEGIN
      dist_ellipse,tempmask,[sz[1],sz[2]],Dx[ii],Dy[ii],(a[ii]/b[ii]),pa[ii]
      ZvalI = ZvalI < (tempmask/a_f[ii])
      dist_ellipse,tempmask,[sz[1],sz[2]],Px[ii],Py[ii],(a[ii]/b[ii]),pa[ii]
      ZvalB = ZvalB < (tempmask/a_f[ii])
    ENDFOR

    ZmaskI = FLOAT((ZvalI GT 1.0) AND Imask)
    ZmaskB = FLOAT((ZvalB GT 1.0) AND Imask)

; Count up the values in these included pixels.
    RFoutI[0,0] = TOTAL(DOUBLE(Rimg * ZmaskI))
    RFoutI[1,0] = TOTAL(DOUBLE(Rsigbx * ZmaskI))
    NFoutI[0,0] = TOTAL(DOUBLE(Nimg * ZmaskI))
    NFoutI[1,0] = TOTAL(DOUBLE(Nsigbx * ZmaskI))
    SFoutI[0,0] = TOTAL(DOUBLE(Simg * ZmaskI))
    SFoutI[1,0] = TOTAL(DOUBLE(Ssigbx * ZmaskI))
    SFoutI[2,0] = TOTAL(DOUBLE(Rimg*ecntrat * ZmaskI))
   
    RFoutB[0,0] = TOTAL(DOUBLE(Rimg * ZmaskB))
    RFoutB[1,0] = TOTAL(DOUBLE(Rsigbx * ZmaskB))
    NFoutB[0,0] = TOTAL(DOUBLE(Nimg * ZmaskB))
    NFoutB[1,0] = TOTAL(DOUBLE(Nsigbx * ZmaskB))
    SFoutB[0,0] = TOTAL(DOUBLE(Simg * ZmaskB))
    SFoutB[1,0] = TOTAL(DOUBLE(Ssigbx * ZmaskB))
    SFoutB[2,0] = TOTAL(DOUBLE(Rimg*ecntrat * ZmaskB))
  ENDIF

  Fover = DBLARR(4,n_ellipses) ; Flux, Sky uncertainty, Cntrat uncertainty, Number of pixels
  IF overflag THEN BEGIN
    Oimg = readfits(idir+override,/SILENT)
    FOR ii = 0,n_ellipses-1 DO BEGIN
      ind = WHERE(Oimg GT ii+0.5 AND Oimg LT ii+1.5 ,count)
; Count up the values in these specified pixels.
      IF count GT 0 THEN BEGIN
        Fover[0,ii] = TOTAL(DOUBLE(Simg[ind]))
        Fover[1,ii] = TOTAL(DOUBLE(Ssigbx[ind]))
        Fover[2,ii] = TOTAL(DOUBLE(Rimg[ind]*ecntrat))
        Fover[3,ii] = count
      ENDIF
    ENDFOR
  ENDIF

; Now that we're done with the user-input part of things, open files
; for writing.
  isohdr = "# All data in this file is centered around the isophote center."

  Risofile = STRMID(STRTRIM(Rfile,2),0,STRLEN(Rfile)-5)+"_isophote.profile"
  OPENW,RIunit,odir+Risofile,/GET_LUN
  write_profile_header,RIunit,Rfile,as_pix,Runits,Rscale, $
                       "isophote",isohdr,n_ellipses

  Nisofile = STRMID(STRTRIM(Nfile,2),0,STRLEN(Nfile)-5)+"_isophote.profile"
  OPENW,NIunit,odir+Nisofile,/GET_LUN
  write_profile_header,NIunit,Nfile,as_pix,Nunits,Nscale, $
                       "isophote",isohdr,n_ellipses

  Sisofile = STRMID(STRTRIM(Sfile,2),0,STRLEN(Sfile)-5)+"_isophote.profile"
  OPENW,SIunit,odir+Sisofile,/GET_LUN
  write_profile_header,SIunit,Sfile,as_pix,Sunits,Sscale, $
                       "isophote",isohdr,n_ellipses

  brthdr = "# All data in this file is centered around the brightness peak."

  Rbrtfile = STRMID(STRTRIM(Rfile,2),0,STRLEN(Rfile)-5)+"_brightness.profile"
  OPENW,RBunit,odir+Rbrtfile,/GET_LUN
  write_profile_header,RBunit,Rfile,as_pix,Runits,Rscale, $
                       "brightness",brthdr,n_ellipses

  Nbrtfile = STRMID(STRTRIM(Nfile,2),0,STRLEN(Nfile)-5)+"_brightness.profile"
  OPENW,NBunit,odir+Nbrtfile,/GET_LUN
  write_profile_header,NBunit,Nfile,as_pix,Nunits,Nscale, $
                       "brightness",brthdr,n_ellipses

  Sbrtfile = STRMID(STRTRIM(Sfile,2),0,STRLEN(Sfile)-5)+"_brightness.profile"
  OPENW,SBunit,odir+Sbrtfile,/GET_LUN
  write_profile_header,SBunit,Sfile,as_pix,Sunits,Sscale, $
                       "brightness",brthdr,n_ellipses

  FOR ii = 0,(n_ellipses - 1) DO BEGIN
    binA = binZ * a[ii]
; Now that we have a_f, find FWHM and such
    plot_fluxcurve,binA*as_pix,Rsky[1,ii]/as_pix^2,ratio[ii],bwflag,as_pix, $
                   RFI[*,ii],RFB[*,ii],RFoutI[*,ii],RFoutB[*,ii], $
                   Runits,Rscale,a_f[ii]*as_pix,a_s[ii]*as_pix,Rfile
    plot_fluxcurve,binA*as_pix,Nsky[1,ii]/as_pix^2,ratio[ii],bwflag,as_pix, $
                   NFI[*,ii],NFB[*,ii],NFoutI[*,ii],NFoutB[*,ii], $
                   Nunits,Nscale,a_f[ii]*as_pix,a_s[ii]*as_pix,Nfile
    plot_fluxcurve,binA*as_pix,Ssky[1,ii]/as_pix^2,ratio[ii],bwflag,as_pix, $
                   SFI[*,ii],SFB[*,ii],SFoutI[*,ii],SFoutB[*,ii], $
                   Sunits,Sscale,a_f[ii]*as_pix,a_s[ii]*as_pix,Sfile, $
                   CNTSIGI=(RFI[*,ii]*ecntrat), $
                   CNTSIGB=(RFB[*,ii]*ecntrat)
    plot_fluxcurve,binA*as_pix,Ssky[1,ii]/as_pix^2,ratio[ii],bwflag,as_pix, $
                   SSBI[*,ii],SSBB[*,ii],dummy,dummy, $
                   Sunits,Sscale/(as_pix)^2,a_f[ii]*as_pix,a_s[ii]*as_pix,Sfile,/SB

; Create the .jpg of the graph
    jpgfile = STRMID(Sfile,0,STRLEN(Sfile)-11)+"_flux"+STRTRIM(STRING(ii+1),2)+".jpg"
    PRINT,"Writing image to ",jpgfile
    im = tvrd(true=3)
    write_jpeg,odir+jpgfile,im,true=3,quality=100

    temp = MIN(ABS(binZ - (a_f[ii]/a[ii])),endbin)

    total = minibox(Rimg,Px[ii],Py[ii],(14.0/as_pix))
    PRINT,"R band image:"
    PRINT,"  Box 28x28 flux is: ",total*Rscale," ",Runits
    PRINT,"  Final flux is: ",RFI[endbin,ii]*Rscale," ",Runits
    PRINT,"  Fraction is ",total/RFI[endbin,ii]

    total = minibox(Nimg,Px[ii],Py[ii],(14.0/as_pix))
    PRINT,"Narrow band image:"
    PRINT,"  Box 28x28 flux is: ",total*Nscale," ",Nunits
    PRINT,"  Final flux is: ",NFI[endbin,ii]*Nscale," ",Nunits
    PRINT,"  Fraction is ",total/NFI[endbin,ii]

    total = minibox(Simg,Px[ii],Py[ii],(14.0/as_pix))
    PRINT,"Subtracted image:"
    PRINT,"  Box 28x28 flux is: ",total*Sscale," ",Sunits
    PRINT,"  Final flux is: ",(SFI[endbin,ii]+SFoutI[0,ii])*Sscale," ",Sunits
    PRINT,"  Fraction is ",total/(SFI[endbin,ii]+SFoutI[0,ii])

; Now, output the profiles
; Like everything else in this program, we do everything twice.
    PRINT,"  Writing isophote-curve profile"
    growth_profile,RIunit,ii,Dx[ii],Dy[ii],ratio[ii],pa[ii],del_pa, $
                   Rsky[*,ii],a_f[ii],a_s[ii],a_c[ii],binA, $
                   RFI[*,ii],RINgood[*,ii],RINbad[*,ii], $
                   RFgI[*,ii],RSBI[*,ii],RSBsigI[*,ii],RFoutI[*,ii]
    growth_profile,NIunit,ii,Dx[ii],Dy[ii],ratio[ii],pa[ii],del_pa, $
                   Nsky[*,ii],a_f[ii],a_s[ii],a_c[ii],binA, $
                   NFI[*,ii],NINgood[*,ii],NINbad[*,ii], $
                   NFgI[*,ii],NSBI[*,ii],NSBsigI[*,ii],NFoutI[*,ii]
    growth_profile,SIunit,ii,Dx[ii],Dy[ii],ratio[ii],pa[ii],del_pa, $
                   Ssky[*,ii],a_f[ii],a_s[ii],a_c[ii],binA, $
                   SFI[*,ii],SINgood[*,ii],SINbad[*,ii], $
                   SFgI[*,ii],SSBI[*,ii],SSBsigI[*,ii],SFoutI[*,ii], $
                   CNTSIG=(RFI[*,ii]*ecntrat),FOVER=Fover[*,ii]

; ...and once for the brightness-peak image
    PRINT,"  Writing brightness-peak profile"
    growth_profile,RBunit,ii,Px[ii],Py[ii],ratio[ii],pa[ii],del_pa, $
                   Rsky[*,ii],a_f[ii],a_s[ii],a_c[ii],binA, $
                   RFB[*,ii],RBNgood[*,ii],RBNbad[*,ii], $
                   RFgB[*,ii],RSBB[*,ii],RSBsigB[*,ii],RFoutB[*,ii]
    growth_profile,NBunit,ii,Px[ii],Py[ii],ratio[ii],pa[ii],del_pa, $
                   Nsky[*,ii],a_f[ii],a_s[ii],a_c[ii],binA, $
                   NFB[*,ii],NBNgood[*,ii],NBNbad[*,ii], $
                   NFgB[*,ii],NSBB[*,ii],NSBsigB[*,ii],NFoutB[*,ii]
    growth_profile,SBunit,ii,Px[ii],Py[ii],ratio[ii],pa[ii],del_pa, $
                   Ssky[*,ii],a_f[ii],a_s[ii],a_c[ii],binA, $
                   SFB[*,ii],SBNgood[*,ii],SBNbad[*,ii], $
                   SFgB[*,ii],SSBB[*,ii],SSBsigB[*,ii],SFoutB[*,ii], $
                   CNTSIG=(RFB[*,ii]*ecntrat),FOVER=Fover[*,ii]
  ENDFOR

  IF KEYWORD_SET(tv) THEN BEGIN
; Plot the basic 3-color image, including the masking.
    rsig = SXPAR(Rhd,"SKYSIG")
    csig = SXPAR(Nhd,"SKYSIG")
    ssig = SXPAR(Shd,"SKYSIG")
    singg_tvplot,Rimg,Nimg,Simg,[0.0,rsig],[0.0,csig],[0.0,ssig], $
                 3,cntrat,mask=Rmask

    PLOTS,(Px/sz[1]),(Py/sz[2]),PSYM=SYM(13),COLOR=!black,SYMSIZE=2.0,/NORMAL

    plot_ellipse,Dx,Dy,theta,a_f,a_f/ratio,!white,BARCLR=!ddgray, $
                 normal=[sz[1],sz[2]]
  ENDIF

; Now that we're done, close the profile files.
;  CLOSE,/ALL
  CLOSE,RIunit
  CLOSE,NIunit
  CLOSE,SIunit
  CLOSE,RBunit
  CLOSE,NBunit
  CLOSE,SBunit
  FREE_LUN,RIunit
  FREE_LUN,NIunit
  FREE_LUN,SIunit
  FREE_LUN,RBunit
  FREE_LUN,NBunit
  FREE_LUN,SBunit

  RETURN

END
