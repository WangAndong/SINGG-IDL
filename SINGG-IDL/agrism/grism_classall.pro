PRO grism_classall, setup=setup
   COMMON sensmod, lams, sens, esens, hsens
   ;
   ; classify all aXe grism spectra.
   ;
   ; setup  -> if set, this executes IDL commands which specify
   ;           variables used in setup.
   ;
   ; this is a generalized from hdfn_classify, originally written 2002
   ; updated 07/2004
   ;
   ; G. Meurer 07/2004
   ;
   ; default setup
   wd       = 'Output/'                    ; directory containing aXe output
   outdir   = 'Plots/'                     ; directory off of wd for plots
   pfit      = 'spec_gfit_'                 ; prefix for Gaussian fit plots
   filsens  = 'ACS.WFC.1st.sens.5.fits'    ; sensistivity curve
   exts     = 1                            ; extension for sensitivity curve
   filspc   = 'g800l_1.SPC.fits'           ; aXe extracted spectra
   filcat   = 'g800l_1.cat'                ; aXe catalog
   filimg   = 'detectionImage.fits'        ; direct image name
   filgrim  = 'g800l_drz_sci.fits'         ; grism image name
   logf     = outdir + 'emsource.cat'      ; log file for emission line sources
   logm     = outdir + 'mstar_sn.cat'      ; log file for m star (or SN) detections
   logi     = outdir + 'spurious.cat'      ; log file for spurious sources
   fnorm    = 1.0e-18                      ; flux normalization
   lrange   = [5800.0, 9800.0]             ; wavelength range for plotting
   nsmooth  = 19                           ; number of pixels for smoothing in wl
   nsig     = 4.0                          ; min peak S/N for classification
   w50lim   = 8.0                          ; looks for zero order contam sources smaller than this
   maglim   = 28.0                         ; magnitude limit for zero order contam check.
   covar    = 1.00                         ; pixel-pixel covariance (?)
   xoff0    = -111.5                       ; 0th order offset in X
   yoff0    = -0.26                        ; 0th order offset in Y
   xbuff    = 0.0                          ; increase zero ord contam search box by this in X
   ybuff    = 2.0                          ; increase zero ord contam search box by this in Y
   ;
   ; execute commands in setup file, if it exists
   IF keyword_set(setup) THEN BEGIN 
      readfmt, setup, '(a120)', line
      nl = n_elements(line)
      FOR i = 0, nl-1 DO BEGIN 
         lin = strtrim(line[i],2)
         print, lin
         IF strmid(lin,0,1) NE ';' THEN result = execute(lin)
      ENDFOR 
   ENDIF 
   ;
   pfxpfit  = outdir+pfit
   ; print, pfxpfit
   ;
   cwd      = ''
   cd, wd, current=cwd
   setplotcolors
   ;
   print,'Setting error model ... '
   fits_read,filgrim,dum,hgrim,/header_only
   exptime = sxpar(hgrim, 'EXPTIME')
   seterrmod2, hgrim, dark=dark, covar=covar, /verbose
   setsensmod, filsens, extsens
   ;
   print,'Reading direct image catalog ...'
   rd_grdirct_cat, filcat, idcat, xim, yim, magauto, aim, bim, thetaim, w50, class, type=type
   theta = thetaim * !pi / 180.0
   idcat = fix(temporary(idcat))
   print,'Reading direct image header...'
   fits_read,filimg,dum,himg,/header_only
   print,'Calculating RA & Dec ...'
   xyad, himg, xim, yim, radeg, decdeg
   theta = thetaim * !pi / 180.0
   ;
   ; open spectra image
   luns  = fxposit(filspc, 1)
   oldid = 0
   oldi  = 0
   ;
   ; open logs
   grcl_openlog, luf, outdir+logf, /fitlog
   grcl_openlog, lum, outdir+logm
   grcl_openlog, lui, outdir+logi
   ;
   ; loop through test spectra: update fluxes, plot, plot residuals
   ;
   FOR i = 0, n_elements(idcat)-1 DO BEGIN 
      id      = idcat[i]
      skip    = i - oldi - 1
      ;bintab  = kludge_mrdfits(luns, skip, hdr)
      bintab  = mrdfits(luns, skip, hdr)
      npix    = n_elements(bintab.lambda)
      ;
      ; only proceed if spectrum is long enough
      IF npix GT nsmooth THEN BEGIN 
         grism_updflux, bintab, exptime=exptime, rate=rate
         ;
         objpar  = [xim[i], yim[i], magauto[i], aim[i], bim[i], theta[i], w50[i], class[i], $
                    radeg[i], decdeg[i]]
         ;
         ; check contamination
         grism_boundbox, hdr, boxx, boxy, warn
         grism_zerocontam, boxx, boxy, idcat, xim, yim, magauto, w50, $
          xoff0, yoff0, maglim, w50lim, xbuff, ybuff, ncontam, idc, xgr, ygr
         ;
         ; classify
         grism_classify, bintab, id, objpar, lrange, nsmooth, nsig, luf, lum, lui, $
          minsn, maxsn, ncontam, result, fnorm=fnorm, pfx=pfxpfit
         ; keywait, 'Press any key to continue'
      ENDIF 
      ;
      oldi   = i
   ENDFOR 
   free_lun, luns
   free_lun, luf
   free_lun, lum
   free_lun, lui
   ;
   ; return to start directory
   cd, cwd
END

; emcat_html, 'emsource.cat', 'emsource.html', 'Emission line sources in HDFN'
; msnspucat_html, 'mstar_sn.cat', 'mstar_sn.html', 'Stellar spectra in HDFN selected with grism_classify'
; msnspucat_html, 'spurious.cat', 'spurious.html', 'Spurious emission line candidates in HDFN selected with grism_classify'
