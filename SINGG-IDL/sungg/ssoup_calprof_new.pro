PRO ssoup_calprof_new, ll, band, photplam, ebvg, fprofs, fscalprof, ffcalprof, fscalprof0, ffcalprof0, $
                   fecntrat=fecntrat
  ;
  ; calibrate surface brightness profiles and their errors
  ; tabulate results into a single multiband output file.
  ;
  ;  ll         -> logical unit of log file
  ;  band       -> names of bands to process.  This should include
  ;                'R', 'HALPHA', 'NUV', 'FUV'
  ;  photplam   -> pivot wavelength of the bands
  ;  ebvg       -> Galactic (foreground) extinction
  ;  fprofs     -> corresponding profile file names
  ;                the original fits file names will be in the 
  ;                header section, and their headers in turn will
  ;                be read for additional parameters
  ;  fscalprof  -> name of file for output calibrated surface 
  ;                brightness profiles and errors.  
  ;  ffcalprof  -> name of file for output calibrated enclosed flux 
  ;                and error profiles
  ;  fscalprof0 -> name of file output dust corrected surface 
  ;                brightness and error profiles
  ;  ffcalprof0 -> name of file for output dust corrected enclosed 
  ;                flux and error profiles.
  ;  fecntrat   -> If set the the fractional error in the continuum
  ;                scaling ratio.  Otherwise the error in the scaling 
  ;                ratio is taken from the net Halpha image fits 
  ;                header.
  ;
  ; G. Meurer (ICRAR/UWA) 6/2010
  ; G. Meurer (ICRAR/UWA) 4/2011: tidy code fix so it works on 
  ;                               multiple galaxies in the field
  ;
  ; bugs/issues: probably need to check whether this will work if
  ;              order in band or bname is different from default.
  ;
  ; setup stuff
  bname   = ['R', 'HALPHA', 'NUV', 'FUV']
  fmto    = '(f7.2,f8.3,f6.3,f9.3,f6.3,f6.3,f6.3,f8.3,f6.3,f8.3,f6.3,f8.3,f6.3,f8.3,f6.3,f8.3,f6.3,f8.3,f6.3)'
  hlines1 = '# Surface quantities (in annuli)'
  hlines2 = '#  sma   mu_R   err     lSHa   esky  ecnt  etot   mu_nuv err    mu_fuv err     C(f-n) err    C(n-R) err    lHa/R err     lHa/f err  '
  hlines3 = '# Surface quantities, dust corrected (in annuli)'
  hlinef1 = '# Integral quantities (in apertures)'
  hlinef2 = '#  sma   mag_R  err     lFHa   esky  ecnt  etot  mag_nuv err   mag_fuv err     C(f-n) err    C(n-R) err    lHa/R err     lHa/f err  '
  hlinef3 = '# Integral quantities, dust corrected (in apertures)'
  ;snlimit = 3.0
  snlimit = 2.0  ; GRM 4/2011 : go out further
  pixsize = 1.0
  mflag   =  99.999  ; magnitude flag value
  emflag  =   9.999  ; error mag flag
  lflag   = -99.999  ; log flux flux value
  elflag  =   9.999  ; error log flag
  cuflag  =   8.888  ; colour upper limit flag
  clflag  =   7.777  ; colour lower limit flag
  prog    = 'SSOUP_CALPROF_NEW: '
  ;
  nb      = n_elements(bname)
  ;
  plog,ll,prog,'----------------- starting '+prog+'----------------------'
  ;
  ; convert S/N limit to err limits in mag and dex
  edlim   = alog10(1.0+1.0/snlimit)    ; dex
  emlim   = 2.5*edlim                  ; mag
  ;
  ; read one header just to get number of galaxies
  pfplt_rdhdr, fprofs[0], pixsize, filename, funits, fscale, fluxcal, $
               proftype, numgals, galindex, xcenter, ycenter, $
               axerat, posang, posangwc, skylev, skysigpx, skysigbx, $
               rads, radf, radc, fluxs, fluxf, fluxt, flsigskys, flsigskyf, flsigskyt, $
               flsigcnts, flsigcntf, flsigcntt, ref, ret, $
               resigskyf, resigskyt, resigcntf, resigcntt, sef, set, $
               lstart, lend, isoflag, netflag, /silent
  ngal     = numgals
  arat     = axerat
  plog,ll,prog,'number of galaxies in profile files = '+numstr(ngal)
  ;
  ; work out bname <-> band correspondence 
  plog,ll,prog,'working out band name correspondence'
  p0       = where(band EQ bname[0],np0)
  p1       = where(band EQ bname[1],np1)
  p2       = where(band EQ bname[2],np2)
  p3       = where(band EQ bname[3],np3)
  pb       = [p0, p1, p2, p3]
  ;
  ; get deredden parameters
  dredf    = make_array(nb, /float, value=1.0)
  IF ebvg GT 0 THEN ccm_unred, photplam, dredf, ebvg[0]
  plog,ll,prog,'will de-redden fluxes for foreground dust using the following band | wl | factor sets'
  FOR ii = 0, nb-1 DO plog,ll,prog,'   '+ljust(band[ii],6)+' | '+numstr(photplam[ii])+' | '+numstr(dredf[ii])
  ;
  phpl2    = photplam
  pp       = where(band EQ 'HALPHA',npp)
  IF npp GE 1 THEN phpl2[pp] = 6563.8
  acalzlaw = make_array(nb, /float, value=1.0)
  calz_unred, phpl2, acalzlaw, 1.0
  acalzlaw = 2.5*alog10(acalzlaw)
  IF npp GE 1 THEN acalzlaw[pp] = acalzlaw[pp]/0.45   ; correct for emission lines
  plog,ll,prog,'will de-redden fluxes for internal dust by the following law (Calzetti): band | wl | mag/E(B-V)'
  FOR ii = 0, nb-1 DO plog,ll,prog,'   '+ljust(band[ii],6)+' | '+numstr(phpl2[ii])+' | '+numstr(acalzlaw[ii])
  ;
  ; arrays for different galaxies will be concatenated
  ; work out start and stop elements
  nring    = lend - lstart + 1
  pt0      = make_array(ngal, /long, value=0l)
  FOR ii = 1, ngal-1 DO pt0[ii] = pt0[ii-1]+nring[ii-1]  ; start position of array for this galaxy
  pt1      = pt0 + nring - 1l                            ; end position of header
  nrtot    = total(nring)
  plog,ll,prog,'total number of rings = '+numstr(nrtot)
  FOR ii = 0, ngal-1 DO plog,ll,prog,'Galaxy #'+numstr(ii)+' number of rings = '+numstr(nring[ii])
  ;
  ; make arrays to store stuff
  ; in the following quantities starting (or containing):
  ; "sb" - linear surface brightness (per arcsec^2)
  ; "fb" - linear flux (integerated within aperture)
  ; "sm" - logarithmic surface brightness (mag or log per arcsec^2)
  ; "fm" - logarithmic flux 
  filnam   = make_array(nb, /string, value='')
  rad      = make_array(nrtot)
  sbprof   = make_array(nrtot,nb)   ; surface brightnes linear units all bands
  esbproft = make_array(nrtot,nb)   ; total error in surface brightness linear units all bands
  esbprofc = make_array(nrtot)      ; Halpha continuum subtraction error linear surf bright
  esbprofs = make_array(nrtot)      ; Halpha sky + photon error linear surf bright
  fbprof   = make_array(nrtot,nb)   ; flux linear units all bands
  efbproft = make_array(nrtot,nb)   ; total error in flux linear units 
  efbprofc = make_array(nrtot)      ; Halpha continuum subtraction error linear flux
  efbprofs = make_array(nrtot)      ; Halpha sky+photon error linear flux
  sbr_raw  = make_array(nrtot, /float, value=0.0) ; for raw surface brightness profiles in R band
  fbr_raw  = make_array(nrtot, /float, value=0.0) ; for raw enclosed aperture fluxes in R band     
  mag0     = make_array(nb, /float, value=0.0)    ; magnitude scale zeropoint
  phfl     = make_array(nb, /float, value=0.0)    ; photflam (continuum) or photflux (Halpha)
  ;
  ; initialize skylevr
  skylevr  = 0.0
  ;
  ; loop through bands
  FOR ii = 0, nb-1 DO BEGIN 
     ;
     ; pointer to position in db arrays
     pp     = pb[ii]
     ;
     ; read header of file, the main important info is the filename
     plog,ll,prog,'reading in header of file : '+fprofs[pp]
     pixsize = 0.0
     pfplt_rdhdr, fprofs[pp], pixsize, filename, funits, fscale, fluxcal, $
                  proftype, numgals, galindex, xcenter, ycenter, $
                  axerat, posang, posangwc, skylev, skysigpx, skysigbx, $
                  rads, radf, radc, fluxs, fluxf, fluxt, flsigskys, flsigskyf, flsigskyt, $
                  flsigcnts, flsigcntf, flsigcntt, ref, ret, $
                  resigskyf, resigskyt, resigcntf, resigcntt, sef, set, $
                  lstart, lend, isoflag, netflag, /silent
     filnam[ii] = filename
     pixarea    = pixsize*pixsize
     ;
     ; Get stuff from fits header
     plog,ll,prog,'reading in header quantities from fits file: '+filename
     fits_read, filename, im, hd, /header_only
     exptime    = sxpar(hd, 'EXPTIME', count=count)
     IF count NE 1 THEN stop, '**** should be one & only one EXPTIME in header, found='+numstr(count)
     eslev      = sxpar(hd, 'SKYSIGBX', count=count)
     IF count NE 1 THEN stop, '**** should be one & only one SKYSIGBX in header, found='+numstr(count)
     plog,ll,prog,'    SKYSIGBX = '+numstr(eslev)
     skylev     = sxpar(hd, 'SKYLEV', count=count)
     IF count NE 1 THEN stop, '**** should be one & only one SKYLEV in header, found='+numstr(count)
     IF bname[ii] EQ 'HALPHA' THEN BEGIN 
        ;
        ; get PHOTFLUX, CNTRAT1, ECNTRAT2
        netflag = 1b
        phfl[ii] = sxpar(hd, 'PHOTFLUX', count=count)
        IF count NE 1 THEN stop, '**** should be one and only one PHOTFLUX in header, found'+numstr(count)
        mag0[ii] = alog10(phfl[ii])
        plog,ll,prog,'    PHOTFLUX = '+numstr(mag0[ii])
        crat     = sxpar(hd, 'CNTRAT1', count=count)
        IF count NE 1 THEN stop, '**** should be one and only one CNTRAT1 in header, found'+numstr(count)
        plog,ll,prog,'    CNTRAT1 = '+numstr(crat)
        IF keyword_set(fecntrat) THEN BEGIN 
           ecrat = fecntrat*crat
           plog,ll,prog,'    ECNTRAT2 = '+numstr(ecrat)+'  ( = default_ratio * CNTRAT1)'
        ENDIF ELSE BEGIN 
           ecrat = sxpar(hd, 'ECNTRAT2', count=count)
           IF count NE 1 THEN stop, '**** should be one and only one ECNTRAT2 in header, found'+numstr(count)
           plog,ll,prog,'    ECNTRAT2 = '+numstr(ecrat)
        ENDELSE 
     ENDIF ELSE BEGIN 
        ;
        ; get MAGZPT1, PHOTFLAM
        netflag  = 0b
        mag0[ii] = sxpar(hd, 'MAGZPT1', count=count)
        IF count NE 1 THEN stop, '**** should be one and only one MAGZPT1 in header, found'+numstr(count)
        plog,ll,prog,'    MAGZPT1 = '+numstr(mag0[ii])
        phfl[ii] = sxpar(hd, 'PHOTFLAM', count=count)
        IF count NE 1 THEN stop, '**** should be one and only one PHOTFLAM in header, found'+numstr(count)
        plog,ll,prog,'    PHOTFLAM = '+numstr(phfl[ii])
     ENDELSE 
     ;
     ; save R band sky level separately
     if bname[ii] eq 'R' then skylevr = skylev
     ;
     ; read profiles and store
     FOR jj = 0, ngal-1 DO BEGIN 
        ptt0     = pt0[jj]    ; this saves me some typing
        ptt1     = pt1[jj]    ; this saves me some typing
        ;
        plog,ll,prog,'reading profile for galaxy #'+numstr(jj+1)
        ;pfplt_extractprof, fprofs[pp], netflag, sma, fint, dfint, ngood, nbad, sb, esb, $
        ;                   dfraw, egintsk, efintcn, lstart=lstart[jj], lend=lend[jj]
        pfplt_extractprof, fprofs[pp], 0b, sma, fint, dfint, ngood, nbad, sb, esb, $
                           dfraw, egintsk, efintcn, lstart=lstart[jj], lend=lend[jj]
        ;
        ; derive Poissonian error contribution
        nap      = n_elements(sma)                   ; number of apertures
        k1       = 1+indgen(nap-1)                   ; index for outer radius of annuli 
        k0       = k1 - 1                            ; index for inner radius of annuli
        sma0     = make_array(nap,/float,value=0.0)  ; put interior radius of annulus here
        sma0[k1] = sma[k0]                           ; sets all inner radii except for 1st annulus where inner rad = 0.0 in prev line
        npixap   = !pi*sma^2/axerat[jj]              ; ideal number of pixels in ap
        npixan   = !pi*(sma^2 - sma0^2)/axerat[jj]   ; ideal number of pixels in an 
        ngoodt   = total(ngood,/cumulative)          ; total good pixels in apertures
        frawan   = ngood*sb                          ; flux from pixels centered in annulus, doesn't include bad pixels
        frawap   = total(frawan,/cumulative)         ; total flux not corrected for partial or bad pixels
        IF band[ii] NE 'HALPHA' THEN BEGIN 
           ephotsb = sqrt((sb + skylev)/(exptime*ngood))  ; photon error per pixel
           ephotfb = (fint/frawap)*sqrt((frawap+ngoodt*skylev)/exptime)   ; photon error in ap 
        ENDIF ELSE BEGIN 
           ephotsb = sqrt((sb+crat*sbr_raw[ptt0:ptt1] + skylev + crat*skylevr)/(exptime*ngood))
           ephotfb = (fint/frawap)*sqrt(frawap+crat*fbr_raw[ptt0:ptt1]+ngoodt*(skylev+crat*skylevr))
           ;ephotsb = 0.0*sb       ; **** have not yet implemented photon stats uncertainty for Halpha
           ;ephotfb = 0.0*fint
        ENDELSE 
        ;
        ; store raw R band surface brightness and enclosed flux 
        ; profiles; they will be needed in the Halpha noise model.
        if band[ii] eq 'R' then begin
           sbr_raw[ptt0:ptt1]  = sb
           fbr_raw[ptt0:ptt1]  = frawap
        endif
        ;
        ; convert to final quantities and store  
        plog,ll,prog,'inclination correct and derive fractional sky errors for galaxy #'+numstr(jj+1)
        IF ii EQ 0 THEN rad[ptt0:ptt1] = sma*pixsize
        factsb                 = 1.0/(axerat[jj]*pixarea)
        factfb                 = 1.0/axerat[jj]
        sbprof[ptt0:ptt1,ii]   = factsb*sb                ; correct to SB per area, and correct to face-on
        fbprof[ptt0:ptt1,ii]   = factfb*fint              ; correct to face-on
        esbproft[ptt0:ptt1,ii] = factsb*sqrt(eslev^2+ephotsb^2)       ; total error = sky+photon
        efbproft[ptt0:ptt1,ii] = factfb*sqrt((npixap*eslev)^2+ephotfb^2)  ; total error = sky+photon
     ENDFOR 
  ENDFOR
  ;
  ; some pointers to be sure
  ih        = where(bname EQ 'HALPHA', nih)
  ir        = where(bname EQ 'R', nir)
  ih        = ih[0]
  ir        = ir[0]
  ;
  ; derive continuum subtraction and total fractional errors for Halpha
  plog,ll,prog,'calculating HALPHA continuum subtraction, sky+photon and total errors '
  esbprofc[ptt0:ptt1]     = ecrat*sbprof[ptt0:ptt1,ir]                       ; continuum subtraction error
  efbprofc[ptt0:ptt1]     = ecrat*fbprof[ptt0:ptt1,ir]                       ; continuum subtraction error
  esbprofs[ptt0:ptt1]     = esbproft[ptt0:ptt1,ih]                           ; assign Halpha sky error to prev calculated total error
  efbprofs[ptt0:ptt1]     = efbproft[ptt0:ptt1,ih]                           ; assign Halpha sky error to prev calculated total error
  esbproft[ptt0:ptt1,ih]  = sqrt(esbprofc[ptt0:ptt1]^2 + esbprofs[ptt0:ptt1]^2)  ; recalculate total error
  efbproft[ptt0:ptt1,ih]  = sqrt(efbprofc[ptt0:ptt1]^2 + efbprofs[ptt0:ptt1]^2)  ; recalculate total error
  ;
  ; flux cal, and correct for foreground dust
  ; convert errors to magnitudes and logs as needed
  ; do this by calling ssoup_cp_calcmags twice:
  ; once for surface brightness, the other time for enclosed fluxes
  ssoup_cp_calcmags, ll, band, mag0, dredf, snlimit, sbprof, esbproft, esbprofc, esbprofs, $
                     smprof, esmproft, esmprofc, esmprofs, mflag, emflag, lflag, elflag
  ssoup_cp_calcmags, ll, band, mag0, dredf, snlimit, fbprof, efbproft, efbprofc, efbprofs, $
                     fmprof, efmproft, efmprofc, efmprofs, mflag, emflag, lflag, elflag, /flag_trailing
  ;
  FOR ii = 0, nb-1 DO BEGIN 
     ;
     ; now calibrate linear surface brightness and enclosed fluxes,
     ; this requires rewriting flux (& sb) arrays which are in
     ; count/s (/area) to erg/cm^2/s (/area) and rewriting the
     ; relevant error arrays which are currently in uncalibrated
     ; countrate
     sbprof[*,ii]   = dredf[ii]*phfl[ii]*sbprof[*,ii]
     esbproft[*,ii] = dredf[ii]*phfl[ii]*esbproft[*,ii]
     fbprof[*,ii]   = dredf[ii]*phfl[ii]*fbprof[*,ii]
     efbproft[*,ii] = dredf[ii]*phfl[ii]*efbproft[*,ii]
     IF ii EQ ih THEN BEGIN 
        esbprofc = dredf[ii]*phfl[ii]*esbprofc
        esbprofs = dredf[ii]*phfl[ii]*esbprofs
        efbprofc = dredf[ii]*phfl[ii]*esbprofc
        efbprofs = dredf[ii]*phfl[ii]*esbprofs
     ENDIF 
  ENDFOR 
  ;
  ; calculate colors and errors
  ; we do this by calling ssoup_cp_ccolours because there are 
  ; places we do this.
  ssoup_cp_ccolours, ll, bname, mag0, phfl, smprof, esmproft, smcfn, esmcfnt, smcnr, esmcnrt, slewr, eslewrt, slewf, eslewft, $
                     mflag, emflag, lflag, elflag, clflag, cuflag
  ssoup_cp_ccolours, ll, bname, mag0, phfl, fmprof, efmproft, fmcfn, efmcfnt, fmcnr, efmcnrt, flewr, eflewrt, flewf, eflewft, $
                     mflag, emflag, lflag, elflag, clflag, cuflag
  ;
  ; get sma for annuli
  plog,ll,prog,'calculating annuli radii'
  radan  = 0.0*rad
  FOR jj = 0, ngal-1 DO BEGIN 
     ptt0      = pt0[jj]    ; this saves me some typing
     ptt1      = pt1[jj]    ; this saves me some typing
     nap       = ptt1-ptt0+1l
     sma_ap    = rad[ptt0:ptt1]
     sma_an    = make_array(nap, /float, value=0.0)
     sma_an[0] = sma_ap[0]/sqrt(2)
     IF nap GT 1 THEN BEGIN 
        kk0         = lindgen(nap-1)
        kk1         = kk0+1l
        sma_an[kk1] = sqrt((sma_ap[kk0]^2+sma_ap[kk1]^2)/2.)
     ENDIF 
     radan[ptt0:ptt1] = sma_an
  ENDFOR 
  ;
  ; derive maximum good radius
  plog,ll,prog,'deriving maximum radii'
  kssnlim  = make_array(ngal, nb, /long, value=0l)  ; limit for surface brightness
  kfsnlim  = make_array(ngal, nb, /long, value=0l)  ; limit for total flux
  pts2     = make_array(ngal, /long, value=0l)  ; pointer to s/n limited end of sb array
  ptf2     = make_array(ngal, /long, value=0l)  ; pointer to s/n limited end of fb array
  ;
  ; loop through galaxies
  FOR jj = 0, ngal-1 DO BEGIN 
     ptt0     = pt0[jj]    ; this saves me some typing
     ptt1     = pt1[jj]    ; this saves me some typing
     ;
     ; loop through bands
     FOR ii = 0, nb-1 DO BEGIN 
        efproft_ = efmproft[ptt0:ptt1,ii]
        esproft_ = esmproft[ptt0:ptt1,ii]
        IF bname[ii] EQ 'HALPHA' THEN BEGIN 
           kk       = where(efproft_ LE edlim AND efproft_ NE elflag, nkk)
           IF nkk GT 0 THEN kfsnlim[jj,ii] = max(kk) ELSE kfsnlim[jj,ii] = nring[jj]-1l
           kk       = where(esproft_ LE edlim AND esproft_ NE elflag, nkk)
           IF nkk GT 0 THEN kssnlim[jj,ii] = max(kk) ELSE kssnlim[jj,ii] = nring[jj]-1l
        ENDIF ELSE BEGIN 
           kk       = where(efproft_ LE emlim AND efproft_ NE emflag, nkk)
           IF nkk GT 0 THEN kfsnlim[jj,ii] = max(kk) ELSE kfsnlim[jj,ii] = nring[jj]-1l
           kk       = where(esproft_ LE emlim AND esproft_ NE emflag, nkk)
           IF nkk GT 0 THEN kssnlim[jj,ii] = max(kk) ELSE kssnlim[jj,ii] = nring[jj]-1l
        ENDELSE 
        ;print,nring[jj],kfsnlim[jj,0:3],kssnlim[jj,0:3]
     ENDFOR 
     kfsnlimg     = max(kfsnlim[jj,*])
     kssnlimg     = max(kssnlim[jj,*])
     sma_         = rad[ptt0:ptt1]
     sma_an_      = radan[ptt0:ptt1]
     plog,ll,prog,'Galaxy #'+numstr(jj+1)+' flux max(index) = '+numstr(kfsnlimg)+' max(sma_ap) = '+numstr(sma_[kfsnlimg])
     plog,ll,prog,'         '+          ' sb max(index)   = '+numstr(kssnlimg)+' max(sma_an) = '+numstr(sma_an_[kssnlimg])
     ;
     ; convert kssnlimg & kfsnlimg to pointers
     pts2[jj]     = pt0[jj]+kssnlimg
     ptf2[jj]     = pt0[jj]+kfsnlimg
  ENDFOR 
  ;
  ; write output files
  plog,ll,prog,'writing calibrated surface brightnesses and colors'
  ssoup_cp_wmagfile, ll, 0, fscalprof, ngal, pt0, pts2, radan, smprof, esmproft, esmprofs, esmprofc, $
                     smcfn, esmcfnt, smcnr, esmcnrt, slewr, eslewrt, slewf, eslewft
  plog,ll,prog,'writing calibrated integrated magnitudes and colors'
  ssoup_cp_wmagfile, ll, 1, ffcalprof, ngal, pt0, ptf2, rad, fmprof, efmproft, efmprofs, efmprofc, $
                     fmcfn, efmcfnt, fmcnr, efmcnrt, flewr, eflewrt, flewf, eflewft
  ;
  ; derive dust corrected colors and flux ratios
  ;
  ; make arrays for reddening corrected quantities
  ; for now using same nrtot as for un-corrected quantities.  
  ; some compaction is possible, in some cases
  sbprof0   = make_array(nrtot,nb)   ; surface brightnes linear units all bands, corrected for internal dust
  smprof0   = make_array(nrtot,nb)   ; surface brightnes log units all bands, corrected for internal dust
  fbprof0   = make_array(nrtot,nb)   ; flux linear units all bands, corrected for internal dust
  fmprof0   = make_array(nrtot,nb)   ; log flux all bands, corrected for internal dust
  esbproft0 = make_array(nrtot,nb)
  esbprofc0 = make_array(nrtot)
  esbprofs0 = make_array(nrtot)
  efbproft0 = make_array(nrtot,nb)
  efbprofc0 = make_array(nrtot)
  efbprofs0 = make_array(nrtot)
  esmproft0 = make_array(nrtot,nb)
  esmprofc0 = make_array(nrtot)
  esmprofs0 = make_array(nrtot)
  efmproft0 = make_array(nrtot,nb)
  efmprofc0 = make_array(nrtot)
  efmprofs0 = make_array(nrtot)
  ;
  ; calculate colors and errors
  plog,ll,prog,'calculating dust corrected colors and errors'
  ;
  ; derive internal extinction vector, using IRX-beta fit of Boissier
  ; et al. (2007, ApJS, 173, 524-537; see pg528)
  ca   =  0.570
  cb   =  0.671
  cc   =  3.220
  lirx =  alog10(10.0^(ca + cb*smcfn)-cc)
  afuv = -0.0333*lirx^3+0.3522*lirx^2+1.1960*lirx+0.4967  ; A(FUV) as a function of radius
  pf   = where(band EQ 'FUV', npf)
  IF npp NE 1 THEN stop, 'there should be one and only one FUV band'
  ;
  ; derive dust corrected maximum good radius
  plog,ll,prog,'deriving dust corrected maximum radii'
  kssnlim0  = make_array(ngal, nb, /long, value=0l)  ; limit for surface brightness
  kfsnlim0  = make_array(ngal, nb, /long, value=0l)  ; limit for total flux
  pts3      = make_array(ngal, /long, value=0l)  ; convert kssnlimg to a pointer
  ptf3      = make_array(ngal, /long, value=0l)  ; convert kfsnlimg to a pointer
  ;
  ; loop through galaxies
  FOR jj = 0, ngal-1 DO BEGIN 
     ptt0     = pt0[jj]    ; this saves me some typing
     ptt1     = pt1[jj]    ; this saves me some typing
     ;
     ; pointers to how far out the reddening correction goes
     ksnf = min([kssnlim[jj,p2],kssnlim[jj,p3]])  
     kfnf = min([kssnlim[jj,p2],kssnlim[jj,p3]])
     ;
     ; loop through bands
     FOR ii = 0, nb-1 DO BEGIN 
        ;
        ; new limits is the minimum of the limit for the S/N of the band
        ; and the limit of the reddening correction
        kssnlim0[jj,ii] = min([kssnlim[jj,ii],ksnf])
        kfsnlim0[jj,ii] = min([kfsnlim[jj,ii],kfnf])
     ENDFOR 
     kfsnlimg = max(kfsnlim0[jj,*])
     kssnlimg = max(kssnlim0[jj,*])
     ;
     ; convert kssnlimg & kfsnlimg to pointers
     pts3[jj]  = pt0[jj]+kssnlimg
     ptf3[jj]  = pt0[jj]+kfsnlimg
     ;
     sma_          = rad[ptt0:ptt1]
     sma_an_       = radan[ptt0:ptt1]
     plog,ll,prog,'Galaxy #'+numstr(jj+1)+' flux max(index) = '+numstr(kfsnlimg)+' max(sma_ap) = '+numstr(sma_[kfsnlimg])
     plog,ll,prog,'         '+          ' sb max(index)   = '+numstr(kssnlimg)+' max(sma_an) = '+numstr(sma_an_[kssnlimg])
  ENDFOR 
  ;
  ; loop through bands and dust correct surface fluxes and magnitudes quantities
  plog,ll,prog,'Calculating internal dust corrected surface-brightnesses...'
  FOR ii = 0, nb-1 DO BEGIN
     afact       = acalzlaw[ii]/acalzlaw[pf]      ; factor to multiply A_FUV by to get A_band
     afact       = afact[0]                       ; have to convert this to a scalar...
     aband       = -1.0*afuv*afact                ; magnitude correction this should now be an array, since afuv is an array
     sbfact      = 10.0^(-0.4*aband)
     flgtest     = mflag
     eflgtest    = emflag
     IF band[ii] EQ 'HALPHA' THEN  BEGIN 
        aband    = -0.4*aband                     ; convert to dex for Halpha
        sbfact   = 10.0^(aband)
        flgtest  = lflag
        eflgtest = elflag
     ENDIF 
     ;
     ; find good elements (kg) and elements to flag (kb)
     kg          = where(smprof[*,ii] NE flgtest AND esmproft[*,ii] NE eflgtest, nkg)
     kb          = where(smprof[*,ii] EQ flgtest OR esmproft[*,ii] EQ eflgtest, nkb)
     IF nkg GT 0 THEN BEGIN 
        sbprof0[kg,ii]   = sbfact[kg]*sbprof[kg,ii]
        esbproft0[kg,ii] = sbfact[kg]*esbproft[kg,ii]
     ENDIF 
     IF nkb GT 0 THEN BEGIN 
        ;
        ; upper limits are dust corrected, errors (which are flags) 
        ; are just copied.
        sbprof0[kb,ii]   = sbfact[kb]*sbprof[kb,ii]
        esbproft0[kb,ii] = esbproft[kb,ii]
     ENDIF 
     ;
     IF ii EQ ih THEN BEGIN 
        kg = where(esmprofc[*] NE eflgtest, nkg)
        kb = where(esmprofc[*] EQ eflgtest, nkb)
        IF nkg GT 0 THEN BEGIN 
           esbprofc0[kg] = sbfact[kg]*esbprofc[kg]
        ENDIF 
        IF nkb GT 0 THEN BEGIN
           esbprofc0[kb] = esbprofc[kb]
        ENDIF 
        kg = where(esmprofs[*] NE eflgtest, nkg)
        kb = where(esmprofs[*] EQ eflgtest, nkb)
        IF nkg GT 0 THEN BEGIN 
           esbprofs0[kg] = sbfact[kg]*esbprofs[kg]
        ENDIF 
        IF nkb GT 0 THEN BEGIN
           esbprofs0[kb] = esbprofs[kb]
        ENDIF 
     ENDIF 
  ENDFOR 
  ;
  ; convert fluxes to mag (or log flux) and set upper limits using 
  ; ssoup_cp_calcmags. 
  ; first the foreground dust corrections are kludged to 1.0
  ; since they have been done, and set zeropoints so they work
  ; with fluxes rather than count rates
  dummy = 0.0*dredf+1.0
  mag1  = 0.0*mag0
  for ii = 0, nb-1 do if ii ne ih then mag1[ii] = mag0[ii]+2.5*alog10(phfl[ii])
  ssoup_cp_calcmags, ll, band, mag1, dummy, snlimit, sbprof0, esbproft0, esbprofc0, esbprofs0, $
                     smprof0, esmproft0, esmprofc0, esmprofs0, mflag, emflag, lflag, elflag
  ;
  ; calculate surface colours
  ssoup_cp_ccolours, ll, bname, mag0, phfl, smprof0, esmproft0, smcfn0, esmcfn0, smcnr0, esmcnr0, slewr0, eslewr0, slewf0, eslewf0, $
                     mflag, emflag, lflag, elflag, clflag, cuflag
  ;
  ; reintegrate linear surface brightnesses to get fluxes
  plog,ll,prog,'integrating dust corrected surface brightness profiles to get dust corrected enclosed fluxes '
  FOR jj = 0, ngal-1 DO BEGIN 
     ptt0     = pt0[jj]    ; this saves me some typing
     ptt1     = pt1[jj]    ; this saves me some typing
     nap      = ptt1 - ptt0 + 1
     rout     = rad[ptt0:ptt1]
     rin      = [0.0,rad[ptt0:ptt1-1]]
     anarea   = !pi*(rout^2-rin^2)
     FOR ii = 0, nb-1 DO BEGIN 
        fbprof0[ptt0:ptt1,ii]   = total(anarea*sbprof0[ptt0:ptt1,ii],/cumulative)
        efbproft0[ptt0:ptt1,ii] = sqrt(total((anarea*esbproft0[ptt0:ptt1,ii])^2,/cumulative))
        IF ii EQ ih THEN BEGIN 
           efbprofc0[ptt0:ptt1] = sqrt(total((anarea*esbprofc0[ptt0:ptt1])^2,/cumulative))
           efbprofs0[ptt0:ptt1] = sqrt(total((anarea*esbprofs0[ptt0:ptt1])^2,/cumulative))
        ENDIF 
     ENDFOR 
  ENDFOR  
  ;
  ; now convert fluxes to magnitudes / log, this is done 
  ; in another call to ssoup_cp_calcmags.
  ssoup_cp_calcmags, ll, band, mag1, dummy, snlimit, fbprof0, efbproft0, efbprofc0, efbprofs0, $
                     fmprof0, efmproft0, efmprofc0, efmprofs0, mflag, emflag, lflag, elflag, /flag_trailing
  ;
  ; calculate dust corrected integrated colours
  ssoup_cp_ccolours, ll, bname, mag0, phfl, fmprof0, efmproft0, fmcfn0, efmcfn0, fmcnr0, efmcnr0, flewr0, eflewr0, flewf0, eflewf0, $
                     mflag, emflag, lflag, elflag, clflag, cuflag
  ;
  ; write dust corrected magnitude files
  plog,ll,prog,'writing calibrated dust corrected surface brightnesses and colors'
  ssoup_cp_wmagfile, ll, 2, fscalprof0, ngal, pt0, pts3, radan, smprof0, esmproft0, esmprofs0, esmprofc0, $
                     smcfn0, esmcfn0, smcnr0, esmcnr0, slewr0, eslewr0, slewf0, eslewf0
  plog,ll,prog,'writing calibrated dust corrected integrated magnitudes and colors'
  ssoup_cp_wmagfile, ll, 3, ffcalprof0, ngal, pt0, ptf3, rad, fmprof0, efmproft0, efmprofs0, efmprofc0, $
                     fmcfn0, efmcfn0, fmcnr0, efmcnr0, flewr0, eflewr0, flewf0, eflewf0
END 
