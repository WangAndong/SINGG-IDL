PRO ssoup_calprof, ll, hname, photplam, ebvg, fprofs, fscalprof, ffcalprof, fscalprof0, ffcalprof0, $
                   saveprof, fecntrat=fecntrat
  ;
  ; calibrate surface brightness profiles and their errors
  ; tabulate results into a single multiband output file.
  ;
  ;  ll         -> logical unit of log file
  ;  hname      -> HIPASS name of target
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
  ;  saveprof   -> filename for profile saveset
  ;  fecntrat   -> If set the the fractional error in the continuum
  ;                scaling ratio.  Otherwise the error in the scaling 
  ;                ratio is taken from the net Halpha image fits 
  ;                header.
  ;
  ; G. Meurer (ICRAR/UWA) 6/2010
  ; G. Meurer (ICRAR/UWA) 4/2011: tidy code fix so it works on 
  ;                               multiple galaxies in the field
  ; G. Meurer (ICRAR/UWA) 5/2011: 
  ;    * now reintegrates surface brightness profiles after dust 
  ;      correction.  
  ;    * improved error model introduced for Halpha
  ;    * upper limits and flags introduced
  ;    * output data file format changed
  ;    * s/n limit set to 2.0 (from 3.0)
  ; S. Andrews (ICRAR/UWA) 1/2013
  ;    * significantly refactored
  ;    * dump results to IDL saveset
  ;    * added r20, r50, r80, kron stuff, fir model
  ;
  ; setup stuff
   COMMON bands, band, nband, bandnam, bandavail, nbandavail
   compile_opt idl2
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
  prog    = 'SSOUP_CALPROF: '
  ;
  plog,ll,prog,'----------------- starting '+prog+'----------------------'
  ;
  ; convert S/N limit to err limits in mag and dex
  edlim   = alog10(1.0+1.0/snlimit)    ; dex
  emlim   = 2.5*edlim                  ; mag
  ;
  ; read R header just to get number of galaxies
  ir = (where(bandavail eq band.R, nir))[0]
  pfplt_rdhdr, fprofs[ir], pixsize, filename, funits, fscale, fluxcal, $
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
  ; work out bandparam <-> band correspondence 
  plog,ll,prog,'working out band name correspondence'
  pb = intarr(nbandavail)
  for i=0,nband-1 do begin
      px = where(bandavail EQ band.(i), npx)
      if npx gt 0 then pb[i] = px[0]
  endfor
  ;
  ; get deredden parameters
  dredf    = make_array(nbandavail, /float, value=1.0)
  IF ebvg GT 0 THEN ccm_unred, photplam, dredf, ebvg[0]
  plog,ll,prog,'will de-redden fluxes for foreground dust using the following band | wl | factor sets'
  FOR ii = 0, nbandavail-1 DO plog,ll,prog,'   '+ljust(bandavail[ii],6)+' | '+numstr(photplam[ii])+' | '+numstr(dredf[ii])
  ;
  phpl2    = photplam
  pp       = where(bandavail EQ band.HALPHA,npp)
  IF npp GE 1 THEN phpl2[pp] = 6563.8
  acalzlaw = make_array(nbandavail, /float, value=1.0)
  calz_unred, phpl2, acalzlaw, 1.0
  acalzlaw = 2.5*alog10(acalzlaw)
  IF npp GE 1 THEN acalzlaw[pp] = acalzlaw[pp]/0.45   ; correct for emission lines
  plog,ll,prog,'will de-redden fluxes for internal dust by the following law (Calzetti): band | wl | mag/E(B-V)'
  FOR ii = 0, nbandavail-1 DO plog,ll,prog,'   '+ljust(bandavail[ii],6)+' | '+numstr(phpl2[ii])+' | '+numstr(acalzlaw[ii])
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
  filnam   = make_array(nbandavail, /string, value='')
  rad      = make_array(nrtot)
  sbprof   = make_array(nrtot,nbandavail)   ; surface brightnes linear units all bands
  esbproft = make_array(nrtot,nbandavail)   ; total error in surface brightness linear units all bands
  esbprofc = make_array(nrtot)      ; Halpha continuum subtraction error linear surf bright
  esbprofs = make_array(nrtot)      ; Halpha sky + photon error linear surf bright
  fbprof   = make_array(nrtot,nbandavail)   ; flux linear units all bands
  efbproft = make_array(nrtot,nbandavail)   ; total error in flux linear units 
  efbprofc = make_array(nrtot)      ; Halpha continuum subtraction error linear flux
  efbprofs = make_array(nrtot)      ; Halpha sky+photon error linear flux
  sbr_raw  = make_array(nrtot, /double, value=0.0) ; for raw surface brightness profiles in R band
  fbr_raw  = make_array(nrtot, /double, value=0.0) ; for raw enclosed aperture fluxes in R band     
  mag0     = make_array(nbandavail, /double, value=0.0)    ; magnitude scale zeropoint
  phfl     = make_array(nbandavail, /double, value=0.0)    ; photflam (continuum) or photflux (Halpha)
  ;
  ; initialize skylevr
  skylevr  = 0.0d
  skysigbx1 = dblarr(nbandavail)
  ;
  ; loop through bands
  ; this requires R band to execute first, otherwise it will break!
  FOR ii = ir, nbandavail-1 DO BEGIN 
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
     IF count NE 1 THEN stop, '**** should be one & only one EXPTIME in header, found '+numstr(count)
     eslev      = sxpar(hd, 'SKYSIGBX', count=count)
     IF count NE 1 THEN stop, '**** should be one & only one SKYSIGBX in header, found '+numstr(count)
     plog,ll,prog,'    SKYSIGBX = '+numstr(eslev)
     skysigbx1[ii] = eslev
     skylev     = sxpar(hd, 'SKYLEV', count=count)
     IF count NE 1 THEN stop, '**** should be one & only one SKYLEV in header, found '+numstr(count)
     IF bandavail[ii] EQ band.HALPHA THEN BEGIN 
        ;
        ; get PHOTFLUX, CNTRAT1, ECNTRAT2
        netflag = 1b
        phfl[ii] = sxpar(hd, 'PHOTFLUX', count=count)
        IF count NE 1 THEN stop, '**** should be one and only one PHOTFLUX in header, found '+numstr(count)
        mag0[ii] = alog10(phfl[ii])
        plog,ll,prog,'    PHOTFLUX = '+numstr(mag0[ii])
        crat     = sxpar(hd, 'CNTRAT1', count=count)
        IF count NE 1 THEN stop, '**** should be one and only one CNTRAT1 in header, found '+numstr(count)
        plog,ll,prog,'    CNTRAT1 = '+numstr(crat)
        IF keyword_set(fecntrat) THEN BEGIN 
           ecrat = fecntrat*crat
           plog,ll,prog,'    ECNTRAT2 = '+numstr(ecrat)+'  ( = default_ratio * CNTRAT1)'
        ENDIF ELSE BEGIN 
           ecrat = sxpar(hd, 'ECNTRAT2', count=count)
           IF count NE 1 THEN stop, '**** should be one and only one ECNTRAT2 in header, found '+numstr(count)
           plog,ll,prog,'    ECNTRAT2 = '+numstr(ecrat)
        ENDELSE 
     ENDIF ELSE BEGIN 
        ;
        ; get MAGZPT1, PHOTFLAM
        netflag  = 0b
        mag0[ii] = sxpar(hd, 'MAGZPT1', count=count)
        IF count NE 1 THEN stop, '**** should be one and only one MAGZPT1 in header, found '+numstr(count)
        plog,ll,prog,'    MAGZPT1 = '+numstr(mag0[ii])
        phfl[ii] = sxpar(hd, 'PHOTFLAM', count=count)
        IF count NE 1 THEN stop, '**** should be one and only one PHOTFLAM in header, found '+numstr(count)
        plog,ll,prog,'    PHOTFLAM = '+numstr(phfl[ii])
     ENDELSE 
     ;
     ; save R band sky level separately
     if bandavail[ii] eq band.R then skylevr = skylev
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
        IF bandavail[ii] NE band.HALPHA THEN BEGIN 
           ephotsb = sqrt((sb + skylev)/(exptime*ngood))  ; photon error per pixel
           ephotfb = (fint/frawap)*sqrt((frawap+ngoodt*skylev)/exptime)   ; photon error in ap 
        ENDIF ELSE BEGIN 
           ephotsb = sqrt((sb+crat*sbr_raw[ptt0:ptt1] + skylev + crat*skylevr)/(exptime*ngood))
           ephotfb = (fint/frawap)*sqrt((frawap+crat*fbr_raw[ptt0:ptt1]+ngoodt*(skylev+crat*skylevr))/exptime)
        ENDELSE 
        ;
        ; store raw R band surface brightness and enclosed flux 
        ; profiles; they will be needed in the Halpha noise model.
        if bandavail[ii] eq band.R then begin
           sbr_raw[ptt0:ptt1]  = sb
           fbr_raw[ptt0:ptt1]  = frawap
        endif
        ;
        ; convert to final quantities and store  
        plog,ll,prog,'inclination correct and derive fractional sky errors for galaxy #'+numstr(jj+1)
        IF bandavail[ii] eq band.R THEN rad[ptt0:ptt1] = sma*pixsize
        factsb                 = 1.0/(axerat[jj]*pixarea)
        factfb                 = 1.0/axerat[jj]
        sbprof[ptt0:ptt1,ii]   = factsb*sb                ; correct to SB per area, and correct to face-on
        fbprof[ptt0:ptt1,ii]   = factfb*fint              ; correct to face-on
        esbproft[ptt0:ptt1,ii] = factsb*sqrt(eslev^2+ephotsb^2)          ; total error = sky+photon
        efbproft[ptt0:ptt1,ii] = factfb*sqrt((npixap*eslev)^2+ephotfb^2) ; total error = sky+photon
     ENDFOR 
     ;stupid kludge to get R executing before Ha
     if ii eq ir then ii = -1 
     if ii eq ir-1 then ii = ir
  ENDFOR
  ;
  ; some pointers to be sure
  ih        = (where(bandavail EQ band.HALPHA, nih))[0]
  ;
  ; derive continuum subtraction and total fractional errors for Halpha
  plog,ll,prog,'calculating HALPHA continuum subtraction, sky+photon and total errors '
  esbprofc        = ecrat*sbprof[*,ir]                       ; continuum subtraction error
  efbprofc        = ecrat*fbprof[*,ir]                       ; continuum subtraction error
  esbprofs        = esbproft[*,ih]                           ; assign Halpha sky error to prev calculated total error
  efbprofs        = efbproft[*,ih]                           ; assign Halpha sky error to prev calculated total error
  esbproft[*,ih]  = sqrt(esbprofc^2 + esbprofs^2)            ; recalculate total error
  efbproft[*,ih]  = sqrt(efbprofc^2 + efbprofs^2)            ; recalculate total error
  ;
  ; flux cal, and correct for foreground dust
  ; convert errors to magnitudes and logs as needed
  ; do this by calling ssoup_cp_calcmags twice:
  ; once for surface brightness, the other time for enclosed fluxes
  ssoup_cp_calcmags, ll, mag0, dredf, snlimit, sbprof, esbproft, esbprofc, esbprofs, $
                     smprof, esmproft, esmprofc, esmprofs, mflag, emflag, lflag, elflag
  ssoup_cp_calcmags, ll, mag0, dredf, snlimit, fbprof, efbproft, efbprofc, efbprofs, $
                     fmprof, efmproft, efmprofc, efmprofs, mflag, emflag, lflag, elflag, /flag_trailing
  ;
  FOR ii = 0, nbandavail-1 DO BEGIN 
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
  ; other places we do this.
  ssoup_cp_ccolours, ll, mag0, phfl, smprof, esmproft, smcfn, esmcfnt, smcnr, esmcnrt, slewr, eslewrt, slewf, eslewft, $
                     mflag, emflag, lflag, elflag, clflag, cuflag
  ssoup_cp_ccolours, ll, mag0, phfl, fmprof, efmproft, fmcfn, efmcfnt, fmcnr, efmcnrt, flewr, eflewrt, flewf, eflewft, $
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
  kssnlim  = make_array(ngal, nbandavail, /long, value=0l)  ; limit for surface brightness
  kfsnlim  = make_array(ngal, nbandavail, /long, value=0l)  ; limit for total flux
  pts2     = make_array(ngal, /long, value=0l)  ; pointer to s/n limited end of sb array
  ptf2     = make_array(ngal, /long, value=0l)  ; pointer to s/n limited end of fb array
  ;
  ; loop through galaxies
  FOR jj = 0, ngal-1 DO BEGIN 
     ptt0     = pt0[jj]    ; this saves me some typing
     ptt1     = pt1[jj]    ; this saves me some typing
     ;
     ; loop through bands
     FOR ii = 0, nbandavail-1 DO BEGIN 
        efproft_ = efmproft[ptt0:ptt1,ii]
        esproft_ = esmproft[ptt0:ptt1,ii]
        IF bandavail[ii] EQ band.HALPHA THEN BEGIN 
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
  ; derive dust corrected colors and flux ratios
  ;
  ; make arrays for reddening corrected quantities
  ; for now using same nrtot as for un-corrected quantities.  
  ; some compaction is possible, in some cases
  sbprof0   = make_array(nrtot,nbandavail)   ; surface brightnes linear units all bands, corrected for internal dust
  smprof0   = make_array(nrtot,nbandavail)   ; surface brightnes log units all bands, corrected for internal dust
  fbprof0   = make_array(nrtot,nbandavail)   ; flux linear units all bands, corrected for internal dust
  fmprof0   = make_array(nrtot,nbandavail)   ; log flux all bands, corrected for internal dust
  esbproft0 = make_array(nrtot,nbandavail)
  esbprofc0 = make_array(nrtot)
  esbprofs0 = make_array(nrtot)
  efbproft0 = make_array(nrtot,nbandavail)
  efbprofc0 = make_array(nrtot)
  efbprofs0 = make_array(nrtot)
  esmproft0 = make_array(nrtot,nbandavail)
  esmprofc0 = make_array(nrtot)
  esmprofs0 = make_array(nrtot)
  efmproft0 = make_array(nrtot,nbandavail)
  efmprofc0 = make_array(nrtot)
  efmprofs0 = make_array(nrtot)
  ;
  ; calculate colors and errors
  plog,ll,prog,'calculating dust corrected colors and errors'
  ;
  ; derive internal extinction vector, using IRX-beta fit of Boissier
  ; et al. (2007, ApJS, 173, 524-537; see pg528)
  ca   =  0.570d
  cb   =  0.671d
  cc   =  3.220d
  lirx =  alog10(10.0d^(ca + cb*smcfn)-cc) ; log(FIR/FUV)
  pf   = where(bandavail EQ band.FUV, npf)
  IF npf NE 1 THEN stop, 'there should be one and only one FUV band'
  fir_model = 10.0d^lirx * sbprof[*,pf]
  bogus = where(fir_model gt 1 or fir_model le 0, /null)
  fir_model[bogus] = !values.f_nan
  afuv = -0.0333d*lirx^3+0.3522d*lirx^2+1.1960d*lirx+0.4967d  ; A(FUV) as a function of radius
  ;
  ; write output files
  plog,ll,prog,'writing calibrated surface brightnesses and colors'
  ssoup_cp_wmagfile, ll, 0, fscalprof, ngal, pt0, pts2, radan, smprof, esmproft, esmprofs, esmprofc, $
                     smcfn, esmcfnt, smcnr, esmcnrt, slewr, eslewrt, slewf, eslewft
  plog,ll,prog,'writing calibrated integrated magnitudes and colors'
  ssoup_cp_wmagfile, ll, 1, ffcalprof, ngal, pt0, ptf2, rad, fmprof, efmproft, efmprofs, efmprofc, $
                     fmcfn, efmcfnt, fmcnr, efmcnrt, flewr, eflewrt, flewf, eflewft
  ;
  ; derive dust corrected maximum good radius
  plog,ll,prog,'deriving dust corrected maximum radii'
  kssnlim0  = make_array(ngal, nbandavail, /long, value=0l)  ; limit for surface brightness
  kfsnlim0  = make_array(ngal, nbandavail, /long, value=0l)  ; limit for total flux
  pts3      = make_array(ngal, /long, value=0l)  ; convert kssnlimg to a pointer
  ptf3      = make_array(ngal, /long, value=0l)  ; convert kfsnlimg to a pointer
  ;
  ; loop through galaxies
  FOR jj = 0, ngal-1 DO BEGIN 
     ptt0     = pt0[jj]    ; this saves me some typing
     ptt1     = pt1[jj]    ; this saves me some typing
     ;
     ; pointers to how far out the reddening correction goes
     ksnf = min([kssnlim[jj,pb[2]],kssnlim[jj,pb[3]]])  
     kfnf = min([kssnlim[jj,pb[2]],kssnlim[jj,pb[3]]])
     ;
     ; loop through bands
     FOR ii = 0, nbandavail-1 DO BEGIN 
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
  FOR ii = 0, nbandavail-1 DO BEGIN
     afact       = acalzlaw[ii]/acalzlaw[pf]      ; factor to multiply A_FUV by to get A_band
     afact       = afact[0]                       ; have to convert this to a scalar...
     aband       = -1.0*afuv*afact                ; magnitude correction this should now be an array, since afuv is an array
     sbfact      = 10.0^(-0.4*aband)
     flgtest     = mflag
     eflgtest    = emflag
     IF bandavail[ii] EQ band.HALPHA THEN  BEGIN 
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
        ; upper limits are dust corrected, so are errors
        sbprof0[kb,ii]   = sbfact[kb]*sbprof[kb,ii]
        esbproft0[kb,ii] = sbfact[kb]*esbproft[kb,ii]
     ENDIF 
     ;
     IF ii EQ ih THEN BEGIN 
        kg = where(esmprofc[*] NE eflgtest, nkg)
        kb = where(esmprofc[*] EQ eflgtest, nkb)
        IF nkg GT 0 THEN BEGIN 
           esbprofc0[kg] = sbfact[kg]*esbprofc[kg]
        ENDIF 
        IF nkb GT 0 THEN BEGIN
           esbprofc0[kb] = sbfact[kb]*esbprofc[kb]
        ENDIF 
        kg = where(esmprofs[*] NE eflgtest, nkg)
        kb = where(esmprofs[*] EQ eflgtest, nkb)
        IF nkg GT 0 THEN BEGIN 
           esbprofs0[kg] = sbfact[kg]*esbprofs[kg]
        ENDIF 
        IF nkb GT 0 THEN BEGIN
           esbprofs0[kb] = sbfact[kb]*esbprofs[kb]
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
  for ii = 0, nbandavail-1 do if ii ne ih then mag1[ii] = mag0[ii]+2.5*alog10(phfl[ii])
  ssoup_cp_calcmags, ll, mag1, dummy, snlimit, sbprof0, esbproft0, esbprofc0, esbprofs0, $
                     smprof0, esmproft0, esmprofc0, esmprofs0, mflag, emflag, lflag, elflag
  ;
  ; calculate surface colours
  ssoup_cp_ccolours, ll, mag0, phfl, smprof0, esmproft0, smcfn0, esmcfn0, smcnr0, esmcnr0, slewr0, eslewr0, slewf0, eslewf0, $
                     mflag, emflag, lflag, elflag, clflag, cuflag
  ;
  r20           = dblarr(ngal, nbandavail)
  r50           = dblarr(ngal, nbandavail)
  r80           = dblarr(ngal, nbandavail)
  err20         = dblarr(ngal, nbandavail)
  err50         = dblarr(ngal, nbandavail)
  err80         = dblarr(ngal, nbandavail)
  rkron         = dblarr(ngal, nbandavail)
  errkron       = dblarr(ngal, nbandavail)
  kronmag       = dblarr(ngal, nbandavail)
  errkronmag    = dblarr(ngal, nbandavail)
  fir_model_int = dblarr(nrtot)
  ; reintegrate linear surface brightnesses to get fluxes
  plog,ll,prog,'integrating dust corrected surface brightness profiles to get dust corrected enclosed fluxes '
  FOR jj = 0, ngal-1 DO BEGIN 
     ptt0     = pt0[jj]    ; this saves me some typing
     ptt1     = pt1[jj]    ; this saves me some typing
     nap      = ptt1 - ptt0 + 1
     rout     = rad[ptt0:ptt1]
     rin      = [0.0,rad[ptt0:ptt1-1]]
     anarea   = !pi*(rout^2-rin^2)
     FOR ii = 0, nbandavail-1 DO BEGIN 
        fbprof0[ptt0:ptt1,ii]   = total(anarea*sbprof0[ptt0:ptt1,ii],/cumulative,/nan)
        efbproft0[ptt0:ptt1,ii] = sqrt(total((anarea*esbproft0[ptt0:ptt1,ii])^2,/cumulative,/nan))
        IF ii EQ ih THEN BEGIN 
           efbprofc0[ptt0:ptt1] = sqrt(total((anarea*esbprofc0[ptt0:ptt1])^2,/cumulative,/nan))
           efbprofs0[ptt0:ptt1] = sqrt(total((anarea*esbprofs0[ptt0:ptt1])^2,/cumulative,/nan))
        ENDIF
        
        ; calculate radii
        ; TODO: different from ssoup_compresults...
        halflight, fbprof0[ptt0:ptt1,ii], efbproft0[ptt0:ptt1,ii], rad[ptt0:ptt1]/1.5, rad[ptt1]/1.5, r20a, err20a, thresh=0.2
        r20[jj,ii] = 1.5*r20a & err20[jj,ii] = 1.5*err20a ; convert from pixels to arcsec
        halflight, fbprof0[ptt0:ptt1,ii], efbproft0[ptt0:ptt1,ii], rad[ptt0:ptt1]/1.5, rad[ptt1]/1.5, r50a, err50a, thresh=0.5
        r50[jj,ii] = 1.5*r50a & err50[jj,ii] = 1.5*err50a
        halflight, fbprof0[ptt0:ptt1,ii], efbproft0[ptt0:ptt1,ii], rad[ptt0:ptt1]/1.5, rad[ptt1]/1.5, r80a, err80a, thresh=0.8
        r80[jj,ii] = 1.5*r80a & err80[jj,ii] = 1.5*err80a
        kron_radius, sbprof0[ptt0:ptt1, ii], esbproft0[ptt0:ptt1, ii], rad[ptt0:ptt1]/1.5, mag0[ii], skysigbx1[ii]*phfl[ii], rk, erk, km, ekm
        rkron[jj,ii] = 1.5*rk & errkron[jj,ii] = 1.5*erk
        kronmag[jj,ii] = km & errkronmag[jj,ii] = ekm
        ;
        ; integrate FIR model
        fir_model_int[ptt0:ptt1] = total(anarea * fir_model[ptt0:ptt1], /cumulative, /nan)
     ENDFOR
  ENDFOR

  ;
  ; now convert fluxes to magnitudes / log, this is done 
  ; in another call to ssoup_cp_calcmags.
  ssoup_cp_calcmags, ll, mag1, dummy, snlimit, fbprof0, efbproft0, efbprofc0, efbprofs0, $
                     fmprof0, efmproft0, efmprofc0, efmprofs0, mflag, emflag, lflag, elflag, /flag_trailing
  ;
  ; calculate dust corrected integrated colours
  ssoup_cp_ccolours, ll, mag0, phfl, fmprof0, efmproft0, fmcfn0, efmcfn0, fmcnr0, efmcnr0, flewr0, eflewr0, flewf0, eflewf0, $
                     mflag, emflag, lflag, elflag, clflag, cuflag
  ;
  ; write dust corrected magnitude files
  plog,ll,prog,'writing calibrated dust corrected surface brightnesses and colors'
  ssoup_cp_wmagfile, ll, 2, fscalprof0, ngal, pt0, pts3, radan, smprof0, esmproft0, esmprofs0, esmprofc0, $
                     smcfn0, esmcfn0, smcnr0, esmcnr0, slewr0, eslewr0, slewf0, eslewf0
  plog,ll,prog,'writing calibrated dust corrected integrated magnitudes and colors'
  ssoup_cp_wmagfile, ll, 3, ffcalprof0, ngal, pt0, ptf3, rad, fmprof0, efmproft0, efmprofs0, efmprofc0, $
                     fmcfn0, efmcfn0, fmcnr0, efmcnr0, flewr0, eflewr0, flewf0, eflewf0
     
  ; dump the points into an IDL saveset
  ; note: due to IDL being... IDL we cannot get more than 6 digits of accuracy because
  ; IDL uses 32 bit floats. These give about 7dp accuracy. Exercise: try
  ; IDL> a = 9999.90
  ; IDL> print,a,format='(F)'
  plog,ll,prog,"making IDL saveset"
  profilestr = { $
      radius                      : ptr_new(!null),     $ ; galactocentric radius along major axis (arcsec)
      radius_int                  : ptr_new(!null),     $ ; like radius, but for integrated quantities
      r20                         : dblarr(nbandavail), $ ; radius enclosing 20% of flux (dust corrected)
      err20                       : dblarr(nbandavail), $ ; error in above
      r50                         : dblarr(nbandavail), $ ; radius enclosing 50% of flux (dust corrected)
      err50                       : dblarr(nbandavail), $ ; error in above
      r80                         : dblarr(nbandavail), $ ; radius enclosing 80% of flux (dust corrected)
      err80                       : dblarr(nbandavail), $ ; error in above
      rkron                       : dblarr(nbandavail), $ ; Kron radius (dust corrected)
      errkron                     : dblarr(nbandavail), $ ; error in above
      kronmag                     : dblarr(nbandavail), $ ; Kron magnitude (dust corrected)
      errkronmag                  : dblarr(nbandavail), $ ; error in above
      mprof                       : ptrarr(nbandavail), $ ; surface brightness profile, corresponds (like everything below) 1-1 with bname
      err_mprof                   : ptrarr(nbandavail), $ ; total error in mprof
      mprof_int                   : ptrarr(nbandavail), $ ; integrated (enclosed) surface brightness profile
      err_mprof_int               : ptrarr(nbandavail), $ ; total error in mprof_int
      mprof_dustcor               : ptrarr(nbandavail), $ ; surface brightness profile corrected for dust
      err_mprof_dustcor           : ptrarr(nbandavail), $ ; total error in mprof_dustcor
      mprof_dustcor_int           : ptrarr(nbandavail), $ ; integrated surface (enclosed) brightness profile corrected for dust
      err_mprof_dustcor_int       : ptrarr(nbandavail), $ ; total error in mprof_dustcor_int
      col_fuv_nuv                 : ptr_new(!null),     $ ; FUV-NUV color index (uncorrected for dust)
      err_col_fuv_nuv             : ptr_new(!null),     $ ; error in FUV-NUV color index
      col_nuv_r                   : ptr_new(!null),     $ ; NUV-R color index (uncorrected for dust)
      err_col_nuv_r               : ptr_new(!null),     $ ; error in FUV-NUV color index
      log_ha_r                    : ptr_new(!null),     $ ; log(flux in Ha/flux in R)
      err_log_ha_r                : ptr_new(!null),     $ ; error in above
      log_ha_fuv                  : ptr_new(!null),     $ ; log(flux in Ha/flux in FUV)
      err_log_ha_fuv              : ptr_new(!null),     $ ; error in above
      col_fuv_nuv_int             : ptr_new(!null),     $ ; integrated FUV-NUV color index (uncorrected for dust)
      err_col_fuv_nuv_int         : ptr_new(!null),     $ ; error in FUV-NUV color index
      col_nuv_r_int               : ptr_new(!null),     $ ; integrated NUV-R color index (uncorrected for dust)
      err_col_nuv_r_int           : ptr_new(!null),     $ ; error in FUV-NUV color index
      log_ha_r_int                : ptr_new(!null),     $ ; integrated log(flux in Ha/flux in R)
      err_log_ha_r_int            : ptr_new(!null),     $ ; error in above
      log_ha_fuv_int              : ptr_new(!null),     $ ; integrated log(flux in Ha/flux in FUV)
      err_log_ha_fuv_int          : ptr_new(!null),     $ ; error in above
      col_fuv_nuv_dustcor         : ptr_new(!null),     $ ; FUV-NUV color index (corrected for dust)
      err_col_fuv_nuv_dustcor     : ptr_new(!null),     $ ; error in FUV-NUV color index
      col_nuv_r_dustcor           : ptr_new(!null),     $ ; NUV-R color index (corrected for dust)
      err_col_nuv_r_dustcor       : ptr_new(!null),     $ ; error in FUV-NUV color index
      log_ha_r_dustcor            : ptr_new(!null),     $ ; log(flux in Ha/flux in R)
      err_log_ha_r_dustcor        : ptr_new(!null),     $ ; error in above
      log_ha_fuv_dustcor          : ptr_new(!null),     $ ; log(flux in Ha/flux in FUV)
      err_log_ha_fuv_dustcor      : ptr_new(!null),     $ ; error in above
      col_fuv_nuv_dustcor_int     : ptr_new(!null),     $ ; integrated FUV-NUV color index (corrected for dust)
      err_col_fuv_nuv_dustcor_int : ptr_new(!null),     $ ; error in FUV-NUV color index
      col_nuv_r_dustcor_int       : ptr_new(!null),     $ ; integrated NUV-R color index (corrected for dust)
      err_col_nuv_r_dustcor_int   : ptr_new(!null),     $ ; error in FUV-NUV color index
      log_ha_r_dustcor_int        : ptr_new(!null),     $ ; integrated dust corrected log(flux in Ha/flux in R)
      err_log_ha_r_dustcor_int    : ptr_new(!null),     $ ; error in above
      log_ha_fuv_dustcor_int      : ptr_new(!null),     $ ; integrated dust corrected log(flux in Ha/flux in FUV)
      err_log_ha_fuv_dustcor_int  : ptr_new(!null),     $ ; error in above
      fir_model                   : ptr_new(!null),     $ ; fir model flux
      fir_model_int               : ptr_new(!null),     $ ; integrated fir model flux
      fir_model_r20               : 0.0d,               $ ; fir model r20
      fir_model_r50               : 0.0d,               $ ; fir model r50
      fir_model_r80               : 0.0d,               $ ; fir model r80
     ; fir_model_rkron             : 0.0d,               $ ; fir model kron radius
     ; fir_model_kronmag           : 0.0d                $ ; fir model kron mag
  }
  allprofiles = replicate(profilestr, ngal) ; one entry for each galaxy 
  ; populate structure
  for i=0,ngal-1 do begin
      a = max([pts2[i],ptf2[i],pts3[i],ptf3[i]])
      allprofiles[i].radius     = ptr_new(radan[pt0[i] : a ])
      allprofiles[i].radius_int = ptr_new(rad[pt0[i]   : a ])
      allprofiles[i].r20        = r20[i,*]
      allprofiles[i].err20      = err20[i,*]
      allprofiles[i].r50        = r50[i,*]
      allprofiles[i].err50      = err50[i,*]
      allprofiles[i].r80        = r80[i,*]
      allprofiles[i].err80      = err80[i,*]
      allprofiles[i].rkron      = rkron[i,*]
      allprofiles[i].errkron    = errkron[i,*]
      allprofiles[i].kronmag    = kronmag[i,*]
      allprofiles[i].errkronmag = errkronmag[i,*]
      for j=0,nbandavail-1 do begin
          allprofiles[i].mprof[j]                 = ptr_new(smprof[pt0[i]    : pts2[i], j])
          allprofiles[i].err_mprof[j]             = ptr_new(esmproft[pt0[i]  : pts2[i], j])
          allprofiles[i].mprof_int[j]             = ptr_new(fmprof[pt0[i]    : ptf2[i], j])
          allprofiles[i].err_mprof_int[j]         = ptr_new(efmproft[pt0[i]  : ptf2[i], j])
          allprofiles[i].mprof_dustcor[j]         = ptr_new(smprof0[pt0[i]   : pts3[i], j])
          allprofiles[i].err_mprof_dustcor[j]     = ptr_new(esmproft0[pt0[i] : pts3[i], j])
          allprofiles[i].mprof_dustcor_int[j]     = ptr_new(fmprof0[pt0[i]   : ptf3[i], j])
          allprofiles[i].err_mprof_dustcor_int[j] = ptr_new(efmproft0[pt0[i] : ptf3[i], j])
      endfor
      allprofiles[i].col_fuv_nuv                 = ptr_new(smcfn[pt0[i]   : a])
      allprofiles[i].err_col_fuv_nuv             = ptr_new(esmcfnt[pt0[i] : a])
      allprofiles[i].col_fuv_nuv_int             = ptr_new(fmcfn[pt0[i]   : a])
      allprofiles[i].err_col_fuv_nuv_int         = ptr_new(efmcfnt[pt0[i] : a])
      allprofiles[i].col_fuv_nuv_dustcor         = ptr_new(smcfn0[pt0[i]  : a])
      allprofiles[i].err_col_fuv_nuv_dustcor     = ptr_new(esmcfn0[pt0[i] : a])
      allprofiles[i].col_fuv_nuv_dustcor_int     = ptr_new(fmcfn0[pt0[i]  : a])
      allprofiles[i].err_col_fuv_nuv_dustcor_int = ptr_new(efmcfn0[pt0[i] : a])
      allprofiles[i].col_nuv_r                   = ptr_new(smcnr[pt0[i]   : a])
      allprofiles[i].err_col_nuv_r               = ptr_new(esmcnrt[pt0[i] : a])
      allprofiles[i].col_nuv_r_int               = ptr_new(fmcnr[pt0[i]   : a])
      allprofiles[i].err_col_nuv_r_int           = ptr_new(efmcnrt[pt0[i] : a])
      allprofiles[i].col_nuv_r_dustcor           = ptr_new(smcnr0[pt0[i]  : a])
      allprofiles[i].err_col_nuv_r_dustcor       = ptr_new(esmcnr0[pt0[i] : a])
      allprofiles[i].col_nuv_r_dustcor_int       = ptr_new(fmcnr0[pt0[i]  : a])
      allprofiles[i].err_col_nuv_r_dustcor_int   = ptr_new(efmcnr0[pt0[i] : a])
      allprofiles[i].log_ha_r                    = ptr_new(slewr[pt0[i]   : a])
      allprofiles[i].err_log_ha_r                = ptr_new(eslewrt[pt0[i] : a])
      allprofiles[i].log_ha_fuv                  = ptr_new(slewf[pt0[i]   : a])
      allprofiles[i].err_log_ha_fuv              = ptr_new(eslewft[pt0[i] : a])
      allprofiles[i].log_ha_r_int                = ptr_new(flewr[pt0[i]   : a])
      allprofiles[i].err_log_ha_r_int            = ptr_new(eflewrt[pt0[i] : a])
      allprofiles[i].log_ha_fuv_int              = ptr_new(flewf[pt0[i]   : a])
      allprofiles[i].err_log_ha_fuv_int          = ptr_new(eflewft[pt0[i] : a])
      allprofiles[i].log_ha_r_dustcor            = ptr_new(slewr0[pt0[i]  : a])
      allprofiles[i].err_log_ha_r_dustcor        = ptr_new(eslewr0[pt0[i] : a])
      allprofiles[i].log_ha_fuv_dustcor          = ptr_new(slewf0[pt0[i]  : a])
      allprofiles[i].err_log_ha_fuv_dustcor      = ptr_new(eslewf0[pt0[i] : a])
      allprofiles[i].log_ha_r_dustcor_int        = ptr_new(flewr0[pt0[i]  : a])
      allprofiles[i].err_log_ha_r_dustcor_int    = ptr_new(eflewr0[pt0[i] : a])
      allprofiles[i].log_ha_fuv_dustcor_int      = ptr_new(flewf0[pt0[i]  : a])
      allprofiles[i].err_log_ha_fuv_dustcor_int  = ptr_new(eflewf0[pt0[i] : a])
      allprofiles[i].fir_model                   = ptr_new(fir_model[pt0[i] : pt1[i]])
      allprofiles[i].fir_model_int               = ptr_new(fir_model_int[pt0[i] : pt1[i]])
      ; calculate FIR model stuff
      halflight, *(allprofiles[i].fir_model_int), 0, rad[pt0[i]:pt1[i]]/1.5, rad[pt1[i]]/1.5, temp1, a, thresh=0.2
      allprofiles[i].fir_model_r20 = 1.5*temp1
      halflight, *(allprofiles[i].fir_model_int), 0, rad[pt0[i]:pt1[i]]/1.5, rad[pt1[i]]/1.5, temp1, a, thresh=0.5
      allprofiles[i].fir_model_r50 = 1.5*temp1      
      halflight, *(allprofiles[i].fir_model_int), 0, rad[pt0[i]:pt1[i]]/1.5, rad[pt1[i]]/1.5, temp1, a, thresh=0.8
      allprofiles[i].fir_model_r80 = 1.5*temp1
      ;kron_radius, *(allprofiles[i].fir_model), dblarr(pt1[i]-pt0[i]+1), rad[pt0[i]:pt1[i]]/1.5, rad[pt1[i]]/1.5, 0, 1.0e-19, temp1, temp2
      ;allprofiles[i].fir_model_rkron = 1.5*temp1
      ;allprofiles[i].fir_model_kronmag = temp2
  endfor
  bname = bandavail
  save,filename=saveprof,hname,bname,allprofiles
  plog,ll,prog,'finished '
END

