PRO ssoup_calprof_old, ll, band, photplam, ebvg, fprofs, fscalprof, ffcalprof, fscalprof0, ffcalprof0, $
                   fecntrat=fecntrat
  ;
  ; calibrate surface brightness profiles and their errors
  ; tabulate results into a single multiband output file
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
  ;  fscalprof  -> name of output calibrated surface brightness 
  ;                profiles and errors.  
  ;  ffcalprof  -> name of output calibrated enclosed flux and error 
  ;                profiles
  ;  fscalprof0 -> name of output dust corrected surface brightness a
  ;                and error profiles
  ;  ffcalprof0 -> name of output dust corrected enclosed flux and 
  ;                error profiles.
  ;  fecntrat   -> If set the the fractional error in the continuum
  ;                scaling ratio.  Otherwise the error in the scaling 
  ;                ratio is taken from the net Halpha image fits 
  ;                header.
  ;
  ; G. Meurer (ICRAR/UWA) 6/2010
  bname   = ['R', 'HALPHA', 'NUV', 'FUV']
  fmto    = '(f7.2,f8.3,f6.3,f9.3,f6.3,f6.3,f6.3,f8.3,f6.3,f8.3,f6.3,f8.3,f6.3,f8.3,f6.3,f7.3,f6.3,f7.3,f6.3)'
  hlines1 = '# Surface quantities (in annuli)'
  hlines2 = '#  sma   mu_R   err     lSHa   esky  ecnt  etot   mu_nuv err    mu_fuv err    C(f-n) err    C(n-R) err    lHa/R err    lHa/f err  '
  hlines3 = '# Surface quantities, dust corrected (in annuli)'
  hlinef1 = '# Integral quantities (in apertures)'
  hlinef2 = '#  sma   mag_R  err     lFHa   esky  ecnt  etot  mag_nuv err   mag_fuv err    C(f-n) err    C(n-R) err    lHa/R err    lHa/f err  '
  hlinef3 = '# Integral quantities, dust corrected (in apertures)'
  snlimit = 3.0
  pixsize = 1.0
  prog    = 'SSOUP_CALPROF_OLD: '
  ;
  nb      = n_elements(bname)
  ;
  plog,ll,prog,'----------------- starting '+prog+'----------------------'
  ;
  ; convert S/N limit to err limits in mag and dex
  edlim   = alog10(1.0+1.0/snlimit)    ; dex
  emlim   = 2.5*edlim
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
  pb    = [p0, p1, p2, p3]
  ;
  ; get deredden parameters
  dredf    = make_array(4, /float, value=1.0)
  IF ebvg GT 0 THEN ccm_unred, photplam, dredf, ebvg[0]
  plog,ll,prog,'will de-redden fluxes for foreground dust using the following band | wl | factor sets'
  FOR ii = 0, 3 DO plog,ll,prog,'   '+ljust(band[ii],6)+' | '+numstr(photplam[ii])+' | '+numstr(dredf[ii])
  ;
  phpl2    = photplam
  pp       = where(band EQ 'HALPHA',npp)
  IF npp GE 1 THEN phpl2[pp] = 6563.8
  acalzlaw = make_array(4, /float, value=1.0)
  calz_unred, phpl2, acalzlaw, 1.0
  acalzlaw = 2.5*alog10(acalzlaw)
  IF npp GE 1 THEN acalzlaw[pp] = acalzlaw[pp]/0.45   ; correct for emission lines
  plog,ll,prog,'will de-redden fluxes for internal dust by the following law (Calzetti): band | wl | mag/E(B-V)'
  FOR ii = 0, 3 DO plog,ll,prog,'   '+ljust(band[ii],6)+' | '+numstr(phpl2[ii])+' | '+numstr(acalzlaw[ii])
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
  filnam   = make_array(4, /string, value='')
  rad      = make_array(nrtot)
  sprof    = make_array(nrtot,4)
  esprofs  = make_array(nrtot,4)
  esprofc  = make_array(nrtot)
  esproft  = make_array(nrtot)
  fprof    = make_array(nrtot,4)
  efprofs  = make_array(nrtot,4)
  efprofc  = make_array(nrtot)
  efproft  = make_array(nrtot)
  mag0     = make_array(4, /float, value=0.0)
  phfl     = make_array(4, /float, value=0.0)
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
     ; read profiles and store
     FOR jj = 0, ngal-1 DO BEGIN 
        plog,ll,prog,'reading profile for galaxy #'+numstr(jj)
        ;pfplt_extractprof, fprofs[pp], netflag, sma, fint, dfint, ngood, nbad, sb, esb, $
        ;                   dfraw, egintsk, efintcn, lstart=lstart[jj], lend=lend[jj]
        pfplt_extractprof, fprofs[pp], 0b, sma, fint, dfint, ngood, nbad, sb, esb, $
                           dfraw, egintsk, efintcn, lstart=lstart[jj], lend=lend[jj]
        ;
        ; derive Poissonian error contribution
        IF band[ii] NE 'HALPHA' THEN BEGIN 
           ngoodt = total(ngood,/cumulative)
           ephots = sqrt(exptime*ngood*(sb*pixarea + skylev))/(exptime*ngood*pixarea*axerat[jj]) 
           ephotf = sqrt(exptime*(fint+ngood*skylev))/(exptime*fint)
        ENDIF ELSE BEGIN 
           ephots = 0.0*sb       ; **** have not yet implemented photon stats uncertainty for Halpha
           ephotf = 0.0*fint
        ENDELSE 
        ;
        ; convert to final quantities and store
        plog,ll,prog,'inclination correct and derive fractional sky errors for galaxy #'+numstr(jj)
        IF ii EQ 0 THEN rad[pt0[jj]:pt1[jj]] = sma
        sprof[pt0[jj]:pt1[jj],ii]   = sb/axerat[jj]            ; correct to face-on
        fprof[pt0[jj]:pt1[jj],ii]   = fint/axerat[jj]          ; correct to face-on
        esprofs[pt0[jj]:pt1[jj],ii] = abs(sqrt(eslev^2 + ephots^2)/sb)                         ; fractional sky  error for now
        efprofs[pt0[jj]:pt1[jj],ii] = abs(sqrt((!pi*sma^2*eslev/fint/axerat[jj])^2+ephotf^2))  ; fractional sky  error for now
     ENDFOR 
  ENDFOR
  ;
  ; some pointers to be sure
  jm        = where(bname NE 'HALPHA', njm)
  jh        = where(bname EQ 'HALPHA', njh)
  jr        = where(bname EQ 'R', njr)
  ;
  ; derive continuum subtraction and total errors
  plog,ll,prog,'calculating HALPHA continuum subtraction and total errors '
  esprofc   = abs(sprof[*,jh[0]]*ecrat/sprof[*,jh[0]])  ;fractional error for now
  efprofc   = abs(fprof[*,jh[0]]*ecrat/fprof[*,jh[0]])  ;fractional error for now
  esproft   = sqrt(esprofc^2 + esprofs[*,jh[0]]^2)
  efproft   = sqrt(efprofc^2 + efprofs[*,jh[0]]^2)
  ;
  ; flux cal, and correct for foreground dust
  ; convert errors to magnitudes and logs as needed
  plog,ll,prog,'flux calibrating and put on mag scale all continuum bands'
  sr        = dredf[pb[0]]*phfl[pb[0]]*sprof[*,pb[0]]
  esr       = esprofs[*,pb[0]]                                    ; fractional error
  sf        = dredf[pb[3]]*phfl[pb[3]]*sprof[*,pb[3]]
  esf       = esprofs[*,pb[3]]                                    ; fractional error
  fr        = dredf[pb[0]]*phfl[pb[0]]*fprof[*,pb[0]]
  efr       = efprofs[*,pb[0]]                                    ; fractional error
  ff        = dredf[pb[3]]*phfl[pb[3]]*fprof[*,pb[3]]
  eff       = efprofs[*,pb[3]]                                    ; fractional error
  FOR jj = 0, njm-1 DO BEGIN 
     kk             = jm[jj]
     sprof[*,kk]    = mag0[kk] - 2.5*alog10(dredf[pb[kk]]*sprof[*,kk])
     fprof[*,kk]    = mag0[kk] - 2.5*alog10(dredf[pb[kk]]*fprof[*,kk])
     esprofs[*,kk]  = 2.5*alog10(1.0+esprofs[*,kk])
     efprofs[*,kk]  = 2.5*alog10(1.0+efprofs[*,kk])
  ENDFOR
  ;
  plog,ll,prog,'flux calibrating and putting on log scale the HALPHA quantities'
  sha               = phfl[jh[0]]*dredf[jh[0]]*sprof[*,[jh[0]]]
  esha              = esproft                                    ; fractional error
  fha               = phfl[jh[0]]*dredf[jh[0]]*sprof[*,[jh[0]]]
  efha              = efproft                                    ; fractional error
  sprof[*,jh[0]]    = mag0[jh[0]] + alog10(dredf[pb[jh[0]]]*dredf[jh[0]]*sprof[*,[jh[0]]])
  fprof[*,jh[0]]    = mag0[jh[0]] + alog10(dredf[pb[jh[0]]]*dredf[jh[0]]*fprof[*,[jh[0]]])
  esprofs[*,jh[0]]  = alog10(1.0+esprofs[*,jh[0]])
  esprofc           = alog10(1.0+esprofc)
  esproft           = alog10(1.0+esproft)
  efprofs[*,jh[0]]  = alog10(1.0+efprofs[*,jh[0]])
  efprofc           = alog10(1.0+efprofc)
  efproft           = alog10(1.0+efproft)
  ;
  ; calculate colors and errors
  plog,ll,prog,'calculating colors and errors'
  scfn              = sprof[*,pb[3]] - sprof[*,pb[2]]
  scnr              = sprof[*,pb[2]] - sprof[*,pb[0]]
  escfn             = sqrt(esprofs[*,pb[3]]^2 + esprofs[*,pb[2]]^2)
  escnr             = sqrt(esprofs[*,pb[2]]^2 + esprofs[*,pb[0]]^2)
  fcfn              = fprof[*,pb[3]] - fprof[*,pb[2]]
  fcnr              = fprof[*,pb[2]] - fprof[*,pb[0]]
  efcfn             = sqrt(efprofs[*,pb[3]]^2 + efprofs[*,pb[2]]^2)
  efcnr             = sqrt(efprofs[*,pb[2]]^2 + efprofs[*,pb[0]]^2)
  slewr             = alog10(sha/sr)
  slewf             = alog10(sha/sf)
  eslewr            = alog10(1.0+sqrt(esha^2+esr^2))
  eslewf            = alog10(1.0+sqrt(esha^2+esf^2))
  flewr             = alog10(fha/fr)
  flewf             = alog10(fha/ff)
  eflewr            = alog10(1.0+sqrt(efha^2+efr^2))
  eflewf            = alog10(1.0+sqrt(efha^2+eff^2))
  ;
  ; get sma for annuli
  plog,ll,prog,'calculating annuli radii'
  radan  = 0.0*rad
  FOR jj = 0, ngal-1 DO BEGIN 
     nap       = pt1[jj]-pt0[jj]+1l
     sma_ap    = rad[pt0[jj]:pt1[jj]]
     sma_an    = make_array(nap, /float, value=0.0)
     sma_an[0] = sma_ap[0]/sqrt(2)
     IF nap GT 1 THEN BEGIN 
        kk0         = lindgen(nap-1)
        kk1         = kk0+1l
        sma_an[kk1] = sqrt((sma_ap[kk0]^2+sma_ap[kk1]^2)/2.)
     ENDIF 
     radan[pt0[jj]:pt1[jj]] = sma_an
  ENDFOR 
  ;
  ; derive maximum good radius
  plog,ll,prog,'deriving maximum radii'
  kssnlim  = make_array(ngal, nb, /long, value=0l)  ; limit for surface brightness
  kfsnlim  = make_array(ngal, nb, /long, value=0l)  ; limit for total flux
  kssnlimg = make_array(ngal, /long, value=0l)
  kfsnlimg = make_array(ngal, /long, value=0l)
  ;
  ; loop through galaxies
  FOR jj = 0, ngal-1 DO BEGIN 
     ;
     ; loop through bands
     FOR ii = 0, nb-1 DO BEGIN 
        IF bname[ii] EQ 'HALPHA' THEN BEGIN 
           efproft_ = efproft[pt0[jj]:pt1[jj]]
           esproft_ = esproft[pt0[jj]:pt1[jj]]
           kk       = where(efproft_ LE edlim, nkk)
           IF nkk GT 0 THEN kfsnlim[jj,ii] = max(kk) ELSE kfsnlim[jj,ii] = nring[jj]-1l
           kk       = where(esproft_ LE edlim, nkk)
           IF nkk GT 0 THEN kssnlim[jj,ii] = max(kk) ELSE kssnlim[jj,ii] = nring[jj]-1l
        ENDIF ELSE BEGIN 
           efprofs_ = efprofs[pt0[jj]:pt1[jj],ii]
           esprofs_ = esprofs[pt0[jj]:pt1[jj],ii]
           kk       = where(efprofs_ LE emlim, nkk)
           IF nkk GT 0 THEN kfsnlim[jj,ii] = max(kk) ELSE kfsnlim[jj,ii] = nring[jj]-1l
           kk       = where(esprofs_ LE emlim, nkk)
           IF nkk GT 0 THEN kssnlim[jj,ii] = max(kk) ELSE kssnlim[jj,ii] = nring[jj]-1l
        ENDELSE 
        ;print,nring[jj],kfsnlim[jj,0:3],kssnlim[jj,0:3]
     ENDFOR 
     kfsnlimg[jj] = max(kfsnlim[jj,*])
     kssnlimg[jj] = max(kssnlim[jj,*])
     sma_         = rad[pt0[jj]:pt1[jj]]
     sma_an_      = radan[pt0[jj]:pt1[jj]]
     plog,ll,prog,'Galaxy #'+numstr(jj)+' flux max(index) = '+numstr(kfsnlimg[jj])+' max(sma_ap) = '+numstr(sma_[kfsnlimg])
     plog,ll,prog,'         '+          ' sb max(index)   = '+numstr(kssnlimg[jj])+' max(sma_an) = '+numstr(sma_an_[kssnlimg])
  ENDFOR 
  ;
  ; open output file
  plog,ll,prog,'writing calibrated surface brightnesses and colors file '+fscalprof
  openw, lu, fscalprof, /get_lun
  ;
  ; write header lines
  printf,-1,hlines1
  printf,-1,hlines2
  printf,lu,hlines1
  printf,lu,hlines2
  ;
  ; write rest of output file
  FOR jj = 0, ngal-1 DO BEGIN 
     ;
     IF ngal GT 0 THEN BEGIN 
        printf,-1,'# galaxy index #'+numstr(jj+1)
        printf,lu,'# galaxy index #'+numstr(jj+1)
     ENDIF 
     FOR ii = pt0[jj], pt0[jj]+kssnlimg[jj] DO BEGIN 
        printf,-1,radan[ii],sprof[ii,0],esprofs[ii,0],sprof[ii,1],esprofs[ii,1],$
               esprofc[ii],esproft[ii],sprof[ii,2],esprofs[ii,2],sprof[ii,3],$
               esprofs[ii,3],scfn[ii],escfn[ii],scnr[ii],escnr[ii],$
               slewr[ii],eslewr[ii],slewf[ii],eslewf[ii],format=fmto
        printf,lu,radan[ii],sprof[ii,0],esprofs[ii,0],sprof[ii,1],esprofs[ii,1],$
               esprofc[ii],esproft[ii],sprof[ii,2],esprofs[ii,2],sprof[ii,3],$
               esprofs[ii,3],scfn[ii],escfn[ii],scnr[ii],escnr[ii],$
               slewr[ii],eslewr[ii],slewf[ii],eslewf[ii],format=fmto
     ENDFOR 
  ENDFOR 
  free_lun, lu
  ;
  ; now work on calibrate enclosed flux/ color proffiles
  openw, lu, ffcalprof, /get_lun
  plog,ll,prog,'writing calibrated surface brightnesses and colors file '+fscalprof
  printf,-1,hlinef1
  printf,-1,hlinef2
  printf,lu,hlinef1
  printf,lu,hlinef2
  ;
  ; write rest of output file
  FOR jj = 0, ngal-1 DO BEGIN 
     IF ngal GT 0 THEN BEGIN 
        printf,-1,'# galaxy index #'+numstr(jj+1)
        printf,lu,'# galaxy index #'+numstr(jj+1)
     ENDIF 
     FOR ii = pt0[jj], pt0[jj]+kfsnlimg[jj] DO BEGIN 
        printf,-1,rad[ii],fprof[ii,0],efprofs[ii,0],fprof[ii,1],efprofs[ii,1],$
               efprofc[ii],efproft[ii],fprof[ii,2],efprofs[ii,2],fprof[ii,3],$
               efprofs[ii,3],fcfn[ii],efcfn[ii],fcnr[ii],efcnr[ii],$
               flewr[ii],eflewr[ii],flewf[ii],eflewf[ii],format=fmto
        printf,lu,rad[ii],fprof[ii,0],efprofs[ii,0],fprof[ii,1],efprofs[ii,1],$
               efprofc[ii],efproft[ii],fprof[ii,2],efprofs[ii,2],fprof[ii,3],$
               efprofs[ii,3],fcfn[ii],efcfn[ii],fcnr[ii],efcnr[ii],$
               flewr[ii],eflewr[ii],flewf[ii],eflewf[ii],format=fmto
     ENDFOR 
  ENDFOR 
  free_lun, lu
  ;
  ; derive internal extinction vector, using IRX-beta fit of Boissier
  ; et al. (2007, ApJS, 173, 524-537; see pg528)
  ca   =  0.570
  cb   =  0.671
  cc   =  3.220
  lirx =  alog10(10.0^(ca + cb*scfn)-cc)
  afuv = -0.0333*lirx^3+0.3522*lirx^2+1.1960*lirx+0.4967  ; A(FUV) as a function of radius
  pf   = where(band EQ 'FUV', npf)
  IF npp NE 1 THEN stop, 'there should be one and only one FUV band'
  ;
  ; derive dust corrected maximum good radius
  plog,ll,prog,'deriving dust corrected maximum radii'
  kssnlim0  = make_array(ngal, nb, /long, value=0l)  ; limit for surface brightness
  kfsnlim0  = make_array(ngal, nb, /long, value=0l)  ; limit for total flux
  kssnlimg0 = make_array(ngal, /long, value=0l)
  kfsnlimg0 = make_array(ngal, /long, value=0l)
  ;
  ; loop through galaxies
  FOR jj = 0, ngal-1 DO BEGIN 
     ;
     ; pointers to how far out the reddening correction goes
     ksnf = min([kssnlim[jj,pb[2]],kssnlim[jj,pb[3]]])  
     kfnf = min([kssnlim[jj,pb[2]],kssnlim[jj,pb[3]]])
     ;
     ; loop through bands
     FOR ii = 0, nb-1 DO BEGIN 
        ;
        ; new limits is the minimum of the limit for the S/N of the band
        ; and the limit of the reddening correction
        kssnlim0[jj,ii] = min([kssnlim[jj,ii],ksnf])
        kfsnlim0[jj,ii] = min([kfsnlim[jj,ii],kfnf])
     ENDFOR 
     kfsnlimg0[jj] = max(kfsnlim0[jj,*])
     kssnlimg0[jj] = max(kssnlim0[jj,*])
     sma_          = rad[pt0[jj]:pt1[jj]]
     sma_an_       = radan[pt0[jj]:pt1[jj]]
     plog,ll,prog,'Galaxy #'+numstr(jj)+' flux max(index) = '+numstr(kfsnlimg[jj])+' max(sma_ap) = '+numstr(sma_[kfsnlimg])
     plog,ll,prog,'         '+          ' sb max(index)   = '+numstr(kssnlimg[jj])+' max(sma_an) = '+numstr(sma_an_[kssnlimg])
  ENDFOR 
  ;
  ; make arrays for reddening corrected quantities
  ; for now using same nrtot as for un-corrected quantities.  
  ; some compaction is possible, in some cases
  sprof0   = make_array(nrtot,4, /float, value=0.0)
  esprofs0 = make_array(nrtot,4, /float, value=0.0)
  fprof0   = make_array(nrtot,4, /float, value=0.0)
  efprofs0 = make_array(nrtot,4, /float, value=0.0)
  ;
  FOR ii = 0, nb-1 DO BEGIN
     fact         = acalzlaw[ii]/acalzlaw[pf]
     fact         = fact[0]                        ; have to convert this to a scalar...
     aband        = -1.0*afuv*fact                 ; this should be an array...
     IF band[ii] EQ 'HALPHA' THEN  BEGIN 
        aband = -0.4*aband
     ENDIF 
     IF band[ii] EQ 'R' THEN BEGIN 
        sr0   = sr*10.0^(-0.4*aband)
        fr0   = fr*10.0^(-0.4*aband)       ; **** not strictly correct, should reintegrate
     ENDIF 
     IF band[ii] EQ 'FUV' THEN BEGIN 
        sf0   = sf*10.0^(-0.4*aband)
        ff0   = ff*10.0^(-0.4*aband)       ; **** not strictly correct, should reintegrate
     ENDIF 
     sprof0[*,ii]   = sprof[*,ii] + aband
     fprof0[*,ii]   = fprof[*,ii] + aband
     esprofs0[*,ii] = esprofs[*,ii]        ; for now reddening corrected errors are same as uncorrected
     efprofs0[*,ii] = efprofs[*,ii]        ; should include the error in the extinction correction
  ENDFOR 
  ;
  ; derive dust corrected colors and flux ratios
  ;
  ; calculate colors and errors
  plog,ll,prog,'calculating dust corrected colors and errors'
  sha0              = 10.0^sprof0[*,jh[0]]
  esha0             = 10.0^esprofs0[*,jh[0]]-1.0                 ; fractional error
  fha0              = 10.0^fprof0[*,jh[0]]                       ; **** not strictly correct, should reintegrate
  efha0             = 10.0^efprofs0[*,jh[0]]-1.0                 ; fractional error
  esprofc0          = esprofc                                    ; same as uncorrected cont sub error 
  esproft0          = esproft                                    ; same as uncorrected cont sub error (for now)
  efprofc0          = efprofc                                    ; same as uncorrected cont sub error 
  efproft0          = efproft                                    ; same as uncorrected cont sub error (for now)
  scfn0             = sprof0[*,pb[3]] - sprof0[*,pb[2]]
  scnr0             = sprof0[*,pb[2]] - sprof0[*,pb[0]]
  escfn0            = sqrt(esprofs0[*,pb[3]]^2 + esprofs0[*,pb[2]]^2)
  escnr0            = sqrt(esprofs0[*,pb[2]]^2 + esprofs0[*,pb[0]]^2)
  fcfn0             = fprof0[*,pb[3]] - fprof0[*,pb[2]]
  fcnr0             = fprof0[*,pb[2]] - fprof0[*,pb[0]]
  efcfn0            = sqrt(efprofs0[*,pb[3]]^2 + efprofs0[*,pb[2]]^2)
  efcnr0            = sqrt(efprofs0[*,pb[2]]^2 + efprofs0[*,pb[0]]^2)
  slewr0            = alog10(sha0/sr0)
  slewf0            = alog10(sha0/sf0)
  eslewr0           = alog10(1.0+sqrt(esha^2+esr^2))
  eslewf0           = alog10(1.0+sqrt(esha^2+esf^2))
  flewr0            = alog10(fha0/fr0)
  flewf0            = alog10(fha0/ff0)
  eflewr0           = alog10(1.0+sqrt(efha^2+efr^2))
  eflewf0           = alog10(1.0+sqrt(efha^2+eff^2))
  ;
  ; open output file - surface quantities, dust corrected
  plog,ll,prog,'wrting dust correceted surface brightness profile file: '+fscalprof0
  openw, lu, fscalprof0, /get_lun
  ; 
  ; write header lines
  printf,-1,hlines3
  printf,-1,hlines2
  printf,lu,hlines3
  printf,lu,hlines2
  ;
  ; write rest of output file
  FOR jj = 0, ngal-1 DO BEGIN 
     ;
     IF ngal GT 0 THEN BEGIN 
        printf,-1,'# galaxy index #'+numstr(jj+1)
        printf,lu,'# galaxy index #'+numstr(jj+1)
     ENDIF 
     FOR ii = pt0[jj], pt0[jj]+kssnlimg0[jj] DO BEGIN 
        printf,-1,radan[ii],sprof0[ii,0],esprofs0[ii,0],sprof0[ii,1],esprofs0[ii,1],$
               esprofc0[ii],esproft0[ii],sprof0[ii,2],esprofs0[ii,2],sprof0[ii,3],$
               esprofs0[ii,3],scfn0[ii],escfn0[ii],scnr0[ii],escnr0[ii],$
               slewr0[ii],eslewr0[ii],slewf0[ii],eslewf0[ii],format=fmto
        printf,lu,radan[ii],sprof0[ii,0],esprofs0[ii,0],sprof0[ii,1],esprofs0[ii,1],$
               esprofc0[ii],esproft0[ii],sprof0[ii,2],esprofs0[ii,2],sprof0[ii,3],$
               esprofs0[ii,3],scfn0[ii],escfn0[ii],scnr0[ii],escnr0[ii],$
               slewr0[ii],eslewr0[ii],slewf0[ii],eslewf0[ii],format=fmto
     ENDFOR 
  ENDFOR 
  free_lun, lu
  ;
  ; now work on calibrate enclosed flux/ color proffiles
  openw, lu, ffcalprof0, /get_lun
  plog,ll,prog,'writing dust corrected aperture fluxes and colors '+ffcalprof0
  printf,-1,hlinef3
  printf,-1,hlinef2
  printf,lu,hlinef3
  printf,lu,hlinef2
  ;
  ; write rest of output file
  FOR jj = 0, ngal-1 DO BEGIN 
     IF ngal GT 0 THEN BEGIN 
        printf,-1,'# galaxy index #'+numstr(jj+1)
        printf,-1,'# galaxy index #'+numstr(jj+1)
     ENDIF 
     FOR ii = pt0[jj], pt0[jj]+kfsnlimg0[jj] DO BEGIN 
        printf,-1,rad[ii],fprof0[ii,0],efprofs0[ii,0],fprof0[ii,1],efprofs0[ii,1],$
               efprofc0[ii],efproft0[ii],fprof0[ii,2],efprofs0[ii,2],fprof0[ii,3],$
               efprofs0[ii,3],fcfn0[ii],efcfn0[ii],fcnr0[ii],efcnr0[ii],$
               flewr0[ii],eflewr0[ii],flewf0[ii],eflewf0[ii],format=fmto
        printf,lu,rad[ii],fprof0[ii,0],efprofs0[ii,0],fprof0[ii,1],efprofs0[ii,1],$
               efprofc0[ii],efproft0[ii],fprof0[ii,2],efprofs0[ii,2],fprof0[ii,3],$
               efprofs0[ii,3],fcfn0[ii],efcfn0[ii],fcnr0[ii],efcnr0[ii],$
               flewr0[ii],eflewr0[ii],flewf0[ii],eflewf0[ii],format=fmto
     ENDFOR 
  ENDFOR 
  free_lun, lu
END 
