PRO ssoup_cp_ccolours, ll, mag0, phfl, mg, emg, colfn, ecolfn, colnr, ecolnr, $
                       lgewr, elgewr, lgewf, elgewf, mflag, emflag, lflag, elflag, clflag, cuflag
  ;
  ; Calculate some defined colours from magnitudes or log(fluxes).
  ; This routine is called by ssoup_calprof(_new)
  ;
  ;  ll     -> logical unit for log file
  ;  mag0   -> ABmagnitude zeropoints for the above bands (except for
  ;            HALPHA
  ;  phfl   -> PHOTFLAM (continuum) and PHOTFLUX (HALPHA) for the bands
  ;  mg     -> array of magnitudes and log fluxes (e.g. radial profiles)
  ;  emg    -> errors on mg.  When these have certain flag values, the
  ;            relevent colours and their errors are also flagged.
  ;  colfn  <- FUV-NUV colour
  ;  ecolfn <- error in FUV-NUV colour
  ;  colnr  <- NUV-R colour
  ;  ecolnr <- error in NUV-R colour
  ;  lgewr  <- log(EW_Halpha) = log(F_Halpha/f_R)
  ;  elgewr <- error in lgewr
  ;  lgewf  <- log(F_Halpha/f_FUV)
  ;  elgewf <- error in log(F_Halpha/f_FUV)
  ;  mflag  -> magnitude flag
  ;  emflag -> mag error flag
  ;  lflag  -> log flux ratio flag
  ;  elflag -> log flux ratio error flag
  ;  clflag -> color or log(flux ratio) is a lower limit
  ;  cuflaf -> color or log(flux ratio) is an upper limit
  ;
  ; G. Meurer (ICRAR/UWA) 4/2011
  ; S. Andrews (ICRAR/UWA) refactored
  ;
  ;mflag   =  99.999  ; magnitude flag value
  ;emflag  =   9.999  ; error mag flag
  ;lflag   = -99.999  ; log flux flux value
  ;elflag  =   9.999  ; error log flag
  prog    = 'SSOUP_CP_CCOLOURS: '
  ;
  COMMON bands, band, nband, bandnam, bandavail, nbandavail, combo, ncombo 
  ;
  plog,ll,prog,'----------------- starting '+prog+'----------------------'
  ;
  plog,ll,prog,'working out band name correspondence'
  p0      = where(bandavail EQ band.R, np0)
  p1      = where(bandavail EQ band.HALPHA, np1)
  p2      = where(bandavail EQ band.NUV, np2)
  p3      = where(bandavail EQ band.FUV, np3)
  IF (np0 NE 1 OR np1 NE 1 OR np2 NE 1 OR np3 NE 1) THEN stop,prog+'band names not correct: ',bname
  p0      = p0[0]
  p1      = p1[0]
  p2      = p2[0]
  p3      = p3[0]
  ;
  ; get dimensions of magnitude array so that the output 
  ; arrays can be made
  sz       = size(mg)
  IF (sz[0] NE 2 AND sz[2] NE nbandavail) THEN stop,prog+'input error array does not have the right dimensions'
  nn       = sz[1]  ; number of elements for each band
  colfn    = make_array(nn, /float, value=0.0) ; empty array
  ecolfn   = colfn                             ; copy that empty array...
  colnr    = colfn
  ecolnr   = colfn
  lgewr    = colfn
  elgewr   = colfn
  lgewf    = colfn
  elgewf   = colfn
  ;
  ; fuv-nuv
  ; **** these blocks are repetitive and hence the block should be put
  ; into a subroutine -> ssoup_cp_ccol0.pro which I started
  kg       = where(mg[*,p3] ne mflag and mg[*,p2] ne mflag and emg[*,p3] NE emflag AND emg[*,p2] NE emflag, nkg)   
  kl       = where(mg[*,p3] ne mflag and mg[*,p2] ne mflag and emg[*,p2] ne emflag and emg[*,p3] eq emflag, nkl)
  ku       = where(mg[*,p3] ne mflag and mg[*,p2] ne mflag and emg[*,p3] ne emflag and emg[*,p2] eq emflag, nku)
  kb       = where((mg[*,p3] eq mflag or mg[*,p2] eq mflag) or (emg[*,p3] EQ emflag and emg[*,p2] EQ emflag), nkb)
  IF nkg GT 0 THEN BEGIN 
     colfn[kg]   = mg[kg,p3] - mg[kg,p2]
     ecolfn[kg]  = sqrt(emg[kg,p3]^2 + emg[kg,p2]^2)
  ENDIF 
  if nkl gt 0 then begin
     colfn[kl]   = mg[kl,p3] - mg[kl,p2]
     ecolfn[kl]  = clflag
  endif
  if nku gt 0 then begin
     colfn[ku]   = mg[ku,p3] - mg[ku,p2]
     ecolfn[ku]  = cuflag
  endif
  IF nkb GT 0 THEN BEGIN 
     colfn[kb]   = mflag
     ecolfn[kb]  = emflag
  ENDIF 
  ;
  ; nuv-r
  kg       = where(mg[*,p2] ne mflag and mg[*,p0] ne mflag and emg[*,p2] NE emflag AND emg[*,p0] NE emflag, nkg)
  kl       = where(mg[*,p2] ne mflag and mg[*,p0] ne mflag and emg[*,p0] ne emflag and emg[*,p2] eq emflag, nkl)
  ku       = where(mg[*,p2] ne mflag and mg[*,p0] ne mflag and emg[*,p2] ne emflag and emg[*,p0] eq emflag, nku)
  kb       = where((mg[*,p2] eq mflag or mg[*,p0] eq mflag) or (emg[*,p2] EQ emflag and emg[*,p0] EQ emflag), nkb)
  ;kg       = where(emg[*,p2] NE emflag AND emg[*,p0] NE emflag, nkg)
  ;kb       = where(emg[*,p2] EQ emflag OR emg[*,p0] EQ emflag, nkb)
  IF nkg GT 0 THEN BEGIN 
     colnr[kg]   = mg[kg,p2] - mg[kg,p0]
     ecolnr[kg]  = sqrt(emg[kg,p2]^2 + emg[kg,p0]^2)
  ENDIF 
  if nkl gt 0 then begin
     colnr[kl]   = mg[kl,p2] - mg[kl,p0]
     ecolnr[kl]  = clflag
  endif
  if nku gt 0 then begin
     colnr[ku]   = mg[ku,p2] - mg[ku,p0]
     ecolnr[ku]  = cuflag
  endif
  IF nkb GT 0 THEN BEGIN 
     colnr[kb]   = mflag
     ecolnr[kb]  = emflag
  ENDIF 
  ;
  ; EW(Halpha)
  ; * note here and for Halpha/FUV the trick is that the mag array for 
  ;   Halpha is actually log(F_halpha), while the ABmag calib. of the 
  ;   continuum bands must be backed out to get f_lambda.
  kg       = where(mg[*,p0] ne mflag and mg[*,p1] ne lflag and emg[*,p0] NE emflag AND emg[*,p1] NE elflag, nkg)
  ku       = where(mg[*,p0] ne mflag and mg[*,p1] ne lflag and emg[*,p0] ne emflag and emg[*,p1] eq elflag, nku)
  kl       = where(mg[*,p0] ne mflag and mg[*,p1] ne lflag and emg[*,p1] ne elflag and emg[*,p0] eq emflag, nkl)
  kb       = where((mg[*,p0] eq mflag or mg[*,p1] eq lflag) or (emg[*,p1] EQ elflag and emg[*,p0] EQ emflag), nkb)
  ;kg       = where(emg[*,p0] NE emflag AND emg[*,p1] NE elflag, nkg)
  ;kb       = where(emg[*,p0] EQ emflag OR emg[*,p1] EQ elflag, nkb)
  IF nkg GT 0 THEN BEGIN 
     lgewr[kg]   = mg[kg,p1] - 0.4*(mag0[p0] - mg[kg,p0]) - alog10(phfl[p0])
     elgewr[kg]  = sqrt(emg[kg,p1]^2+(0.4*emg[kg,p0])^2)
  ENDIF
  if nkl gt 0 then begin
     lgewr[kl]   = mg[kl,p1] - 0.4*(mag0[p0] - mg[kl,p0]) - alog10(phfl[p0])
     elgewr[kl]  = clflag
  endif
  if nku gt 0 then begin
     lgewr[ku]   = mg[ku,p1] - 0.4*(mag0[p0] - mg[ku,p0]) - alog10(phfl[p0])
     elgewr[ku]  = cuflag
  endif
  IF nkb GT 0 THEN BEGIN 
     lgewr[kb]   = lflag
     elgewr[kb]  = elflag
  ENDIF 
  ;
  ; Halpha/FUV
  kg       = where(mg[*,p3] ne mflag and mg[*,p1] ne lflag and emg[*,p3] NE emflag AND emg[*,p1] NE elflag, nkg)
  ku       = where(mg[*,p3] ne mflag and mg[*,p1] ne lflag and emg[*,p3] ne emflag and emg[*,p1] eq elflag, nku)
  kl       = where(mg[*,p3] ne mflag and mg[*,p1] ne lflag and emg[*,p1] ne elflag and emg[*,p3] eq emflag, nkl)
  kb       = where((mg[*,p3] eq mflag or mg[*,p1] eq lflag) or (emg[*,p1] EQ elflag and emg[*,p3] EQ emflag), nkb)
  ;kg       = where(emg[*,p3] NE emflag AND emg[*,p1] NE elflag, nkg)
  ;kb       = where(emg[*,p3] EQ emflag OR emg[*,p1] EQ elflag, nkb)
  IF nkg GT 0 THEN BEGIN 
     lgewf[kg]   = mg[kg,p1] - 0.4*(mag0[p3] - mg[kg,p3]) - alog10(phfl[p3])
     elgewf[kg]  = sqrt(emg[kg,p1]^2+(0.4*emg[kg,p3])^2)
  ENDIF
  if nkl gt 0 then begin
     lgewf[kl]   = mg[kl,p1] - 0.4*(mag0[p3] - mg[kl,p3]) - alog10(phfl[p3])
     elgewf[kl]  = clflag
  endif
  if nku gt 0 then begin
     lgewf[ku]   = mg[ku,p1] - 0.4*(mag0[p3] - mg[ku,p3]) - alog10(phfl[p3])
     elgewf[ku]  = cuflag
  endif
  IF nkb GT 0 THEN BEGIN 
     lgewf[kb]   = lflag
     elgewf[kb]  = elflag
  ENDIF 
END
