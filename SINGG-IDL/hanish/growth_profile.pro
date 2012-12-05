PRO growth_profile,unit,ellipsenum,cenX,cenY,axerat,pa,del_pa, $
                   sky,a_f,a_s,a_c,binA,Flux,Ngood,Nbad, $
                   Flux_good,SB,SBsig,Fout,CNTSIG=cntsig,FOVER=Fover
; Creates output profile from the Curve of Growth logic, for each
; object in the image.
; INPUTS
; unit         File I/O unit number
; ellipsenum   Array index, from 0:(N-1)
; CenX,CenY    Centerpoint of ellipse, in pixels
; axerat       a/b
; pa,del_pa    Local position angle, and conversion to world coordinates
; sky[3]       Image sky [value, sig_box, sig_pixel]
; a_f,a_s,a_c  Semi-major axes for full flux, sky, and SNR=3 apertures
; binA[N]      Values of a for each bin
; Flux[N]      Flux within each bin
; Ngood[N]     Number of good pixels within each bin
; Nbad[N]      Number of bad pixels within each bin
; Flux_good[N] Flux from good pixels within each bin
; Fout[3]      Flux outside the apertures (value, sigsky, sigcnt)
; OPTIONAL INPUTS:
; cntsig       Array of flux uncertainties due to continuum scaling
;                error, for continuum-subtracted images only

  num_bins = N_ELEMENTS(binA)

  Ffinal_f = SPLINE(binA,Flux,[a_f])
  Ffinal_s = SPLINE(binA,Flux,[a_s])
  IF KEYWORD_SET(cntsig) THEN BEGIN
    Fcnt_f = SPLINE(binA,cntsig,[a_f])
    Fcnt_s = SPLINE(binA,cntsig,[a_s])
  ENDIF

  eps = 1E-8
  overflag = 0b
  IF KEYWORD_SET(cntsig) THEN BEGIN
    outflag = (ABS(Fout[0]) GT eps AND Fout[1] GT eps AND Fout[2] GT eps)
    IF KEYWORD_SET(Fover) THEN overflag = (ABS(Fover[0]) GT eps AND Fover[1] GT eps AND Fover[2] GT eps AND Fover[3] GT 0)
  ENDIF ELSE BEGIN
    outflag = (ABS(Fout[0]) GT eps AND Fout[1] GT eps)
    IF KEYWORD_SET(Fover) THEN overflag = (ABS(Fover[0]) GT eps AND Fover[1] GT eps AND Fover[3] GT 0)
  ENDELSE

  PRINTF,unit,"#"
  PRINTF,unit,"# GALINDEX = "+STRING(ellipsenum+1)+" / Source index (1 - N)"
  PRINTF,unit,"# XCENTER = "+STRING(cenX)+" / X(center) [pixels]"
  PRINTF,unit,"# YCENTER = "+STRING(cenY)+" / Y(center) [pixels]"
  PRINTF,unit,"# AXERAT = "+STRING(axerat)+" / Axial ratio (a/b)"
  PRINTF,unit,"# PA_IMAGE = "+STRING(pa)+" / Position angle, counterclockwise from Up [deg]"
  PRINTF,unit,"# PA = "+STRING(pa-del_pa)+" / Position angle, from N towards E [deg]"

  PRINTF,unit,"# SKYLEV = "+STRING(sky[0])+" / Sky level [DN/sec]"
  PRINTF,unit,"# ERR_SKY_PIXEL = "+STRING(sky[2])+" / Sky RMS (pixel-to-pixel) [DN/sec]"
  PRINTF,unit,"# ERR_SKY_BOX = "+STRING(sky[1])+" / Sky RMS (box-to-box) [DN/sec]"

  PRINTF,unit,"# FLUXRAD_S = "+STRING(a_s)+" / Sky aperture radius [pixels]"
  PRINTF,unit,"# FLUXRAD_F = "+STRING(a_f)+" / Flux aperture radius [pixels]"
  PRINTF,unit,"# FLUXRAD_C = "+STRING(a_c)+" / Radius where SNR=3 [pixels]"

  PRINTF,unit,"# FLUX_S = "+STRING(Ffinal_s)+" / Flux within sky aperture [DN/sec]"
  PRINTF,unit,"# FLUX_F = "+STRING(Ffinal_f)+" / Flux within flux aperture [DN/sec]"
  IF outflag THEN PRINTF,unit,"# FLUX_T = "+STRING(Ffinal_f+Fout[0])+" / Total flux [DN/sec]"
  IF overflag THEN PRINTF,unit,"# FLUX_O = "+STRING(Fover[0])+" / Override flux [DN/sec]"

  PRINTF,unit,"# ERR_FLUX_S_SKY = "+STRING(sky[1]*!pi*(a_s^2.0)/axerat)+ $
              " / Uncertainty in FLUX_S due to sky [DN/sec]"
  PRINTF,unit,"# ERR_FLUX_F_SKY = "+STRING(sky[1]*!pi*(a_f^2.0)/axerat)+ $
              " / Uncertainty in FLUX_F due to sky [DN/sec]"
  IF outflag THEN BEGIN
    flsigsky_t = sky[1]*!pi*(a_f^2.0)/axerat + Fout[1]
    PRINTF,unit,"# ERR_FLUX_T_SKY = "+STRING(flsigsky_t)+ $
                " / Uncertainty in FLUX_T due to sky [DN/sec]"
  ENDIF
  IF overflag THEN PRINTF,unit,"# ERR_FLUX_O_SKY = "+STRING(Fover[1])+ $
                " / Uncertainty in FLUX_O due to sky [DN/sec]"

  IF KEYWORD_SET(cntsig) THEN BEGIN
    PRINTF,unit,"# ERR_FLUX_S_CONT = "+STRING(Fcnt_s)+ $
                " / Uncertainty in FLUX_S due to continuum [DN/sec]"
    PRINTF,unit,"# ERR_FLUX_F_CONT = "+STRING(Fcnt_f)+ $
                " / Uncertainty in FLUX_F due to continuum [DN/sec]"
    IF outflag THEN BEGIN
      PRINTF,unit,"# ERR_FLUX_T_CONT = "+STRING(Fcnt_f + Fout[2])+ $
                  " / Uncertainty in FLUX_T due to continuum [DN/sec]"
    ENDIF
    IF overflag THEN PRINTF,unit,"# ERR_FLUX_O_CONT = "+STRING(Fover[2])+ $
                  " / Uncertainty in FLUX_O due to continuum [DN/sec]"
  ENDIF

; Find the half-light radii on the fly
  Ferrsky = sky[1]*!pi*(binA^2)/axerat
  halflight,Flux,FerrSky,binA,a_f, Acent_f,Aerr_f_sky

  PRINTF,unit,"# RE_F = "+STRING(Acent_f)+" / Flux aperture half-light radius [pixels]"
  IF outflag THEN BEGIN
    halflight,Flux,FerrSky,binA,a_f, Acent_t,Aerr_t_sky,Fout=[Fout[0],Fout[1]]
    PRINTF,unit,"# RE_T = "+STRING(Acent_t)+" / Total flux half-light radius [pixels]"
  ENDIF
  PRINTF,unit,"# ERR_RE_F_SKY = "+STRING(Aerr_f_sky)+" / Uncertainty in RE_F due to sky [pixels]"
  IF outflag THEN BEGIN
    PRINTF,unit,"# ERR_RE_T_SKY = "+STRING(Aerr_t_sky)+" / Uncertainty in RE_T due to sky [pixels]"
  ENDIF
  
  IF KEYWORD_SET(cntsig) THEN BEGIN
    halflight,Flux,cntsig,binA,a_f, Acent_f_cont,Aerr_f_cont
    PRINTF,unit,"# ERR_RE_F_CONT = "+STRING(Aerr_f_cont)+" / Uncertainty in RE_F due to continuum [pixels]"
    IF outflag THEN BEGIN
      halflight,Flux,cntsig,binA,a_f, Acent_t_cont,Aerr_t_cont,Fout=[Fout[0],Fout[2]]
      PRINTF,unit,"# ERR_RE_T_CONT = "+STRING(Aerr_t_cont)+" / Uncertainty in RE_T due to continuum [pixels]"
    ENDIF
  ENDIF

  PRINTF,unit,"# SE_F = "+STRING(Ffinal_f/(2.0*!pi*(Acent_f^2)))+ $
             " / Face-on surface brightness within RE_F [DN/sec/pixel]"
  IF outflag THEN BEGIN
    IF Acent_t GT 0 THEN se_t = (Ffinal_f+Fout[0])/(2.0*!pi*(Acent_t^2)) $
                    ELSE se_t = -999.0
    PRINTF,unit,"# SE_T = "+STRING(se_t)+ $
             " / Face-on surface brightness within RE_T [DN/sec/pixel]"
  ENDIF
  IF overflag THEN BEGIN
    se_o = Fover[0] / Fover[3]
    PRINTF,unit,"# SE_O = "+STRING(se_o)+ $
             " / Face-on surface brightness override area [DN/sec/pixel]"
  ENDIF

; Now, plot the table of data.
  IF KEYWORD_SET(cntsig) THEN BEGIN
    PRINTF,unit,"#  a[pix]  F(interior)     dF       N(good) N(bad) SurfBright  Sigma(SB)    dF(raw)  Sigma(F,sky)  Sigma(F,cntrat)"
    fmt = '(F9.4," ",2(g12.6," "),2(I6," "),4(g12.6," "),g12.6)'
  ENDIF ELSE BEGIN
    PRINTF,unit,"#  a[pix]  F(interior)     dF       N(good) N(bad) SurfBright  Sigma(SB)    dF(raw)  Sigma(F,sky)"
    fmt = '(F9.4," ",2(g12.6," "),2(I6," "),3(g12.6," "),g12.6)'
  ENDELSE

  PRINTF,unit,"# all Flux and Surface Brightness values are in [DN/sec]"
;  IF Ngood[0] GT 0 THEN BEGIN 
    IF KEYWORD_SET(cntsig) THEN BEGIN
      PRINTF,unit,binA[0],Flux[0],Flux[0],Ngood[0],Nbad[0], $
                  SB[0],SBsig[0],Flux_good[0], $
                  sky[1]*!pi*(binA[0]^2.0)/axerat,cntsig[0], $
                  FORMAT=fmt
    ENDIF ELSE BEGIN
      PRINTF,unit,binA[0],Flux[0],Flux[0],Ngood[0],Nbad[0], $
                  SB[0],SBsig[0],Flux_good[0], $
                  sky[1]*!pi*(binA[0]^2.0)/axerat, $
                  FORMAT=fmt
    ENDELSE
;  ENDIF
  FOR ii=1,num_bins-1 DO BEGIN
    IF KEYWORD_SET(cntsig) THEN BEGIN
      PRINTF,unit,binA[ii],Flux[ii],(Flux[ii]-Flux[ii-1]),Ngood[ii],Nbad[ii], $
                  SB[ii],SBsig[ii],Flux_good[ii], $
                  sky[1]*!pi*(binA[ii]^2.0)/axerat,cntsig[ii], $
                  FORMAT=fmt
    ENDIF ELSE BEGIN
      PRINTF,unit,binA[ii],Flux[ii],(Flux[ii]-Flux[ii-1]),Ngood[ii],Nbad[ii], $
                  SB[ii],SBsig[ii],Flux_good[ii], $
                  sky[1]*!pi*(binA[ii]^2.0)/axerat, $
                  FORMAT=fmt
    ENDELSE
  ENDFOR

  RETURN

END
