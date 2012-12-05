PRO calc_sfd0,H0=H0,MONTE=monte,PS=ps,DATA=data,SILENT=silent,CURVE=curve, $
              HIMF=himf,STARBURST=starburst,Z0=z0,PHOT=phot
; Script to calculate the local star formation density from the
; observations in the SINGG database.
;
; OPTIONAL INPUTS:
; H0            Hubble constant override.  Default is 70 km/s/Mpc
; /monte        Monte Carlo mode.  Using "monte=1000" will override
;                 the number of iterations (default = 10k)
; /ps           Output as postscript files instead of using the plot window
; /data         Dump extra information to the screen
; /silent       Disables the print and plotting statements
; /curve        Use a Schechter curve instead of the spline interpolation
; himf          Which HI mass function to use:
;                 0: New values with Mould distance model [default]
;                 1: New values (Zwaan2005)
;                 2+: Old values (Zwaan2003)
; /starburst    The plots will show ONLY the starburst contributions
; /z0           Extrapolates each galaxy to z = 0
; /phot         Use only photometric (or patched) galaxies.

  psflag = KEYWORD_SET(ps)
  dataflag = KEYWORD_SET(data)
  printflag = 1b - KEYWORD_SET(silent)
  starflag = KEYWORD_SET(starburst)
  photflag = KEYWORD_SET(phot)

  IF NOT KEYWORD_SET(himf) THEN himf = -1

  IF NOT KEYWORD_SET(H0) THEN H0 = 70.0
  hipassH0 = 70.0

  headerdb = "proc3_header"
  refdb = "singg_sample"
  fluxdb = "singg_flux"
  derdb = "singg_derived"

  dbopen,headerdb,0
  dbext,-1,"TARGET, IMTYPE, MAGZPT1,RUNID,PHOTQUAL,CNTRAT1,CNTRAT2", $
            headobj,himtype,magzpt1,hrun,photqual, cntrat1,cntrat2
  dbclose,dummy
  headobj = update_name(headobj)
;; cntrat1,cntrat2 just for debug purposes.

  dbopen,refdb,0
  dbext,-1,"NAME,   DISTANCE,LOGMHI,VHEL,VLG,EBV,W50", $
            refname,distance,logMHI,vhel,vlg,EBV,W50
  dbclose,dummy
  refname = update_name(refname)

  rind = WHERE(STRTRIM(refname,2) EQ 'J0242+00',count)
  logMHI[rind] = 9.4523

  rs = SORT(logMHI)

  refname = refname[rs]
  distance = distance[rs]
  logMHI = logMHI[rs]
  vhel = vhel[rs]
  vlg = vlg[rs]
  ebv = ebv[rs]
  w50 = w50[rs]

; If we're using either of the HIPASS HIMFs, use distances derived
; from the local group model, which is what was used to generate that
; HIMF.  If himf=2, we're using the Mould HIMF, and the DISTANCE in
; the database was derived from the Mould model.
  IF himf GE 1 THEN BEGIN
    tempdist = (vlg/hipassH0)
; logMHI is also derived using the Mould distance model.  If the local
; group distance is larger than the Mould distance, the logMHI derived
; from measured HI flux would be larger.
    logMHI = logMHI + 2.0*ALOG10(tempdist/distance)
    distance = tempdist
  ENDIF

  dbopen,fluxdb,0
  IF photflag THEN flind = good_flux(/phot)+1 ELSE flind = good_flux()+1
  dbext,flind,"OBJECT, FILENAME,IMTYPE,FILTER,GALINDEX,NUMGALS,EW50_T_BRT", $
               fluxobj,ffile,  fimtype,ffilt, fluxgal, fnum,   flew50
  dbext,flind,"FLUX_T_ISO,ERR_FLUX_T_SKY_ISO,ERR_FLUX_T_CONT_ISO,SE_F_ISO", $
               fluxval,   flsigsky,          flsigcnt,           flsb
  dbext,flind,"AXERAT,FLUXRAD_F_ISO,FLUX_SCALE,RUNID", $
               axerat,fluxrad,flux_scale,frun
  dbext,flind,"FLAG_O,FLUX_O,ERR_FLUX_O_SKY,ERR_FLUX_O_CONT,SE_O", $
               flag_o,flux_o,flsigsky_o,flsigcnt_o,se_o
  dbclose,dummy
  fluxobj = update_name(fluxobj)

  fluxobj = STRTRIM(fluxobj,2)
  ffile = STRTRIM(ffile,2)
  fimtype = STRTRIM(fimtype,2)
  ffilt = STRTRIM(ffilt,2)
  frun = STRTRIM(frun,2)
  flag_o = STRTRIM(flag_o,2)

;;  dbopen,derdb,0
;;  dbext,-1,"NAME, OBJECT,FILTER_R,FILTER_N,LOGMHI, DISTANCE,LOGFHA", $
;;            dname,derobj,dfilt_r, dfilt_n ,dlogMHI,derdist, logfha
;;  dbclose,dummy
;;  derobj = update_name(derobj)
  
; In SR1, we also tossed out J2022-31 and J0403-01.

; For each mass bin, calculate the mean (SFR/M(HI)) for each galaxy,
; multiply by the HI mass function to get SFR for that total area,
; then integrate.

; Divide mass into logarithmic bins.

; Note: graph in HIPASS goes 6-11 with the HIMF dropping at 10.5, but
; The data points themselves are at 7-11.

; Our mass bin distribution:
;;  xbin = [0.0,4.0,5.0,6.0,7.0,8.0,8.5,9.0,9.5,10.0,10.5,11.0,11.5]
  xbin = [0.0,5.0,6.9,7.5,8.0,8.2,8.4,8.6,8.8,9.0,9.2,9.4,9.6,9.8,10.0,10.2,10.4,10.6,10.8,11.0,11.5]

  num_bins = N_ELEMENTS(xbin) - 1

  avemass = FLTARR(num_bins)
; Set some default values; these will be overriden with the averages later.
  FOR ii = 0,num_bins-1 DO BEGIN
    avemass[ii] = (xbin[ii]+xbin[ii+1])/2.0
  ENDFOR

; Convert the Vhel values to local extinction correction factors,
; based on what wavelength that V corresponds to.
  vlight = 3.0E5 ; km/s
  wave_n = 6562.8 * (1.0+(vhel/vlight))
  nunred = wave_n*0.0 + 1.0
  ccm_unred,wave_n,nunred,ebv
; "unred" will be a multiplicative factor; unred*flux = true flux
  wave_r = wave_n*0.0 + 6507.46
  runred = wave_n*0.0 + 1.0
  ccm_unred,wave_r,runred,ebv

  IF KEYWORD_SET(monte) THEN BEGIN
; If someone uses /monte its value will be 1.  Switch to the default.
    IF monte EQ 1 THEN num_monte = 10000 $ ; 10 thousand iterations
                  ELSE num_monte = LONG(monte)
    IF printflag THEN PRINT,"Monte carlo logic initiated: ",num_monte," iterations"
  ENDIF ELSE num_monte = 0
; We actually have two separate sets of monte carlo'ing; the HIMF
; loop, and the Corrections loop.  We'll assume both start off at the
; same number, but this can vary in setup_himf

  rho = 0.d0 ; rho is our final value, the star formation per volume per year
  rho_SB = 0.d0 ; rho for starburst galaxies only
  mass_rho = 0.d0
  Mdyn_rho = DBLARR(4) ; good, mult, inc, total
  num_rho = 0.d0
  Rlum_rho = 0.d0
  Rlum_rho_nodust = 0.d0
  niirho = 0.d0
  nodustrho = 0.d0
  nounredrho = 0.d0
  drho_HIMF = DBLARR(2,9) ; in addition to the usual four, add rhoHI,tgas,rhodyn,rhoN, and EW
  drhoNII_rand = DBLARR(2,4)
  drhoDust_rand = DBLARR(2,4)
  drhosky = DBLARR(2,4)
  drhocnt = DBLARR(2,4)
  drhocal = DBLARR(2,4)
  drhoNII_sys = DBLARR(2,4)
  drhodust_sys = DBLARR(2,4)
  drho_samp = DBLARR(2,4)
  drho_sampplus = 0.d0
  drho_sampminus = 0.d0
  num_sb = 0

  imf = 1.26D41 ; the conversion factor from erg/s to solar masses per year
  al10 = DOUBLE(ALOG(10.0))
  cm_Mpc = 3.0857D24; cm per Mpc
  G = (1.0 / 232.47) ; (km/s)^2 pc Msolar^-1

  hmonte = num_monte
  IF KEYWORD_SET(curve) THEN BEGIN
    setup_himf,theta0,refmass,alpha,num_monte=hmonte,H0=H0,/curve,HIMF=himf
  ENDIF ELSE BEGIN
    setup_himf,theta0,refmass,alpha,num_monte=hmonte,H0=H0,HIMF=himf
  ENDELSE
; hmonte can now be different than num_monte.  If himf=-1, hmonte=100,
; otherwise it's the number of valid HIMF combinations.

  IF hmonte GT 0 THEN rho_m_theta = DBLARR(7,hmonte)

  IF num_monte GT 0 THEN BEGIN
    rho_m_NII = DBLARR(2,num_monte)
    rho_m_Dust = DBLARR(2,num_monte)
    erhosky = DBLARR(4,num_monte)
    erhocnt = DBLARR(4,num_monte)
  ENDIF ELSE BEGIN
    erhosky = DBLARR(4)
    erhocnt = DBLARR(4)
  ENDELSE

; Find Mdyn
  q0 = 0.20
  sini = SQRT((1.0 - (1.0/axerat)^2)/(1.0 - q0^2))

  delrho = DBLARR(num_bins)
  mindelrho = DBLARR(num_bins)
  maxdelrho = DBLARR(num_bins)
  mindelRlum = DBLARR(num_bins)
  maxdelRlum = DBLARR(num_bins)
  meansfr = DBLARR(num_bins)
  dmeansfr = DBLARR(num_bins,2)
  meantgas = DBLARR(num_bins)
  dmeantgas = DBLARR(num_bins,2)
  meannodustsfr = DBLARR(num_bins)
  dmeannodustsfr = DBLARR(num_bins,2)
  meantgas_nodust = DBLARR(num_bins)
  dmeantgas_nodust = DBLARR(num_bins,2)
  relative = DBLARR(num_bins)
  lumin = DOUBLE(0.0)
  delR = DBLARR(num_bins)

  max_gals = N_ELEMENTS(fluxobj)/3 ; Should be more exact, but this'll be close

  gal_name = STRARR(max_gals)
  gal_rho = DBLARR(max_gals)
  dgal_rho = DBLARR(max_gals)
  gal_rho_nodust = DBLARR(max_gals)
  gal_mass = DBLARR(max_gals)
  gal_mdyn = DBLARR(max_gals)
  gal_Rlum = DBLARR(max_gals)
  gal_Nlum = DBLARR(max_gals)
  gal_Rrho = DBLARR(max_gals)
  gal_Rrho_nodust = DBLARR(max_gals)
  gal_num = DBLARR(max_gals)
  gal_ew = DBLARR(max_gals)
  gal_tgas = DBLARR(max_gals)
  gal_tgas_nodust = DBLARR(max_gals)
  gal_himf = DBLARR(max_gals)
  gal_bin = INTARR(max_gals)
  gal_Rfrac = DBLARR(max_gals)
  gal_Nfrac = DBLARR(max_gals)
  numsfr = 0
  num_in_bin = INTARR(num_bins)
  sfrref = INTARR(num_bins)-1
  sbgal = INTARR(max_gals)

  used = INTARR(N_ELEMENTS(refname))

  IF dataflag THEN BEGIN
    datafile = !singgdir+"/sfrd.dat"
    OPENW,dunit,datafile,/GET_LUN
    spawn,"date",datestring
    PRINTF,dunit,"# Updated on "+STRTRIM(datestring,2)
    PRINTF,dunit,""
    PRINTF,dunit,"#   Name    gals   MHI   HIMF   L_Ha   L_R     tgas   EW[A] Dust  NII   SBs"
  ENDIF

  FOR ii = 0,num_bins-1 DO BEGIN
    xmin = xbin[ii]
    xmax = xbin[ii+1]

    totsfr = 0.d0
    etotsfr = 0.d0
    totnodustsfr = 0.d0
    etotnodustsfr = 0.d0
    totmass = 0.d0
    totdelrho = 0.d0
    totdelrho_NII = DBLARR(2,4)
    totdelrho_dust = DBLARR(2,4)
    totdelrho_cal = DBLARR(2,4)
    totdelrho_SB = 0.d0
    totdelmass = 0.d0
    totdelMdyn = DBLARR(3) ; good, patch_mult, patch_inc
    totdelnum = 0.d0
    totRlum = 0.d0
    totRlum_nodust = 0.d0
    totdelrho_noNII = 0.d0
    totdelrho_noDust = 0.d0
    totdelrho_nounred = 0.d0
    totdelrhoplus = 0.d0
    totdelrhominus = 0.d0

    IF hmonte GT 0 THEN m_theta = DBLARR(7,hmonte) ; variation in rho due to HIMF

    IF num_monte GT 0 THEN BEGIN
      totskydelrho = DBLARR(4,num_monte)
      totcntdelrho = DBLARR(4,num_monte)
      m_NII = DBLARR(2,num_monte) ; variation in l_Ha',l_Ha due to NII (random)
      m_Dust = DBLARR(2,num_monte) ; variation in l_R,l_ha due to Dust (random)
    ENDIF ELSE BEGIN
      totskydelrho = DBLARR(4)
      totcntdelrho = DBLARR(4)
    ENDELSE

    refindex = WHERE(logMHI GE xmin+2.0*ALOG10(H0/hipassH0) AND $
                     logMHI LT xmax+2.0*ALOG10(H0/hipassH0) AND $
                     STRMID(refname,0,1) EQ "J", refcount)

; refindex is the number in refdb.  For each object, find it in the fluxdb.
    IF refcount GT 0 THEN BEGIN
; The object is in the basic SINGG sample set, now find it in the
; other two databases.
      FOR jj = 0,refcount-1 DO BEGIN
        Sindex = WHERE(STRTRIM(fluxobj,2) EQ STRTRIM(refname[refindex[jj]],2) AND $
                       STRTRIM(fimtype,2) EQ "net", Scount)

; If fluxcount > 1 then that probably means multiple galaxies were in the
; same object image, or that multiple filters were used.  This'll be
; checked further down, as they're being used.

; Find the R-band entry corresponding to this entry:
        Rindex = WHERE(STRTRIM(fluxobj,2) EQ STRTRIM(refname[refindex[jj]],2) AND $
                       STRTRIM(fimtype,2) EQ "cont",Rcount)

        headcount = 0
        headScount = 0
        IF Rcount GT 0 AND Scount GT 0 THEN BEGIN
          headRindex = WHERE(STRMID(headobj,0,8) EQ STRMID(refname[refindex[jj]],0,8) AND $
;;photqual LT 0.1 AND $
                             'Run'+STRTRIM(hrun,2) EQ STRTRIM(frun[Rindex[0]],2) AND $
                             STRTRIM(himtype,2) EQ "cont", headcount)
          headSindex = WHERE(STRMID(headobj,0,8) EQ STRMID(refname[refindex[jj]],0,8) AND $
                             'Run'+STRTRIM(hrun,2) EQ STRTRIM(frun[Sindex[0]],2) AND $
                             STRTRIM(himtype,2) EQ "net",hscount)
        ENDIF

        IF headcount GT 1 AND printflag THEN BEGIN
; If headcount > 1 then that probably means multiple filters were
; used.  Or, it could mean that we observed in multiple runs and
; the good_derived equivalent didn't clarify which one to use.
;;          PRINT,"WARNING: Multiple continuum header matches for object ",refname[refindex[jj]]
;;forprint,headobj[headRindex]+' ',hrun[headRindex]
; IF NINT(hrun[headRindex[0]]) NE 6 AND NINT(hrun[headRindex[0]]) GT 3 THEN headcount = 0 ;; patch to ignore any multiple-matches outside of 1/2/3/6
        ENDIF

        IF headScount GT 1 AND printflag THEN BEGIN
; If headcount > 1 then that probably means multiple filters were
; used.  Or, it could mean that we observed in multiple runs and
; the good_derived equivalent didn't clarify which one to use.
;;          PRINT,"WARNING: Multiple net-image header matches for object ",refname[refindex[jj]]
;;forprint,headobj[headSindex]+' ',hrun[headSindex]
; IF NINT(hrun[headSindex[0]]) NE 6 AND NINT(hrun[headSindex[0]]) GT 3 THEN headcount = 0 ;; patch to ignore any multiple-matches outside of 1/2/3/6
        ENDIF

; Scount/Rcount=0 means the galaxy wasn't in our flux DB.  headcount=0
; means the galaxy wasn't in our header DB.  If the galaxy is missing
; from the reference DB, it'll be dropped even if it's in the others.

        IF Scount GT 0 AND headcount GT 0 AND Rcount GT 0 THEN BEGIN
          used[refindex[jj]] = 1

IF hscount EQ 0 then begin
  print,'mismatch ',refname[refindex[jj]]
  print,Rcount,Scount,headcount
forprint,headobj[headRindex]+' ',hrun[headRindex]
  stop ;;
endif

; luminosity is in erg/s, so multiply flux by area?
          distcm = DOUBLE(distance[refindex[jj]]) * cm_Mpc / (H0 / hipassH0)
          delmass = 10.0^logMHI[refindex[jj]] / (H0/hipassH0)^2.0

; Find the initial mass function value at this mass.  If the inputs
; are arrays (i.e., Monte Carlo), the outputs will as well.
; If you're not using Monte Carlo, sigtheta will be propagated
; uncertainties (with correlation issues); if you ARE, it'll be
; the standard deviation for this value of theta.

          theta = calc_theta(ALOG10(delmass),theta0[*,0:hmonte], $
                  refmass[*,0:hmonte],alpha[*,0:hmonte],sigtheta)

; Instead of doing the simple method, derive the corrections using
; fluxes from the database.
          correction,refname[refindex[jj]],niicorrection,dustcorrection, $
                     niierr,dusterr,MONTE=num_monte ; ,RUN=frun[Rindex[0]],FILT=[,] ;;;
          niicorr = 1.d0 - niicorrection
          dustcorr = 10.0^(dustcorrection/2.5)

          galind = fluxgal[Sindex]
; correct for stellar absorption by increasing Halpha flux by 4%
          oflag = STRTRIM(flag_o[Sindex],2) EQ "T"
          IF MAX(oflag) THEN BEGIN
            gal_sb   = se_o[Sindex] * 1.04
            gal_flux = flux_o[Sindex] * 1.04
            gal_flsigsky = flsigsky_o[Sindex] * 1.04
            gal_flsigcnt = flsigcnt_o[Sindex] * 1.04
          ENDIF ELSE BEGIN
            gal_sb   = flsb[Sindex] * 1.04
            gal_flux = fluxval[Sindex] * 1.04
            gal_flsigsky = flsigsky[Sindex] * 1.04
            gal_flsigcnt = flsigcnt[Sindex] * 1.04
          ENDELSE
          gal_ewarr = flew50[Sindex] * 1.04

          gal_Rflux = fluxval[Rindex]
          gal_Rind = fluxgal[Rindex]
          gal_Rflsigsky = flsigsky[Rindex]
          gal_Rflsigcnt = flsigcnt[Rindex]

          IF KEYWORD_SET(z0) THEN BEGIN
            z = DOUBLE(distance[refindex[jj]] * H0 / (3.0E5))
            zfact = DOUBLE((1+z)^(-3.0))
            gal_flux = gal_flux * zfact
            gal_Rflux = gal_Rflux * zfact
            gal_flsigsky = gal_flsigsky * zfact
            gal_flsigcnt = gal_flsigcnt * zfact
            gal_Rflsigsky = gal_Rflsigsky * zfact
            gal_Rflsigcnt = gal_Rflsigcnt * zfact
          ENDIF

          num_sources = MAX(galind)-MIN(galind)+1
          delsfr = 0.0
          delsfr_SB = 0.0
          delsfr_NII_rand = 0.0
          delsfr_NII_rand_nodust = 0.0
          delsfr_dust_rand = 0.0
          delsfr_dust_rand_R = 0.0
          edelsfr_NII_sys = DBLARR(2)
          edelsfr_NII_sys_nodust = DBLARR(2)
          edelsfr_dust_sys = DBLARR(2)
          edelsfr_dust_sys_R = DBLARR(2)
          delRlum = 0.0
          delRlum_nodust = 0.0
          delsfr_noNII = 0.0
          delsfr_noDust = 0.0
          delsfr_nounred = 0.0
          edelrho_sky = DBLARR(4)
          edelrho_cnt = DBLARR(4)
          sbtally = 0
;          sbtext = ""
          Mdyn = DBLARR(3)
          dyn = INTARR(num_sources)

          FOR kk = MIN(galind),MAX(galind) DO BEGIN
; Search for every object with galaxy index "kk", and average any ones
; you find.  The only way you'll get multiple matches is if this is
; one of the few galaxies where two R-band or two narrow-band sets
; were made.
            gal_arr = kk-MIN(galind)
            delbase = distcm^2.d0*4.d0*!pi/imf

            index = WHERE(galind EQ kk,galcount)
            index2 = WHERE(gal_Rind EQ kk,gal2count)
            IF galcount GT 0 AND gal2count GT 0 THEN BEGIN
              IF galcount GT 1 OR gal2count GT 1 THEN BEGIN
                IF printflag THEN BEGIN
runind = frun[Sindex]
                  PRINT,"Multiple matches for object ",refname[refindex[jj]], galcount,gal2count
;;forprint,'  '+refname[refindex[jj]]+':S'+STRTRIM(STRING(kk+1),2)+'  '+frun[Sindex]
                ENDIF
              ENDIF
; Combine all the correction factors into one 4-element array [R',R,Ha',Ha]
              corr = [runred[refindex[jj]]*[1.0,SQRT(dustcorr[gal_arr,0])], $
                      niicorr[gal_arr,0]*nunred[refindex[jj]]*[1.0,dustcorr[gal_arr,0]]]

              delsfr = delsfr + delbase * corr[3] * DOUBLE(gal_flux[index[0]])
              IF num_monte GT 0 THEN BEGIN
                delsfr_NII_rand = delsfr_NII_rand + delbase * $
                         dustcorr[gal_arr,0] * niicorr[gal_arr,1:num_monte] * $
                         nunred[refindex[jj]] * DOUBLE(gal_flux[index[0]])
                delsfr_NII_rand_nodust = delsfr_NII_rand_nodust + delbase * $
                         niicorr[gal_arr,1:num_monte] * nunred[refindex[jj]]* $
                         DOUBLE(gal_flux[index[0]])
                delsfr_dust_rand_R = delsfr_dust_rand_R + delbase*imf * $
                         SQRT(dustcorr[gal_arr,1:num_monte]) * $
                         runred[refindex[jj]] * DOUBLE(gal_Rflux[index2[0]])
                delsfr_dust_rand = delsfr_dust_rand + delbase * corr[2] * $
                         dustcorr[gal_arr,1:num_monte]*DOUBLE(gal_flux[index[0]])
              ENDIF

              edelsfr_NII_sys = edelsfr_NII_sys + delbase * $
                       dustcorr[gal_arr,0] * niierr[gal_arr,0:1] * $
                       nunred[refindex[jj]] * DOUBLE(gal_flux[index[0]])
              edelsfr_NII_sys_nodust = edelsfr_NII_sys_nodust + delbase * $
                       niierr[gal_arr,0:1] * nunred[refindex[jj]] * $
                       DOUBLE(gal_flux[index[0]])
              edelsfr_dust_sys_R = edelsfr_dust_sys_R + delbase*imf * $
                       (SQRT(dustcorr[gal_arr,0]+dusterr[gal_arr,0:1])-SQRT(dustcorr[gal_arr,0])) * $
                       runred[refindex[jj]] * DOUBLE(gal_Rflux[index2[0]])
              edelsfr_dust_sys = edelsfr_dust_sys + delbase * $
                       dusterr[gal_arr,0:1]*corr[2]*DOUBLE(gal_flux[index[0]])

              delRlum = delRlum + distcm^2.d0*4.d0*!pi * corr[1] * $
                        DOUBLE(gal_Rflux[index2[0]])
              delRlum_nodust = delRlum_nodust + distcm^2.d0*4.d0*!pi* $
                               corr[0]*DOUBLE(gal_Rflux[index2[0]])

              delsfr_noNII = delsfr_noNII + delbase * dustcorr[gal_arr,0] * $
                             nunred[refindex[jj]] * DOUBLE(gal_flux[index[0]])
              delsfr_noDust = delsfr_noDust + delbase * corr[2] * $
                              DOUBLE(gal_flux[index[0]])
              delsfr_nounred = delsfr_nounred + delbase * dustcorr[gal_arr,0] * $
                               niicorr[gal_arr,0] * DOUBLE(gal_flux[index[0]])

              edelrho_sky = edelrho_sky + (delbase*imf) * corr * $
                       DOUBLE([gal_Rflsigsky[index2[0]]*[1.0,1.0], $
                               gal_flsigsky[index[0]]*[1.0,1.0]])
              edelrho_cnt = edelrho_cnt + (delbase*imf) * corr * $
                       DOUBLE([gal_Rflsigcnt[index2[0]]*[1.0,1.0], $
                               gal_flsigcnt[index[0]]*[1.0,1.0]]) $
*cntrat1[headSindex[0]]/cntrat2[headSindex[0]]

              IF axerat[Rindex[index2[0]]] LT 1.25 THEN dyn[gal_arr] = 2
              IF num_sources GT 1 THEN dyn[gal_arr] = 1
              IF dyn[gal_arr] GT 0 THEN BEGIN
                mabs_r0_t = magzpt1[headRindex[0]] - $
                            5.0*(ALOG10(distance[refindex[jj]]) + 5.0) - $
                            2.5*ALOG10(gal_Rflux[index2[0]]*corr[1]/flux_scale[Rindex[index2[0]]])
                logLR = (mabs_r0_t-4.64)/(-2.5) ; in log(solar units)
;                Mdyntemp = 10.d0^(0.794315*logLR + 2.82384)
                Mdyntemp = 10.d0^(0.875414*logLR + 2.17559)
;; replace this with an on-the-fly fit.

;Mdyntemp = 10.d0^(1.25806*logMHI[refindex[jj]] - 1.36303)
              ENDIF ELSE BEGIN
                vcirc = 0.5 * W50[refindex[jj]] / (sini[Rindex[index2[0]]] < 1.0) ; in km/s
                dynrad = fluxrad[Rindex[index2[0]]] * 1E3 /3600.0 * !dtor * $
                       distance[refindex[jj]] * 1E3
                Mdyntemp = (vcirc^2 * dynrad / G) ; in solar masses
              ENDELSE
              Mdyn[dyn[gal_arr]] = Mdyn[dyn[gal_arr]] + Mdyntemp
            ENDIF

; Now, the part that only counts starburst galaxies
; Old definition: surface brightness
;            sbindex = WHERE(galind EQ kk AND gal_sb*corr[3] GT 3.9E-16,sbcount)
; New definition: equivalent width
; If it's dust-corrected, it should be corr[3]/corr[1]
            sbindex = WHERE(galind EQ kk AND gal_ewarr*corr[3]/corr[1] GE 50.0,sbcount)
;            sbindex = WHERE(galind EQ kk AND gal_ewarr GE 50.0,sbcount)
;; Need a minimum cutoff.  Too many of the :S7 and :S8 meet this.

            IF sbcount GT 0 THEN BEGIN
; If we're in Starburst mode, we want a list of WHICH objects are starbursts.
              num_sb = num_sb + 1
              delsfr_SB = delsfr_SB + delbase * corr[3] * $
                          DOUBLE(MEAN(gal_flux[sbindex]))
              sbtally = sbtally + 2^(gal_arr)
;              sbtext = sbtext +" "+STRTRIM(STRING(gal_arr+1),2)
            ENDIF

            IF fnum[Sindex[0]] GT 1 AND dataflag THEN BEGIN
; Write the line to the data file for each galaxy.  Yes, this goes
; BEFORE the "total" line.
               sourcename = STRTRIM(refname[refindex[jj]],2)+":S"+STRTRIM(STRING(gal_arr+1),2)+"   "
               dha = (delbase*imf) * corr[3] * DOUBLE(MEAN(gal_flux[index]))
               dR = (delbase*imf) * corr[1] * DOUBLE(MEAN(gal_Rflux[index2]))
               IF sbcount GT 0 THEN sbtext2 = "Y" ELSE sbtext2 = "N"
               tgas = (2.3E-9*delmass/(dha/imf) < 9999.99) ; cap it.
               PRINTF,dunit,numsfr+1,sourcename,ALOG10(delmass),ALOG10(theta[0]), $
                          ALOG10(dha),ALOG10(dR),tgas,dha/dR, $
                          dustcorr[gal_arr,0],niicorr[gal_arr,0],sbtext2, $
                          FORMAT='(I3," ",A12," ",4(F6.3," "),F7.2," ",F6.2," ",2(F5.3," "),A1)'
            ENDIF

          ENDFOR

          totsfr = totsfr + delsfr*theta[0]
          sfrerr = SQRT(edelsfr_NII_sys^2 + edelsfr_dust_sys^2 + (edelrho_sky[3]^2 + edelrho_cnt[3]^2)/(imf^2))
          etotsfr = etotsfr + sfrerr*theta[0]
          totnodustsfr = totnodustsfr + delsfr_noDust*theta[0]
          etotnodustsfr = etotnodustsfr + SQRT(edelsfr_NII_sys_nodust^2 + (edelrho_sky[2]^2 + edelrho_cnt[2]^2)/(imf^2))*theta[0]
          totmass = totmass + delmass*theta[0]
          deltarho = theta[0] * delsfr

; Monte carlo logic arrays for better estimation of uncertainties
          IF hmonte GT 0 THEN BEGIN
            m_theta[0,*] = m_theta[0,*] + theta[1:hmonte] * delRlum_nodust
            m_theta[1,*] = m_theta[1,*] + theta[1:hmonte] * delRlum
            m_theta[2,*] = m_theta[2,*] + theta[1:hmonte] * delsfr_noDust
            m_theta[3,*] = m_theta[3,*] + theta[1:hmonte] * delsfr

            m_theta[4,*] = m_theta[4,*] + theta[1:hmonte] * delmass
            m_theta[5,*] = m_theta[5,*] + theta[1:hmonte] * TOTAL(Mdyn)
            m_theta[6,*] = m_theta[6,*] + theta[1:hmonte]
          ENDIF

          IF num_monte GT 0 THEN BEGIN
            m_NII[0,*] = m_NII[0,*] + theta[0] * delsfr_NII_rand_nodust
            m_NII[1,*] = m_NII[1,*] + theta[0] * delsfr_NII_rand
            m_Dust[0,*] = m_Dust[0,*] + theta[0] * delsfr_dust_rand_R
            m_Dust[1,*] = m_Dust[1,*] + theta[0] * delsfr_dust_rand
          ENDIF

          totdelrho = totdelrho + deltarho
          totdelrho_NII[*,2] = totdelrho_NII[*,2] + theta[0] * edelsfr_NII_sys_nodust
          totdelrho_NII[*,3] = totdelrho_NII[*,3] + theta[0] * edelsfr_NII_sys
          totdelrho_dust[*,1] = totdelrho_dust[*,1] + theta[0] * edelsfr_dust_sys_R
          totdelrho_dust[*,3] = totdelrho_dust[*,3] + theta[0] * edelsfr_dust_sys
          err_cal = DBLARR(2) ; R, Halpha
          err_cal[0] = 0.008 ; R band is constant, for now
          IF STRPOS(ffilt[Sindex[index[0]]],'6568') GE 0 THEN err_cal[1] = 0.016 $
                                                         ELSE err_cal[1] = 0.008
          totdelrho_cal[*,0] = totdelrho_cal[*,0] + al10*err_cal[0]*(theta[0]*delRlum_nodust)*[1.0,1.0]
          totdelrho_cal[*,1] = totdelrho_cal[*,1] + al10*err_cal[0]*(theta[0]*delRlum)*[1.0,1.0]
          totdelrho_cal[*,2] = totdelrho_cal[*,2] + al10*err_cal[1]*(theta[0]*delsfr_nodust)*[1.0,1.0]
          totdelrho_cal[*,3] = totdelrho_cal[*,3] + al10*err_cal[1]*deltarho*[1.0,1.0]

          totdelmass = totdelmass + theta[0] * delmass
          totdelnum = totdelnum + theta[0]
          totRlum = totRlum + theta[0] * delRlum
          totRlum_nodust = totRlum_nodust + theta[0] * delRlum_nodust
          totdelrho_noNII = totdelrho_noNII + theta[0] * delsfr_noNII
          totdelrho_noDust = totdelrho_noDust + theta[0] * delsfr_noDust
          totdelrho_nounred = totdelrho_nounred + theta[0] * delsfr_nounred
          totdelrho_SB = totdelrho_SB + theta[0] * delsfr_SB
          totdelMdyn = totdelMdyn + theta[0] * Mdyn

          IF num_monte EQ 0 THEN BEGIN
            totskydelrho = totskydelrho + theta[0] * edelrho_sky
            totcntdelrho = totcntdelrho + theta[0] * edelrho_cnt
            totdelrhoplus = totdelrhoplus + sigtheta[0]*delsfr
            totdelrhominus = totdelrhominus + sigtheta[1]*delsfr
          ENDIF ELSE BEGIN
            FOR ll = 0,N_ELEMENTS(edelrho_sky)-1 DO BEGIN
              totskydelrho[ll,*] = totskydelrho[ll,*] + $
                   theta[0] * edelrho_sky[ll] * RANDOMN(seed,num_monte,/DOUBLE)
              totcntdelrho[ll,*] = totcntdelrho[ll,*] + $
                   theta[0] * edelrho_cnt[ll] * RANDOMN(seed,num_monte,/DOUBLE)
            ENDFOR
            totdelrhoplus = totdelrhoplus + $
                 sigtheta[0]*delsfr * RANDOMN(seed,num_monte,/DOUBLE)
            totdelrhominus = totdelrhominus + $
                 sigtheta[1]*delsfr * RANDOMN(seed,num_monte,/DOUBLE)
          ENDELSE

; Store values.
          num_in_bin[ii] = num_in_bin[ii] + 1
          gal_rho[numsfr] = deltarho
          dgal_rho[numsfr] = theta[0] * MEAN(SQRT(edelrho_sky[3,*]^2 + edelrho_cnt[3,*]^2))/imf
          gal_rho_nodust[numsfr] = theta[0] * delsfr_noDust
          gal_num[numsfr] = num_sources
          gal_mass[numsfr] = ALOG10(delmass)
          gal_mdyn[numsfr] = ALOG10(TOTAL(Mdyn))
          gal_Rlum[numsfr] = delRlum
          gal_Nlum[numsfr] = delsfr*imf
          gal_Rrho[numsfr] = theta[0] * delRlum
          gal_Rrho_nodust[numsfr] = theta[0] * delRlum_nodust
          gal_ew[numsfr] = delsfr*imf/delRlum
          sbgal[numsfr] = sbtally
          gal_tgas[numsfr] = 2.3 * delmass / delsfr
          gal_tgas_nodust[numsfr] = 2.3 * delmass / delsfr_noDust
          gal_himf[numsfr] = theta[0]
          gal_bin[numsfr] = ii
          gal_name[numsfr] = refname[refindex[jj]]

          IF dataflag THEN BEGIN
; Now, give the total line
             IF fnum[Sindex[0]] GT 1 THEN BEGIN
; There's multiple sources, so we've already given the SB status
               sbtext2 = "n/a"
             ENDIF ELSE BEGIN
               IF sbtally GT 0 THEN sbtext2 = "Y  " ELSE sbtext2 = "N  "
             ENDELSE
             PRINTF,dunit,numsfr+1,STRTRIM(refname[refindex[jj]],1),fnum[Sindex[0]], $
                          ALOG10(delmass),ALOG10(gal_himf[numsfr]), $
                          ALOG10(gal_Nlum[numsfr]),ALOG10(gal_Rlum[numsfr]), $
                          gal_tgas[numsfr]*1.0E-9,gal_ew[numsfr], $
                          MEAN(dustcorr[*,0]),MEAN(niicorr[*,0]),sbtext2, $
                          FORMAT='(I3," ",A9," ",I2," ",4(F6.3," "),F7.2," ",F6.2," ",2(F5.3," "),A3)'
          ENDIF

          sfrref[ii] = numsfr
          numsfr = numsfr + 1
        ENDIF

      ENDFOR

    ENDIF

    IF num_in_bin[ii] GT 0 THEN BEGIN
      scale = (xmax-xmin) / num_in_bin[ii]
      delrho[ii] = totdelrho / num_in_bin[ii]
      rho = rho + totdelrho * scale
      drhoNII_sys = drhoNII_sys + (totdelrho_NII * scale)
      drhodust_sys = drhodust_sys + (totdelrho_dust * scale)
      drhocal = drhocal + (totdelrho_cal * scale)
      rho_SB = rho_SB + totdelrho_SB * scale
      avemass[ii] = MEAN(gal_mass[(numsfr-num_in_bin[ii]):(numsfr-1)])
      gal_Nfrac[(numsfr-num_in_bin[ii]):(numsfr-1)] = gal_rho[(numsfr-num_in_bin[ii]):(numsfr-1)] * scale
      gal_Rfrac[(numsfr-num_in_bin[ii]):(numsfr-1)] = gal_Rrho[(numsfr-num_in_bin[ii]):(numsfr-1)] * scale

      IF hmonte GT 0 THEN BEGIN
        rho_m_theta = rho_m_theta + m_theta * scale
      ENDIF
      IF num_monte GT 0 THEN BEGIN
        rho_m_NII = rho_m_NII + m_NII * scale
        rho_m_Dust = rho_m_Dust + m_Dust * scale
;;print,avemass[ii],' ',MEAN(m_Dust[1,*]),' ',(MAX(m_Dust[1,*])/MIN(m_Dust[1,*])),' ',(MIN(m_Dust[1,*])/totdelrho),' ',totdelrho ;;
;;print,MEAN(rho_m_Dust[1,*])/rho,MAX(rho_m_Dust[1,*])/rho,MIN(rho_m_Dust[1,*])/rho
      ENDIF

      delR[ii] = totRlum / num_in_bin[ii]
      mass_rho = mass_rho + totdelmass * scale
      num_rho = num_rho + totdelnum * scale
      Mdyn_rho[0:2] = Mdyn_rho[0:2] + totdelMdyn * scale
      Mdyn_rho[3] = Mdyn_rho[3] + TOTAL(totdelMdyn)*scale
      Rlum_rho = Rlum_rho + totRlum * scale
      Rlum_rho_nodust = Rlum_rho_nodust + totRlum_nodust * scale
      niirho = niirho + totdelrho_noNII * scale
      nodustrho = nodustrho + totdelrho_noDust * scale
      nounredrho = nounredrho + totdelrho_nounred * scale
      erhosky = erhosky + totskydelrho * scale
      erhocnt = erhocnt + totcntdelrho * scale

      drho_sampplus = drho_sampplus + totdelrhoplus * scale
      drho_sampminus = drho_sampminus + totdelrhominus * scale

;;      bindist = sig_pm(gal_rho[(numsfr-num_in_bin[ii]):(numsfr-1)],MEANVAL=delrho[ii])

      IF num_in_bin[ii] GT 1 THEN BEGIN
        bindist = STDDEV(gal_rho[(numsfr-num_in_bin[ii]):(numsfr-1)]) * [1.0,1.0]
        maxdelrho[ii] = bindist[0]/SQRT(num_in_bin[ii])
        mindelrho[ii] = bindist[1]/SQRT(num_in_bin[ii])
      
;;      bindist2 = sig_pm(gal_Rrho[(numsfr-num_in_bin[ii]):(numsfr-1)],MEANVAL=delR[ii])
        bindist2 = STDDEV(gal_Rrho[(numsfr-num_in_bin[ii]):(numsfr-1)]) * [1.0,1.0]
        maxdelRlum[ii] = bindist2[0]/SQRT(num_in_bin[ii])
        mindelRlum[ii] = bindist2[1]/SQRT(num_in_bin[ii])
      ENDIF ELSE BEGIN
        mindelrho[ii] = 0.0
        maxdelrho[ii] = 0.0
        mindelRlum[ii] = 0.0
        maxdelRlum[ii] = 0.0
      ENDELSE  

      meansfr[ii] = totsfr/totmass
      dmeansfr[ii,*] = etotsfr/totmass
      meannodustsfr[ii] = totnodustsfr/totmass
      dmeannodustsfr[ii,*] = etotnodustsfr/totmass
    ENDIF ELSE BEGIN
; Bin is empty
      IF printflag THEN $
          PRINT,"Empty bin from log(MHI)=",xmin," to ",xmax
; Set delrho and meansfr.  These aren't actually used in any
; computations, they're just for plotting purposes.
      delrho[ii] = 1.0D-7
      delR[ii] = 3.0D+33
      meansfr[ii] = 3.0D-10
      meannodustsfr[ii] = 2.0D-10

      maxdelrho[ii] = 0.d0
      mindelrho[ii] = 0.d0
      maxdelRlum[ii] = 0.d0
      mindelRlum[ii] = 0.d0
    ENDELSE
  ENDFOR

  meantgas = 2.3/meansfr
  meantgas_nodust = 2.3/meannodustsfr
  FOR ii = 0,num_bins-1 DO BEGIN
    dmeantgas[ii,*] = 2.3*[dmeansfr[ii,1],dmeansfr[ii,0]]/(meansfr[ii]^2)
    dmeantgas_nodust[ii,*] = 2.3*[dmeannodustsfr[ii,1],dmeannodustsfr[ii,0]]/(meannodustsfr[ii]^2)
  ENDFOR

; A few corrections
  drhoNII_sys[0:1,0] = drhoNII_sys[0:1,0] / (al10*Rlum_rho_nodust)
  drhoNII_sys[0:1,1] = drhoNII_sys[0:1,1] / (al10*Rlum_rho)
  drhoNII_sys[0:1,2] = drhoNII_sys[0:1,2] / (al10*nodustrho)
  drhoNII_sys[0:1,3] = drhoNII_sys[0:1,3] / (al10*rho)
  drhoDust_sys[0:1,0] = drhoDust_sys[0:1,0] / (al10*Rlum_rho_nodust)
  drhoDust_sys[0:1,1] = drhoDust_sys[0:1,1] / (al10*Rlum_rho)
  drhoDust_sys[0:1,2] = drhoDust_sys[0:1,2] / (al10*nodustrho)
  drhoDust_sys[0:1,3] = drhoDust_sys[0:1,3] / (al10*rho)
  drhocal[0:1,0] = drhocal[0:1,0] / (al10*Rlum_rho_nodust)
  drhocal[0:1,1] = drhocal[0:1,1] / (al10*Rlum_rho)
  drhocal[0:1,2] = drhocal[0:1,2] / (al10*nodustrho)
  drhocal[0:1,3] = drhocal[0:1,3] / (al10*rho)
  gal_Rfrac = gal_Rfrac / Rlum_rho
  gal_Nfrac = gal_Nfrac / rho
  ew_singg = (rho*imf)/Rlum_rho
  ew_singg_nodust = (nodustrho*imf)/Rlum_rho_nodust
  tgas = 2.3E-9*(mass_rho/rho)

;; forprint,gal_name,gal_Rfrac,gal_Nfrac

  IF num_monte GE 1 THEN BEGIN
; In Monte Carlo logic, rho_m_theta is an hmonte-element array
; containing variation in rho due to changes in the HI mass function
; (theta).  The previous totdelrhoplus/totdelrhominus should be
; replaced with an overall error adjustment.
    drho_HIMF[0:1,0] = sig_pm(ALOG10(rho_m_theta[0,*]),MEANVAL=ALOG10(Rlum_rho_nodust))
    drho_HIMF[0:1,1] = sig_pm(ALOG10(rho_m_theta[1,*]),MEANVAL=ALOG10(Rlum_rho))
    drho_HIMF[0:1,2] = sig_pm(ALOG10(rho_m_theta[2,*]),MEANVAL=ALOG10(nodustrho))
    drho_HIMF[0:1,3] = sig_pm(ALOG10(rho_m_theta[3,*]),MEANVAL=ALOG10(rho))
; Now, for the rest
    drhoHI_HIMF = sig_pm(ALOG10(rho_m_theta[4,*]),MEANVAL=ALOG10(mass_rho))
    drhodyn_HIMF = sig_pm(ALOG10(rho_m_theta[5,*]),MEANVAL=ALOG10(Mdyn_rho[3]))
    dN_HIMF = sig_pm(ALOG10(rho_m_theta[6,*]),MEANVAL=ALOG10(num_rho))

    ew_HIMF = sig_pm((rho_m_theta[3,*]/rho_m_theta[1,*])*imf,MEANVAL=ew_singg)
    ew_HIMF_nodust = sig_pm((rho_m_theta[2,*]/rho_m_theta[0,*])*imf,MEANVAL=ew_singg_nodust)
    dtgas_HIMF = sig_pm(2.3E-9*(rho_m_theta[4,*]/rho_m_theta[3,*]),MEANVAL=tgas)
; NII only applies to the l_Ha (2,3), Dust only to (1,3)
    drhoDust_rand[0:1,1] = sig_pm(ALOG10(rho_m_Dust[0,*]),MEANVAL=ALOG10(Rlum_rho))
    drhoNII_rand[0:1,2] = sig_pm(ALOG10(rho_m_NII[0,*]),MEANVAL=ALOG10(nodustrho))
    drhoNII_rand[0:1,3] = sig_pm(ALOG10(rho_m_NII[1,*]),MEANVAL=ALOG10(rho))
    drhoDust_rand[0:1,3] = sig_pm(ALOG10(rho_m_Dust[1,*]),MEANVAL=ALOG10(rho))

    ew_dust = sig_pm(ALOG10(rho_m_Dust[1,*]/rho_m_Dust[0,*]*imf),MEANVAL=ALOG10(ew_singg))

    drhosky[0:1,0] = sig_pm(ALOG10(Rlum_rho_nodust+(erhosky[0,*])),MEANVAL=ALOG10(Rlum_rho_nodust))
    drhocnt[0:1,0] = sig_pm(ALOG10(Rlum_rho_nodust+(erhocnt[0,*])),MEANVAL=ALOG10(Rlum_rho_nodust))
    drhosky[0:1,1] = sig_pm(ALOG10(Rlum_rho+(erhosky[1,*])),MEANVAL=ALOG10(Rlum_rho))
    drhocnt[0:1,1] = sig_pm(ALOG10(Rlum_rho+(erhocnt[1,*])),MEANVAL=ALOG10(Rlum_rho))
    drhosky[0:1,2] = sig_pm(ALOG10(nodustrho+(erhosky[2,*]/imf)),MEANVAL=ALOG10(nodustrho))
    drhocnt[0:1,2] = sig_pm(ALOG10(nodustrho+(erhocnt[2,*]/imf)),MEANVAL=ALOG10(nodustrho))
    drhosky[0:1,3] = sig_pm(ALOG10(rho+(erhosky[3,*]/imf)),MEANVAL=ALOG10(rho))
    drhocnt[0:1,3] = sig_pm(ALOG10(rho+(erhocnt[3,*]/imf)),MEANVAL=ALOG10(rho))

; Bootstrap!
    newrho = DBLARR(num_monte)
    newrho_nodust = DBLARR(num_monte)
    newRlum = DBLARR(num_monte)
    newRlum_nodust = DBLARR(num_monte)
    newN = DBLARR(num_monte)
    newrhoHI = DBLARR(num_monte)
    newrhodyn = DBLARR(num_monte)
;;    xbin2 = [0.0,4.0,5.0,6.0,7.0,8.0,8.5,9.0,9.5,10.0,10.5,11.0,12.0]
    xbin2 = xbin

    FOR ii = LONG(0),LONG(num_monte)-1 DO BEGIN
      index = LONG(FLOAT(numsfr)*RANDOMU(seed,numsfr,/DOUBLE))

      FOR jj = 0,N_ELEMENTS(xbin2)-2 DO BEGIN
        list = WHERE(gal_mass[index] GE xbin2[jj] AND $
                     gal_mass[index] LT xbin2[jj+1],listcount)
        IF listcount GT 0 THEN BEGIN
           newrho[ii] = newrho[ii] + (xbin2[jj+1]-xbin2[jj]) * $
                        MEAN(gal_rho[index[list]])
           newrho_nodust[ii] = newrho_nodust[ii] + (xbin2[jj+1]-xbin2[jj]) * $
                               MEAN(gal_rho_nodust[index[list]])
           newRlum[ii] = newRlum[ii] + (xbin2[jj+1]-xbin2[jj]) * $
                         MEAN(gal_Rrho[index[list]])
           newRlum_nodust[ii] = newRlum_nodust[ii] + (xbin2[jj+1]-xbin2[jj]) * $
                                MEAN(gal_Rrho_nodust[index[list]])
           newN[ii] = newN[ii] + (xbin2[jj+1]-xbin2[jj]) * $
                      MEAN(gal_himf[index[list]])
           newrhoHI[ii] = newrhoHI[ii] + (xbin2[jj+1]-xbin2[jj]) * $
                        MEAN(10.d0^gal_mass[index[list]]*gal_himf[index[list]])
           newrhodyn[ii] = newrhodyn[ii] + (xbin2[jj+1]-xbin2[jj]) * $
                        MEAN(10.d0^gal_mdyn[index[list]]*gal_himf[index[list]])
        ENDIF
      ENDFOR
    ENDFOR

    drho_samp[0:1,0] = sig_pm(ALOG10(newRlum_nodust),MEANVAL=ALOG10(Rlum_rho_nodust))
    drho_samp[0:1,1] = sig_pm(ALOG10(newRlum),MEANVAL=ALOG10(Rlum_rho))
    drho_samp[0:1,2] = sig_pm(ALOG10(newrho_nodust),MEANVAL=ALOG10(nodustrho))
    drho_samp[0:1,3] = sig_pm(ALOG10(newrho),MEANVAL=ALOG10(rho))
    dN_samp = sig_pm(ALOG10(newN),MEANVAL=ALOG10(num_rho))
    drhoHI_samp = sig_pm(ALOG10(newrhoHI),MEANVAL=ALOG10(mass_rho))
    drhodyn_samp = sig_pm(ALOG10(newrhodyn),MEANVAL=ALOG10(Mdyn_rho[3]))

    ewval=(newrho/newRlum)*imf
    ew_samp = sig_pm(ewval,MEANVAL=ew_singg)
    ewval_nodust=(newrho_nodust/newRlum_nodust)*imf
    ew_samp_nodust = sig_pm(ewval_nodust,MEANVAL=ew_singg_nodust)

    tgasval = 2.3E-9*(newrhoHI/newrho)
    dtgas_samp = sig_pm(tgasval,MEANVAL=tgas)
  ENDIF ELSE BEGIN
; Can't do the HIMF errors if we don't monte.
    drhoHI_HIMF = [0.0,0.0]
    drhodyn_HIMF = [0.0,0.0]
    dN_HIMF = [0.0,0.0]
    ew_HIMF = [0.0,0.0]
    ew_HIMF_nodust = [0.0,0.0]
    dtgas_HIMF = [0.0,0.0]
; For the one case of corrected Halpha, we can at least approximate
; the sampling error.
    drho_samp[0:1,3] = [drho_sampplus,drho_sampminus]/(al10*rho)
; For the rest, don't even bother trying to approximate it.
    dN_samp = [0.0,0.0]
    drhoHI_samp = [0.0,0.0]
    drhodyn_samp = [0.0,0.0]
    ew_samp = [0.0,0.0]
    ew_samp_nodust = [0.0,0.0]
    dtgas_samp = [0.0,0.0]
    ew_dust = [0.0,0.0]
  ENDELSE

; Now that we've calculated everything, start plotting.
; We'll do this if we set /ps or if we DIDN'T set /silent.  /silent
; overrides the normal plot window, but it won't stop a .eps file from
; being made.
  IF printflag OR psflag THEN BEGIN
    IF NOT psflag THEN BEGIN
      set_plot,'X'
      setplotcolors
      setbgfg,!white,!black
      DEVICE, RETAIN=2, DECOMPOSED=0
      WINDOW,XSIZE=800,YSIZE=1100
      !P.MULTI=[0,0,3,0,0]

      charsz = 3.0
      symsz = 1.2
      thick = 1.0
    ENDIF ELSE BEGIN
; We'll write .eps files
      set_plot,'PS'
      setplotcolors
      xs = 5.5
      ys = 4.0
      xoff = 1.2
      yoff = 3.0
      DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
             bits_per_pixel=8,/encapsulated
;      !P.MULTI=[0,0,2,0,0]
      !P.MULTI=[0,0,1,0,0]

      charsz = 1.5
      symsz = 0.8
      thick = 2.0
    ENDELSE

    xmin = 6.9
    xmax = 11.0
    dummy = DBLARR(2)

    plotbin = WHERE(xbin GE xmin AND xbin LT xmax,bincount)

; Only use objects viewed so far
    usedindex = WHERE(used EQ 1)
    IF psflag THEN BEGIN
;      ymin = 0
;      ymax = MAX(num_in_bin)
;      PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],COLOR=!black, $
;         YTICKINTERVAL=10,XTICKLEN=0.05,XSTYLE=9,YMINOR=2,YTICKLEN=1.0,YGRIDSTYLE=1, $
;         XTITLE="!6log(M!DHI!N [M!Dsolar!N])",CHARSIZE=charsz,YTITLE="!6Number", $
;         CLIP=[xmin,ymin,xmax,ymax],POSITION=[0.15,0.20,0.97,0.35],THICK=thick
;      FOR ii = 0,num_bins-1 DO BEGIN
;        IF xbin[ii] GE xmin AND xbin[ii+1] LE xmax THEN BEGIN
;          POLYFILL,[xbin[ii],xbin[ii],xbin[ii+1],xbin[ii+1]], $
;                [0,num_in_bin[ii],num_in_bin[ii],0],COLOR=!dcyan
;          PLOTS,[xbin[ii],xbin[ii],xbin[ii+1],xbin[ii+1]], $
;                [0,num_in_bin[ii],num_in_bin[ii],0],COLOR=!black,THICK=thick
;        ENDIF
;        PLOTS,[xmin,xmax],[0,0],COLOR=!black,THICK=thick
;      ENDFOR
    ENDIF ELSE BEGIN
      plothist,(logMHI[usedindex]-2.0*ALOG10(H0/hipassH0)),xhist,yhist, $
           bin=0.2,XRANGE=[xmin,xmax],/FILL,FCOLOR=!dcyan,HALFBIN=0,/NAN,COLOR=!black, $
           XTITLE="!6log(M!DHI!N [M!Dsolar!N])",CHARSIZE=charsz, $
           YTITLE="!6Number of galaxies",TITLE="!6Number of galaxies",THICK=thick,XSTYLE=1
      FOR ii = 0,N_ELEMENTS(xhist)-2 DO BEGIN
        PLOTS,[(xhist[ii]+xhist[ii+1])/2.0,(xhist[ii]+xhist[ii+1])/2.0], $
              [0,MIN([yhist[ii],yhist[ii+1]])],COLOR=!black,THICK=thick
      ENDFOR
    ENDELSE

    ymin = 8.0
    ymax = 12.0

    IF psflag THEN BEGIN
      PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],COLOR=!black, $
;           XTITLE="",POSITION=[0.15,0.35,0.97,0.9],XCHARSIZE=0.001, $
           XTITLE="!6log(M!DHI!N [M!Dsolar!N])", $
           YTITLE="!6log(t!Dgas!N [yr])",CHARSIZE=charsz,XSTYLE=1, $
         XMARGIN=[6,2],YMARGIN=[3,2], $
           TITLE="",CLIP=[xmin,ymin,xmax,ymax],THICK=thick,YSTYLE=1
    ENDIF ELSE BEGIN
      PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],COLOR=!black, $
           XTITLE="!6log(M!DHI!N [M!Dsolar!N])",CHARSIZE=charsz, $
           YTITLE="!6log(t!Dgas!N [yr])",THICK=thick, $
           TITLE="!6Gas Cycling Time",CLIP=[xmin,ymin,xmax,ymax],XSTYLE=1,YSTYLE=1
    ENDELSE
    index2=WHERE(ALOG10(gal_tgas_nodust) GT ymin AND $
                 ALOG10(gal_tgas_nodust) LT ymax AND $
                 sbgal EQ 0,sfr2count)
    IF sfr2count GT 0 THEN PLOTS,gal_mass[index2],ALOG10(gal_tgas_nodust[index2]), $
                                 PSYM=SYM(1),COLOR=!dmagenta,SYMSIZE=symsz,THICK=thick
    index2SB=WHERE(ALOG10(gal_tgas_nodust) GT ymin AND $
                   ALOG10(gal_tgas_nodust) LT ymax AND $
                   sbgal GT 0,sfr2count)
    IF sfr2count GT 0 THEN BEGIN
      IF starflag THEN BEGIN
        PLOTS,gal_mass[index2SB],ALOG10(gal_tgas_nodust[index2SB]),PSYM=SYM(6), $
              COLOR=!dmagenta,SYMSIZE=symsz,THICK=thick
      ENDIF ELSE BEGIN
        PLOTS,gal_mass[index2SB],ALOG10(gal_tgas_nodust[index2SB]),PSYM=SYM(1), $
              COLOR=!dmagenta,SYMSIZE=symsz,THICK=thick
      ENDELSE
    ENDIF

    index=WHERE(ALOG10(gal_tgas) GT ymin AND $
                ALOG10(gal_tgas) LT ymax AND sbgal EQ 0,sfrcount)
    IF sfrcount GT 0 THEN PLOTS,gal_mass[index], $
          ALOG10(gal_tgas[index]),PSYM=SYM(1),COLOR=!dgreen,SYMSIZE=symsz,THICK=thick
    indexSB=WHERE(ALOG10(gal_tgas) GT ymin AND $
                  ALOG10(gal_tgas) LT ymax AND sbgal GT 0,sfrcount)
    IF sfrcount GT 0 THEN BEGIN
      IF starflag THEN BEGIN
        PLOTS,gal_mass[indexSB],ALOG10(gal_tgas[indexSB]),PSYM=SYM(6), $
              COLOR=!dgreen,SYMSIZE=symsz,THICK=thick
      ENDIF ELSE BEGIN
        PLOTS,gal_mass[indexSB],ALOG10(gal_tgas[indexSB]),PSYM=SYM(1), $
              COLOR=!dgreen,SYMSIZE=symsz,THICK=thick
      ENDELSE
    ENDIF

    FOR ii = 0,bincount-1 DO BEGIN
      binnum = plotbin[ii]
      IF num_in_bin[binnum] GT 0 THEN BEGIN
        IF binnum EQ 0 THEN BEGIN
          gasarr = ALOG10(gal_tgas[0:sfrref[plotbin[0]]])
          gasarr_nodust = ALOG10(gal_tgas_nodust[0:sfrref[plotbin[0]]])
        ENDIF ELSE BEGIN
          gasarr = ALOG10(gal_tgas[sfrref[binnum-1]+1:sfrref[binnum]])
          gasarr_nodust = ALOG10(gal_tgas_nodust[sfrref[binnum-1]+1:sfrref[binnum]])
        ENDELSE
        meanval = ALOG10(meantgas[binnum])
        meanval_nodust = ALOG10(meantgas_nodust[binnum])
        IF num_in_bin[binnum] GT 1 THEN BEGIN
;;          val1 = sig_pm(gasarr,MEANVAL=meanval)/SQRT(num_in_bin[binnum])
          val1 = STDDEV(gasarr)/SQRT(num_in_bin[binnum])*[1.0,1.0]
;;          val2 = sig_pm(gasarr_nodust,MEANVAL=meanval_nodust)/SQRT(num_in_bin[binnum])
          val2 = STDDEV(gasarr_nodust)/SQRT(num_in_bin[binnum])*[1.0,1.0]
        ENDIF ELSE BEGIN
          val1 = 0.0
          val2 = 0.0
        ENDELSE
        val1 = SQRT(val1^2 + (dmeantgas[binnum,*]/meantgas[binnum]/al10)^2)
        val2 = SQRT(val2^2 + (dmeantgas_nodust[binnum,*]/meantgas_nodust[binnum]/al10)^2)

; was ddgray and black
        ERRPLOT,avemass[binnum],meanval_nodust-val2[1], $
                                meanval_nodust+val2[0],COLOR=!magenta,THICK=thick
;        ERRPLOT,avemass[binnum],ALOG10(meantgas[binnum]-dmeantgas[binnum,1]), $
;                                ALOG10(meantgas[binnum]+dmeantgas[binnum,0]),COLOR=!magenta,THICK=thick
        ERRPLOT,avemass[binnum],meanval-val1[1], $
                                meanval+val1[0],COLOR=!green,THICK=thick
;        ERRPLOT,avemass[binnum],ALOG10(meantgas[binnum]-val1[1]), $
;                                ALOG10(meantgas[binnum]+val1[0]),COLOR=!green,THICK=thick
;; If we want the tgas uncertainty to include "sampling" error:
;dmeantgas[binnum,*] = val1 * meantgas[binnum] * al10
;dmeantgas_nodust[binnum,*] = val2 * meantgas_nodust[binnum] * al10
      ENDIF
      PLOTS,[xbin[binnum],xbin[binnum]],[ymin,ymax],COLOR=!black,THICK=thick,LINESTYLE=1
    ENDFOR
    OPLOT,avemass[plotbin],ALOG10(meantgas_nodust[plotbin]),PSYM=SYM(4),COLOR=!ddgray, $
          LINESTYLE=1,CLIP=[xmin,ymin,xmax,ymax],SYMSIZE=symsz*2.5,THICK=thick
    OPLOT,avemass[plotbin],ALOG10(meantgas[plotbin]),PSYM=SYM(4),COLOR=!ddgray, $
          LINESTYLE=1,CLIP=[xmin,ymin,xmax,ymax],SYMSIZE=symsz*2.5,THICK=thick
    OPLOT,avemass[plotbin],ALOG10(meantgas_nodust[plotbin]),PSYM=-SYM(4),COLOR=!magenta, $
          LINESTYLE=1,CLIP=[xmin,ymin,xmax,ymax],SYMSIZE=symsz*1.5,THICK=thick
    OPLOT,avemass[plotbin],ALOG10(meantgas[plotbin]),PSYM=-SYM(4),COLOR=!green, $
          LINESTYLE=1,CLIP=[xmin,ymin,xmax,ymax],SYMSIZE=symsz*1.5,THICK=thick

    hubbleval = ALOG10((3.0857E19)/(H0 * 86400 * 365.25))
    PLOTS,[xmin,xmax],[hubbleval,hubbleval],COLOR=!black,LINESTYLE=2,THICK=thick

    IF psflag THEN BEGIN
      psend,!outdir+"gastime.eps",/noprint,/clobber
      set_plot,'PS'
      DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
             bits_per_pixel=8,/encapsulated
      !P.MULTI=[0,0,1,0,0]
    ENDIF

; luminosity fraction per log(mass)
    ymin = -4.0
    ymax = +1.0
    IF psflag THEN title="" ELSE title = "!6Luminosity Density per log(mass)"
    PLOT,dummy,dummy,XRANGE=[xmin,xmax],YRANGE=[ymin,ymax],COLOR=!black, $
         XTITLE="!6log(M!DHI!N [M!Dsolar!N])",CHARSIZE=charsz, $
         YTITLE="!6log((!7d!8l!6/!7d!6log!8M!6)/!8l!6!Dtotal!N [dex!E-1!N])", $
         TITLE=title,CLIP=[xmin,ymin,xmax,ymax],THICK=thick,XSTYLE=1,YSTYLE=1, $
         XMARGIN=[6,2],YMARGIN=[3,2]

    indexH=WHERE((gal_rho/rho) GT 10.0^ymin AND sbgal EQ 0 AND $
                 (gal_rho/rho) LT 10.0^ymax,rhocount)
    indexR=WHERE((gal_Rrho/Rlum_rho) GT 10.0^ymin AND sbgal EQ 0 AND $
                 (gal_Rrho/Rlum_rho) LT 10.0^ymax,lumcount)
    IF lumcount GT 0 THEN PLOTS,gal_mass[indexR],ALOG10(gal_Rrho[indexR]/Rlum_rho), $
                          PSYM=SYM(1),COLOR=!dblue,CLIP=[xmin,ymin,xmax,ymax],SYMSIZE=symsz,THICK=thick
    IF rhocount GT 0 THEN PLOTS,gal_mass[indexH],ALOG10(gal_rho[indexH]/rho), $
                          PSYM=SYM(1),COLOR=!dred,CLIP=[xmin,ymin,xmax,ymax],SYMSIZE=symsz,THICK=thick

;;; test
;m0 = 6.9
;ind = WHERE(gal_rho/rho GT 10.0^ymin AND gal_rho/rho LT 10.0^ymax)
;k = SORT(gal_mass[ind])
;x = gal_mass[ind[k]]-m0
;y = gal_rho[ind[k]]/rho
;dy = dgal_rho[ind[k]]/rho
;junk = poly_fit(x,ALOG10(y),4,YFIT=curve,MEASURE_ERRORS=(dy/y)/al10)
;plots,x+m0,curve,color=!red
;junk2 = poly_fit(x,y,4,YFIT=curve2,MEASURE_ERRORS=dy)
;errplot,x+m0,ALOG10(y-dy),ALOG10(y+dy),color=!dgreen,THICK=thick
;mtest = m0 + 0.01*FINDGEN(401)
;testsfr = junk2[0]+junk2[1]*(mtest-m0)+junk2[2]*(mtest-m0)^2+junk2[3]*(mtest-m0)^3+junk2[4]*(mtest-m0)^4
;plots,mtest,ALOG10(testsfr),color=!blue

    indexHSB=WHERE((gal_rho/rho) GT 10.0^ymin AND sbgal GT 0 AND $
                   (gal_rho/rho) LT 10.0^ymax,rhocount)
    indexRSB=WHERE((gal_Rrho/Rlum_rho) GT 10.0^ymin AND sbgal GT 0 AND $
                   (gal_Rrho/Rlum_rho) LT 10.0^ymax,lumcount)

    IF lumcount GT 0 THEN BEGIN
      IF starflag THEN BEGIN
        PLOTS,gal_mass[indexRSB],ALOG10(gal_Rrho[indexRSB]/Rlum_rho), $
              PSYM=SYM(6),COLOR=!dblue,CLIP=[xmin,ymin,xmax,ymax],SYMSIZE=symsz,THICK=thick
      ENDIF ELSE BEGIN
        PLOTS,gal_mass[indexRSB],ALOG10(gal_Rrho[indexRSB]/Rlum_rho), $
              PSYM=SYM(1),COLOR=!dblue,CLIP=[xmin,ymin,xmax,ymax],SYMSIZE=symsz,THICK=thick
      ENDELSE
    ENDIF
    IF rhocount GT 0 THEN BEGIN
      IF starflag THEN BEGIN
        PLOTS,gal_mass[indexHSB],ALOG10(gal_rho[indexHSB]/rho), $
              PSYM=SYM(6),COLOR=!dred,CLIP=[xmin,ymin,xmax,ymax],SYMSIZE=symsz,THICK=thick
      ENDIF ELSE BEGIN
        PLOTS,gal_mass[indexHSB],ALOG10(gal_rho[indexHSB]/rho), $
              PSYM=SYM(1),COLOR=!dred,CLIP=[xmin,ymin,xmax,ymax],SYMSIZE=symsz,THICK=thick
      ENDELSE
    ENDIF

    OPLOT,avemass[plotbin],ALOG10(delR[plotbin]/Rlum_rho),COLOR=!ddgray,PSYM=SYM(4),LINESTYLE=1, $
          CLIP=[xmin,ymin,xmax,ymax],SYMSIZE=symsz*2.5,THICK=thick
    OPLOT,avemass[plotbin],ALOG10(delrho[plotbin]/rho),COLOR=!ddgray,PSYM=SYM(4),LINESTYLE=1, $
          CLIP=[xmin,ymin,xmax,ymax],SYMSIZE=symsz*2.5,THICK=thick
    OPLOT,avemass[plotbin],ALOG10(delR[plotbin]/Rlum_rho),COLOR=!blue,PSYM=-SYM(4),LINESTYLE=1, $
          CLIP=[xmin,ymin,xmax,ymax],SYMSIZE=symsz*1.5,THICK=thick
    OPLOT,avemass[plotbin],ALOG10(delrho[plotbin]/rho),COLOR=!red,PSYM=-SYM(4),LINESTYLE=1, $
          CLIP=[xmin,ymin,xmax,ymax],SYMSIZE=symsz*1.5,THICK=thick
    ERRPLOT,avemass[plotbin],ALOG10((delR[plotbin]-mindelRlum[plotbin])/Rlum_rho),$
            ALOG10((delR[plotbin]+maxdelRlum[plotbin])/Rlum_rho),COLOR=!blue,THICK=thick
    ERRPLOT,avemass[plotbin],ALOG10((delrho[plotbin]-mindelrho[plotbin])/rho), $
            ALOG10((delrho[plotbin]+maxdelrho[plotbin])/rho),COLOR=!red,THICK=thick

    FOR ii = 0,bincount-1 DO BEGIN
      binnum = plotbin[ii]
      PLOTS,[xbin[binnum],xbin[binnum]],[ymin,ymax],COLOR=!black,THICK=thick,LINESTYLE=1
    ENDFOR

    IF psflag THEN BEGIN
      !p.multi   = 0
      !p.noerase = 0
      psend,!outdir+"ldensity.eps",/noprint,/clobber
    ENDIF
  ENDIF

; Now that we've finished plotting, dump text to the screen.
  ranerr = SQRT(drhosky^2 + drhocnt^2 + drho_samp^2 + drho_HIMF[0:1,0:3]^2 + drhoNII_rand^2 + drhoDust_rand^2 + drhocal^2)
  syserr = SQRT(drhoNII_sys^2 + drhoDust_sys^2)
  toterr = SQRT(ranerr^2 + syserr^2)
  PRINT,"Star formation density [Msolar/year/Mpc^3] = ",rho, $
         FORMAT='(A,F8.4)'
  PRINT,"log(SFRD) = ",ALOG10(rho),FORMAT='(A,F10.4)'

  PRINT,"Log Uncertainty   Type      sig(l_R')       sig(l_R)        sig(l_Ha')      sig(l_Ha)"
  plotfmt = '(A,8(F6.4,A))'
  PRINT,"HI Mass Function  random (", $
         drho_HIMF[0,0],"/",drho_HIMF[1,0],") (", $
         drho_HIMF[0,1],"/",drho_HIMF[1,1],") (", $
         drho_HIMF[0,2],"/",drho_HIMF[1,2],") (", $
         drho_HIMF[0,3],"/",drho_HIMF[1,3],")", $
         FORMAT=plotfmt
  PRINT,"Sampling          random (", $
         drho_samp[0,0],"/",drho_samp[1,0],") (", $
         drho_samp[0,1],"/",drho_samp[1,1],") (", $
         drho_samp[0,2],"/",drho_samp[1,2],") (", $
         drho_samp[0,3],"/",drho_samp[1,3],")", $
         FORMAT=plotfmt
  PRINT,"Sky subtraction   random (", $
         drhosky[0,0],"/",drhosky[1,0],") (", $
         drhosky[0,1],"/",drhosky[1,1],") (", $
         drhosky[0,2],"/",drhosky[1,2],") (", $
         drhosky[0,3],"/",drhosky[1,3],")", $
         FORMAT=plotfmt
  PRINT,"Continuum scaling random (", $
         drhocnt[0,0],"/",drhocnt[1,0],") (", $
         drhocnt[0,1],"/",drhocnt[1,1],") (", $
         drhocnt[0,2],"/",drhocnt[1,2],") (", $
         drhocnt[0,3],"/",drhocnt[1,3],")", $
         FORMAT=plotfmt
  PRINT,"Calibration       random (", $
         drhocal[0,0],"/",drhocal[1,0],") (", $
         drhocal[0,1],"/",drhocal[1,1],") (", $
         drhocal[0,2],"/",drhocal[1,2],") (", $
         drhocal[0,3],"/",drhocal[1,3],")", $
         FORMAT=plotfmt
  PRINT,"[NII] correction  random (", $
         drhoNII_rand[0,0],"/",drhoNII_rand[1,0],") (", $
         drhoNII_rand[0,1],"/",drhoNII_rand[1,1],") (", $
         drhoNII_rand[0,2],"/",drhoNII_rand[1,2],") (", $
         drhoNII_rand[0,3],"/",drhoNII_rand[1,3],")", $
         FORMAT=plotfmt
  PRINT,"Dust correction   random (", $
         drhoDust_rand[0,0],"/",drhoDust_rand[1,0],") (", $
         drhoDust_rand[0,1],"/",drhoDust_rand[1,1],") (", $
         drhoDust_rand[0,2],"/",drhoDust_rand[1,2],") (", $
         drhoDust_rand[0,3],"/",drhoDust_rand[1,3],")", $
         FORMAT=plotfmt
  PRINT,"TOTAL RANDOM:            (", $
         ranerr[0,0],"/",ranerr[1,0],") (", $
         ranerr[0,1],"/",ranerr[1,1],") (", $
         ranerr[0,2],"/",ranerr[1,2],") (", $
         ranerr[0,3],"/",ranerr[1,3],")", $
         FORMAT=plotfmt
  PRINT,"[NII] zeropoint   system (", $
         drhoNII_sys[0,0],"/",drhoNII_sys[1,0],") (", $
         drhoNII_sys[0,1],"/",drhoNII_sys[1,1],") (", $
         drhoNII_sys[0,2],"/",drhoNII_sys[1,2],") (", $
         drhoNII_sys[0,3],"/",drhoNII_sys[1,3],")", $
         FORMAT=plotfmt
  PRINT,"Dust zeropoint    system (", $
         drhoDust_sys[0,0],"/",drhoDust_sys[1,0],") (", $
         drhoDust_sys[0,1],"/",drhoDust_sys[1,1],") (", $
         drhoDust_sys[0,2],"/",drhoDust_sys[1,2],") (", $
         drhoDust_sys[0,3],"/",drhoDust_sys[1,3],")", $
         FORMAT=plotfmt
  PRINT,"TOTAL SYSTEMATIC:        (", $
         syserr[0,0],"/",syserr[1,0],") (", $
         syserr[0,1],"/",syserr[1,1],") (", $
         syserr[0,2],"/",syserr[1,2],") (", $
         syserr[0,3],"/",syserr[1,3],")", $
         FORMAT=plotfmt
  PRINT,"TOTAL ERROR:             (", $
         toterr[0,0],"/",toterr[1,0],") (", $
         toterr[0,1],"/",toterr[1,1],") (", $
         toterr[0,2],"/",toterr[1,2],") (", $
         toterr[0,3],"/",toterr[1,3],")", $
         FORMAT=plotfmt

  niidex = ALOG10(rho/niirho)
  PRINT,"Value without NII correction = ",niirho,", (log=",ALOG10(niirho),"), for a difference of ", $
        (100.0*(1.0 - rho/niirho)),"% or ",niidex," dex", $
        FORMAT='(A,F6.4,A,F7.4,A,F6.3,A,F6.3,A)'
  
  dustdex = ALOG10(rho/nodustrho)
  PRINT,"Value without internal dust correction = ",nodustrho," (log=",ALOG10(nodustrho),"), for a difference of ", $
        dustdex," dex",FORMAT='(A,F6.4,A,F7.4,A,F6.3,A)'

  unreddex = ALOG10(rho/nounredrho)
  PRINT,"Value without galactic dust correction = ",nounredrho," (log=",ALOG10(nounredrho),"), for a difference of ", $
        unreddex," dex",FORMAT='(A,F6.4,A,F7.4,A,F6.3,A)'

  PRINT,"Value for starbursts = ",rho_SB,", log = ",ALOG10(rho_SB), $
        ", for a contribution of ",(rho_SB/rho)*100.0,"% from ",num_sb," sources",FORMAT='(A,F6.4,A,F6.3,A,F5.2,A,I3,A)'

  drhoHI = SQRT(drhoHI_samp^2 + drhoHI_HIMF^2)*al10*mass_rho
  PRINT,"Mass density [Msolar/Mpc^3] = ",mass_rho, $
        " (+",drhoHI[0],"/-",drhoHI[1],")", FORMAT='(A,E9.3,A,E9.3,A,E9.3,A)'

  dtgas = SQRT(dtgas_samp^2 + dtgas_HIMF^2)
  dtgas_other = SQRT(drhosky[0:1,3]^2 + drhocnt[0:1,3]^2 + drhocal[0:1,3]^2 + $
                     drhoNII_rand[0:1,3]^2 + drhoDust_rand[0:1,3]^2)
  dtgas[0] = SQRT(dtgas[0]^2 + (dtgas_other[1]*al10*tgas)^2)
  dtgas[1] = SQRT(dtgas[1]^2 + (dtgas_other[0]*al10*tgas)^2)
  PRINT,"Gas cycling time = ",tgas," +",dtgas[0],"/-",dtgas[1]," Gyr", $
        FORMAT='(A,3(F5.2,A))'
  PRINT,"  galaxy-to-galaxy variation = ",STDDEV(ALOG10(gal_tgas[0:numsfr-1]))," dex",FORMAT='(A,F5.3,A)'

  drhodyn = SQRT(drhodyn_samp^2 + drhodyn_HIMF^2)*al10*Mdyn_rho[3]
  PRINT,"Dynamical mass density [Msolar/Mpc^3]: ",Mdyn_rho[3], $
        " (+",drhodyn[0],"/-",drhodyn[1],")", FORMAT='(A,E9.3,A,E9.3,A,E9.3,A)'

  PRINT,"  Good = ",Mdyn_rho[0]," (",(Mdyn_rho[0]/Mdyn_rho[3])*100.0,"%)", $
        FORMAT='(A,E9.3,A,F5.2,A)'
  PRINT,"  Patched: multiple ",Mdyn_rho[1]," (",Mdyn_rho[1]/Mdyn_rho[3]*100.0,$
        "%) inclination ",Mdyn_rho[2]," (",Mdyn_rho[2]/Mdyn_rho[3]*100.0,"%)",$
        FORMAT='(A,E9.3,A,F5.2,A,E9.3,A,F5.2,A)'

  dN = SQRT(dN_samp^2 + dN_HIMF^2)*al10*num_rho
  PRINT,"Number density [Mpc^-3] = ",num_rho, $
        " (+",dN[0],"/-",dN[1],")", FORMAT='(A,E9.3,A,E9.3,A,E9.3,A)'

  PRINT,"R-band luminosity density [erg/Angstrom/s/Mpc^3] = ",Rlum_rho," (+", $
        ranerr[0,1]*al10*Rlum_rho,"/-",ranerr[1,1]*al10*Rlum_rho," rand) (+", $
        syserr[0,1]*al10*Rlum_rho,"/-",syserr[1,1]*al10*Rlum_rho," sys)", $
        FORMAT='(A,E9.3,4(A,E9.3),A)'
  PRINT,"  uncorrected for dust = ",Rlum_rho_nodust," (+", $
        ranerr[0,0]*al10*Rlum_rho_nodust,"/-",ranerr[1,0]*al10*Rlum_rho_nodust," rand) (+", $
        syserr[0,0]*al10*Rlum_rho_nodust,"/-",syserr[1,0]*al10*Rlum_rho_nodust," sys)", $
        FORMAT='(A,E9.3,4(A,E9.3),A)'
  PRINT,"  log: Corrected = ",ALOG10(Rlum_rho),", Uncorrected = ",ALOG10(Rlum_rho_nodust),FORMAT='(A,F7.4,A,F7.4)'

  PRINT,"Narrow-band luminosity density [erg/s/Mpc^3] = ",rho*imf," (+", $
        ranerr[0,3]*al10*rho*imf,"/-",ranerr[1,3]*al10*rho*imf," rand) (+", $
        syserr[0,3]*al10*rho*imf,"/-",syserr[1,3]*al10*rho*imf," sys)", $
        FORMAT='(A,E9.3,4(A,E9.3),A)'
  PRINT,"  uncorrected for dust = ",nodustrho*imf," (+", $
        ranerr[0,2]*al10*nodustrho*imf,"/-",ranerr[1,2]*al10*nodustrho*imf, $
        " rand) (+",syserr[0,2]*al10*nodustrho*imf,"/-",syserr[1,2]*al10*nodustrho*imf," sys)", $
        FORMAT='(A,E9.3,4(A,E9.3),A)'

  ew_singg = (rho*imf)/Rlum_rho
  ew_singg_nodust = (nodustrho*imf)/Rlum_rho_nodust

  ewsig = SQRT(ew_samp^2 + ew_HIMF^2 + (ew_dust*al10*ew_singg)^2)
  ewsig_nodust = SQRT(ew_samp_nodust^2 + ew_HIMF_nodust^2)

; Assume the systematic and all other random errors are uncorrelated.
  ew_other = FLTARR(2)
  ew_other[0] = SQRT(drhosky[0,3]^2 + drhosky[1,1]^2 + drhocnt[0,3]^2 + $
                     drhocal[0,3]^2 + drhoNII_rand[0,3]^2)
  ew_other[1] = SQRT(drhosky[1,3]^2 + drhosky[0,1]^2 + drhocnt[1,3]^2 + $
                     drhocal[1,3]^2 + drhoNII_rand[1,3]^2)

  ewsig = SQRT(ewsig^2 + (ew_other*al10*ew_singg)^2)
  ewsig_nodust = SQRT(ewsig_nodust^2 + (ew_other*al10*ew_singg_nodust)^2)

  PRINT,"SINGG EW50 [Angstrom] = ",ew_singg," (+",ewsig[0],"/-",ewsig[1],")", $
        FORMAT='(A,F6.2,A,F5.2,A,F5.2,A)'
  PRINT,"  uncorrected for dust = ",ew_singg_nodust," (+", $
        ewsig_nodust[0],"/-",ewsig_nodust[1],")", $
        FORMAT='(A,F6.2,A,F5.2,A,F5.2,A)'
  ew_sdss = (rho*imf)/(6.30E37) * (Rlum_rho_nodust/Rlum_rho)
  PRINT,"Cosmic EW [Angstrom] = ",ew_sdss," (+", $
        ranerr[0,3]*al10*ew_sdss,"/-",ranerr[1,3]*al10*ew_sdss," rand) (+", $
        syserr[0,3]*al10*ew_sdss,"/-",syserr[1,3]*al10*ew_sdss," sys)", $
        FORMAT='(A,F6.2,4(A,F5.2),A)'
  b_singg = ew_imf(ew_singg+[0.0,ewsig[0],-ewsig[1]],KROUPA=kr_singg,SCALO=sc_singg,/SILENT)
  b_sdss = ew_imf(ew_sdss+[0.0,[toterr[0,3],-toterr[1,3]]*al10*ew_sdss],KROUPA=kr_sdss,SCALO=sc_sdss,/SILENT)
  PRINT,"  Birthrate parameters:"
  PRINT,"  Salpeter: SINGG ",b_singg[0]," (+",(b_singg[1]-b_singg[0]),"/-",(b_singg[0]-b_singg[2]),")",FORMAT='(3(A,F6.4),A)'
  PRINT,"            SDSS  ",b_sdss[0]," (+",(b_sdss[1]-b_sdss[0]),"/-",(b_sdss[0]-b_sdss[2]),")",FORMAT='(3(A,F6.4),A)'
  PRINT,"  Kroupa:   SINGG ",kr_singg[0]," SDSS ",kr_sdss[0],FORMAT='(A,F6.4,A,F6.4)'
  PRINT,"  Scalo:    SINGG ",sc_singg[0]," SDSS ",sc_sdss[0],FORMAT='(A,F6.4,A,F6.4)'

  PRINT,"Total sources: ",TOTAL(gal_num[0:numsfr-1])
  junk = WHERE(gal_num[0:numsfr-1] GT 1,nmult)
  PRINT,"Number of galaxies with multiple sources: ",nmult
  IF dataflag THEN BEGIN
; Dump all the stuff
    PRINTF,dunit,""
    PRINTF,dunit,"Total sources: ",TOTAL(gal_num[0:numsfr-1])
    PRINTF,dunit,""
    textline = "# Mmin  Mmax  N  SFR/MHI   Tgas  uncert EW   Rho(bin) uncert    l_R(bin)  uncert"
    textfmt = '(" ",2(F5.2," "),I2," ",G9.4," ",F6.3," ",F6.3," ",F5.2," ",2(F8.6," "),G9.4," ",G9.4)'
    IF printflag THEN PRINT,textline
    PRINTF,dunit,textline
    index = WHERE(num_in_bin GT 0,goodnum)
    FOR ii = 0,goodnum-1 DO BEGIN
      IF printflag THEN $
         PRINT,xbin[index[ii]],xbin[index[ii]+1],num_in_bin[index[ii]], $
         meansfr[index[ii]],meantgas[index[ii]]*1E-9,MEAN(dmeantgas[index[ii],*])*1E-9, $
         (delrho[index[ii]]/delR[index[ii]]*imf), $
         delrho[index[ii]],(mindelrho[index[ii]]+maxdelrho[index[ii]])/2.0, $
         delR[index[ii]],(mindelRlum[index[ii]]+maxdelRlum[index[ii]])/2.0, $
         FORMAT=textfmt
      PRINTF,dunit,xbin[index[ii]],xbin[index[ii]+1],num_in_bin[index[ii]], $
         meansfr[index[ii]],meantgas[index[ii]]*1E-9,MEAN(dmeantgas[index[ii],*])*1E-9, $
         (delrho[index[ii]]/delR[index[ii]]*imf), $
         delrho[index[ii]],(mindelrho[index[ii]]+maxdelrho[index[ii]])/2.0, $
         delR[index[ii]],(mindelRlum[index[ii]]+maxdelRlum[index[ii]])/2.0, $
         FORMAT=textfmt
    ENDFOR
    ind2 = WHERE(gal_tgas GT 1E-13,numgood)
    averate = MEAN(2.3/gal_tgas[ind2])
    avetime = 2.3E-9/averate
    errrate = STDDEV(2.3/gal_tgas[ind2])/SQRT(numgood)
    errtime = 2.3E-9 * errrate / averate^2
    IF printflag THEN BEGIN
      PRINT,"SFR/MHI = ",averate," +/- ",errrate," yr^-1",FORMAT='(A,E9.3,A,E9.3,A)'
      PRINT,"  corresponding to ",avetime," +/- ",errtime," Gyr",FORMAT='(A,F5.3,A,F5.3,A)'
    ENDIF
    PRINTF,dunit,"# SFR/MHI = ",averate," +/- ",errrate," yr^-1",FORMAT='(A,E9.3,A,E9.3,A)'
    PRINTF,dunit,"#   corresponding to ",avetime," +/- ",errtime," Gyr",FORMAT='(A,F5.3,A,F5.3,A)'

    CLOSE,dunit
    FREE_LUN,dunit
  ENDIF

END
