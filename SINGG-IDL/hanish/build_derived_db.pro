PRO build_derived_db,VERBOSE=verbose,GOOD=good
; Build the "singg_derived" database.  It's basically a compilation of
; data from the "singg_flux" and "singg_sample" DBs, so that you
; don't need to cross-reference so much stuff.
; OPTIONAL INPUTS
;   /verbose  Include debugging print commands
;   /good     Only include "good" entries.

  headerdb = "proc3_header"
  refdb = "singg_sample"
  fluxdb = "singg_flux"

  silentflag = NOT KEYWORD_SET(verbose)

  newdb = "singg_derived"

  dbopen,headerdb,0
  dbext,-1,"TARGET, IMTYPE, FILTNAME,FILENAME,RUNID,MAGZPT1", $
            hobject,himtype,hfilter ,hfile   ,hrun, magzpt1
  dbclose,dummy
  hobject = update_name(hobject)
  hrun = 'Run'+STRTRIM(hrun,2)

  catfile = !singgdir+"/sr2_apmatch_all_02.dat"
  readcol_new,catfile,junk,cent,catname,hopnum,hopent,hopnam, $
              rc3num,rc3ent,rc3nam,nbgnum,nbgent,nbgnam,pgnum,pgent,pgnam, $
              qanum,qaent,qanam,junk,junk,junk,irnum,irent,irnam, $
              comment='#',/silent, $
              format="A,I,A,I,L,A,I,L,A,I,L,A,I,L,A,I,L,A,I,L,A,I,L,A"

  optidfile = !singgdir+"/sr2_optid_03.dat"
  readcol_new,optidfile,optname,junk,junk,junk,optid,source,oent,junk, $
              comment='#',/silent,format="A,I,A,A,X,A,A,L,F"

  voptfile = !singgdir+'sr2_voptid_03.dat'
  readcol_new,voptfile,vname,vent,vid,junk1,junk2,junk3,vbest,vref,vsid,vsv, $
              v2id,v2nam,v2v,v2sq,v6id,v6nam,v6v,v6sq,vnv,vnerr,vnref, $
              COMMENT='#',/SILENT, $
;;              FORMAT='A,I,A,A,I,F,I,A,A,I,L,A,I,I,L,A,I,I,I,I,A'
              FORMAT='A,I,A,A,I,F,I,A,A,I,L,A,I,I,L,A,I,I,I,I,A'

  readcol_new,!singgdir+'singg_multiple.dat',mobj,mfile,mrun,mpr, $
              COMMENT='#',FORMAT='(A,A,A,I)',/SILENT

  hlen = STRLEN(STRTRIM(hfile,2))
  hcont = STRARR(N_ELEMENTS(hfile))
  FOR ii = 0,N_ELEMENTS(hfile)-1 DO hcont[ii] = STRMID(hfile[ii],(hlen[ii]-12),1)

  dbopen,refdb,0
  dbext,-1,"NAME,      OPTID,CATALOG,DISTANCE,LOGMHI,VHEL,VLG,EBV,W50,RA,DEC", $
            refname,refoptid,CATALOG,distance,logMHI,vhel,vlg,EBV,W50,hra,hdec
  dbclose,dummy
  refname = update_name(refname)

  dbopen,fluxdb,0
  dbext,-1,"FILENAME,OBJECT, RUNID,FILTER,IMTYPE, FLUX_SCALE,PA,AXERAT,RA,DEC,GALINDEX,NUMGALS", $
            filename,fobject,runid,filter,fimtype,flux_scale,pa,axerat,ra,dec,galindex,numgals
  dbext,-1,"FLUX_S_ISO,FLUX_F_ISO,FLUX_T_ISO,FLUXRAD_S_ISO,FLUXRAD_F_ISO,FLUXRAD_C_ISO", $
            flux_s_iso,flux_f_iso,flux_t_iso,fluxrad_s_iso,fluxrad_f_iso,fluxrad_c_iso
  dbext,-1,"ERR_FLUX_S_SKY_ISO,ERR_FLUX_F_SKY_ISO,ERR_FLUX_T_SKY_ISO,ERR_FLUX_S_CONT_ISO,ERR_FLUX_F_CONT_ISO,ERR_FLUX_T_CONT_ISO", $
            err_flux_s_sky_iso,err_flux_f_sky_iso,err_flux_t_sky_iso,err_flux_s_cont_iso,err_flux_f_cont_iso,err_flux_t_cont_iso
  dbext,-1,"RE_F_BRT,RE_T_BRT,ERR_RE_F_SKY_BRT,ERR_RE_T_SKY_BRT,ERR_RE_F_CONT_BRT,ERR_RE_T_CONT_BRT", $
            re_f_brt,re_t_brt,err_re_f_sky_brt,err_re_t_sky_brt,err_re_f_cont_brt,err_re_t_cont_brt
  dbext,-1,"R90_F_ISO,R90_T_ISO,ERR_R90_F_SKY_ISO,ERR_R90_T_SKY_ISO,ERR_R90_F_CONT_ISO,ERR_R90_T_CONT_ISO", $
            r90_f_iso,r90_t_iso,err_r90_f_sky_iso,err_r90_t_sky_iso,err_r90_f_cont_iso,err_r90_t_cont_iso
  dbext,-1,"SE_F_BRT,SE_T_BRT,ERR_SE_F_SKY_BRT,ERR_SE_F_CONT_BRT,ERR_SE_T_SKY_BRT,ERR_SE_T_CONT_BRT", $
            se_f_brt,se_t_brt,err_se_f_sky_brt,err_se_f_cont_brt,err_se_t_sky_brt,err_se_t_cont_brt
  dbext,-1,"EW50_F_BRT,EW50_T_BRT,ERR_EW50_F_BRT,ERR_EW50_T_BRT,PHOTMTRC", $
            ew50_f_brt,ew50_t_brt,err_ew50_f_brt,err_ew50_t_brt,photmtrc
  dbext,-1,"FLAG_O,FLUX_O,ERR_FLUX_O_SKY,ERR_FLUX_O_CONT,SE_O,ERR_SE_O_SKY,ERR_SE_O_CONT", $
            FLAG_O,FLUX_O,ERR_FLUX_O_SKY,ERR_FLUX_O_CONT,SE_O,ERR_SE_O_SKY,ERR_SE_O_CONT

;;  flind = good_flux(/TRIM)

  dbclose,dummy
  fobject = update_name(fobject)

;; Should apply flind everywhere the flux database is used.

  dbopen,'rc3'
  dbext,-1,'MORPH,NAME1',rcmorph,rcname1
  dbclose

  dbopen,'hicat_feb04',0
  dbext,-1,'HIPASS_NAME,COMMENT,CFSD_FLG,EXT_FLG', $
            hiname,     hcom,  cfsd_flg,ext_flg
  dbclose

  q0 = 0.20

; Galaxies we don't want to use for dynamical math.
  badlist = ["J1321-31","J0410-61","J2022-31","J0403-01"]

;; REPLACE: call good_flux, but only reject objects with "don't use" flagged.

; Set up the new db and data structure
  spawn,"/bin/rm -f "+!singgdir+""+newdb+".dbf"
  spawn,"/bin/rm -f "+!singgdir+""+newdb+".dbh"
  spawn,"/bin/rm -f "+!singgdir+""+newdb+".dbx"

  !PRIV=2
  dbcreate,newdb,1,1,/EXTERNAL
  spawn,"mv -f "+newdb+".db* "+!singgdir

  dbfile = !singgdir+""+newdb+".dbd"

; dbs is the database structure
  dbs = create_db_struct(dbfile,TITLE="derived", $
                         VNAME=varname,VTYPE=vartype,VUNIT=varunit)

; We're going to have one line of database for each source; the
; flux DB had 3-5 for each source, one for each filter used.
; Therefore, we only need to use the net images as reference.

  netindex = WHERE(STRTRIM(fimtype,2) EQ "net",netcount)
;;  netindex = WHERE(STRTRIM(fimtype,2) EQ "net" AND STRMID(fobject,0,8) EQ 'J1339-31',netcount)

; There will be one net image per source, except for images with
; duplicate filters (which will have two per) or images observed on
; multiple runs.

; Convert the Vhel values to local narrow-band extinction correction factors,
; based on what wavelength that V corresponds to.
  vlight = 3.0E5 ; km/s
  wave_n = 6562.8 * (1.0+(vhel/vlight))
  nunred = wave_n*0.0 + 1.0
  ccm_unred,wave_n,nunred,ebv
; "nunred" will be a multiplicative factor; nunred*flux = true flux
  wave_r = wave_n*0.0 + 6507.46
  runred = wave_n*0.0 + 1.0
  ccm_unred,wave_r,runred,ebv

  hipassH0 = 70.0
  al10 = ALOG(10.d0)
  cm_Mpc = 3.0857D24; cm per Mpc
  G = (1.0 / 232.47) ; (km/s)^2 pc Msolar^-1

  stellabs = 1.04 ; correction to net Halpha flux for stellar absorption

; Now, loop over the sources
  FOR ii = 0,netcount-1 DO BEGIN
    ngals = numgals[netindex[ii]]
    index = WHERE(STRTRIM(STRUPCASE(fobject),2) EQ STRTRIM(STRUPCASE(fobject[netindex[ii]]),2))
    mingal = MIN(galindex[index]) ; smallest galaxy index used for this object
    galind = galindex[netindex[ii]] - mingal

;;    junk = WHERE(STRTRIM(fobject[netindex],2) EQ STRTRIM(fobject[netindex[ii]],2) AND $
;;                 STRTRIM(runid[netindex],2) EQ STRTRIM(runid[netindex[ii]],2) AND $
;;                 STRTRIM(galindex[netindex],2) EQ STRTRIM(galindex[netindex[ii]],2),linecount)
; Next, find the R and narrow-band entries corresponding to the above image
    len = STRLEN(STRTRIM(filename[netindex[ii]],2))
    contfilter = STRMID(STRTRIM(filename[netindex[ii]],2),(len-12),1)
; Multiple continuum matches.  Figure out which to use.
    CASE contfilter OF
    "R": BEGIN
    rindex = WHERE(STRTRIM(fimtype,2) EQ 'cont' AND $
                   STRTRIM(fobject,2) EQ STRTRIM(fobject[netindex[ii]],2) AND $
                   STRTRIM(filter,2) EQ 'R_Harris' AND $
                   STRTRIM(runid,2) EQ STRTRIM(runid[netindex[ii]],2) AND $
                   STRTRIM(galindex,2) EQ STRTRIM(galindex[netindex[ii]],2),rcount)
         
         END
    "C": BEGIN
    rindex = WHERE(STRTRIM(fimtype,2) EQ 'cont' AND $
                   STRTRIM(fobject,2) EQ STRTRIM(fobject[netindex[ii]],2) AND $
                   STRTRIM(filter,2) EQ '6850/95' AND $
                   STRTRIM(runid,2) EQ STRTRIM(runid[netindex[ii]],2) AND $
                   STRTRIM(galindex,2) EQ STRTRIM(galindex[netindex[ii]],2),rcount)
         END
    'V': BEGIN
         END
    'I': BEGIN
         END
    ELSE: BEGIN
      PRINT,"ERROR in build_derived_pro: unknown continuum filter ",contfilter
      RETURN
          END
    ENDCASE

    nindex = WHERE(STRTRIM(fimtype,2) EQ "onband" AND $
                   STRTRIM(fobject,2) EQ STRTRIM(fobject[netindex[ii]],2) AND $
                   STRTRIM(filter,2) EQ STRTRIM(filter[netindex[ii]],2) AND $
                   STRTRIM(runid,2) EQ STRTRIM(runid[netindex[ii]],2) AND $
                   STRTRIM(galindex,2) EQ STRTRIM(galindex[netindex[ii]],2),ncount)

;;    IF rcount*ncount NE linecount THEN BEGIN
; If you have multiple narrow-band images, rcount will equal 2 since 
; the continuum image will have been entered once for each subtracted
; image.
; If you have both an R_Harris and a 6850/95 image of the galaxy,
; ncount will be 2 since duplicate entries will have been used for the
; narrow-band image.
      IF rcount NE 1 THEN BEGIN
        PRINT,"ERROR in build_derived_db: multiple continuum matches: ", $
              fobject[netindex[ii]],rcount
        RETURN
      ENDIF
      IF ncount NE 1 THEN BEGIN
        PRINT,"ERROR in build_derived_db: multiple narrow-band matches: ", $
              fobject[netindex[ii]],ncount
        RETURN
      ENDIF
;;    ENDIF

    clear_struct,dbs,vartype,varunit=varunit
; Match indices in all databases to the flux database
    Rheadindex = WHERE(STRTRIM(himtype,2) EQ "cont" AND $
                       STRTRIM(hfile,2) EQ STRTRIM(filename[rindex[0]],2) AND $
                       STRTRIM(hrun,2) EQ STRTRIM(runid[rindex[0]],2) AND $
                       STRTRIM(hfilter,2) EQ STRTRIM(filter[rindex[0]],2),rheadcount)
    IF rheadcount NE 1 THEN BEGIN
; In the header DB, there should be one entry per raw image.
      PRINT,"ERROR in build_derived_db: continuum header mismatch ",fobject[rindex[0]],rheadcount
print,runid[rindex[0]],filter[rindex[0]]
      RETURN
    ENDIF

    Nheadindex = WHERE(STRTRIM(himtype,2) EQ "onband" AND $
                       STRTRIM(hfile,2) EQ STRTRIM(filename[nindex[0]],2) AND $
                       STRTRIM(hrun,2) EQ STRTRIM(runid[nindex[0]],2) AND $
                       STRTRIM(hfilter,2) EQ STRTRIM(filter[nindex[0]],2),nheadcount)
    IF nheadcount NE 1 THEN BEGIN
; In the header DB, there should be one entry per raw image.
      PRINT,"ERROR in build_derived_db: narrow-band header mismatch ",fobject[nindex[0]],nheadcount
      RETURN
    ENDIF

    Sheadindex = WHERE(STRTRIM(himtype,2) EQ 'net' AND $
                       STRTRIM(hfile,2) EQ STRTRIM(filename[netindex[ii]],2) AND $
                       STRTRIM(hrun,2) EQ STRTRIM(runid[netindex[ii]],2) AND $
                       STRTRIM(hcont,2) EQ STRTRIM(contfilter,2) AND $
                       STRTRIM(hfilter,2) EQ STRTRIM(filter[netindex[ii]],2),sheadcount)
    IF sheadcount NE 1 THEN BEGIN
; In the header DB, there should be one entry per raw image.
      PRINT,"ERROR in build_derived_db: subtracted header mismatch ",fobject[netindex[ii]],sheadcount
      RETURN
    ENDIF

    refindex = WHERE(STRTRIM(refname,2) EQ STRTRIM(fobject[netindex[ii]],2),refcount)
    IF refcount GT 1 THEN BEGIN
; In the reference database there should be one entry per HIPASS object
      PRINT,"ERROR in build_derived_db: multiple reference matches ",fobject[netindex[ii]],refcount
      RETURN
    ENDIF

    IF refcount EQ 0 THEN BEGIN
; If refcount=0 it means the object wasn't in the HIPASS database
; (j0410-61), so skip for now
      IF NOT silentflag THEN $
        PRINT,"WARNING: Object missing from singg_sample database: ",fobject[netindex[ii]]
    ENDIF ELSE BEGIN
; Now that we've matched indices, build the structure
      IF ngals GT 1 THEN BEGIN
        dbs.name = STRTRIM(fobject[netindex[ii]],2)+":S"+STRTRIM(STRING(galind+1),2)
      ENDIF ELSE BEGIN
        dbs.name = STRTRIM(fobject[netindex[ii]],2)
      ENDELSE

      catindex = WHERE(catname EQ dbs.name,catcount)
      IF catcount EQ 0 THEN BEGIN
        catindex = WHERE(catname EQ (STRTRIM(fobject[netindex[ii]],2)+":S1"),catcount)
        IF catcount EQ 0 THEN BEGIN
          IF NOT silentflag THEN $
            PRINT,"WARNING in build_derived_db: no catalog name match for ",dbs.name
          dbs.entry_hopcat = LONG(-1)
          dbs.entry_rc3 = LONG(-1)
          dbs.entry_n_gal = LONG(-1)
          dbs.entry_p_gal = LONG(-1)
          dbs.entry_q_agn = LONG(-1)
          dbs.entry_iras = LONG(-1)
        ENDIF ELSE BEGIN
          dbs.entry_hopcat = hopent[catindex[0]]
          dbs.entry_rc3 = rc3ent[catindex[0]]
          dbs.entry_n_gal = nbgent[catindex[0]]
          dbs.entry_p_gal = pgent[catindex[0]]
          dbs.entry_q_agn = qaent[catindex[0]]
          dbs.entry_iras = irent[catindex[0]]
        ENDELSE
      ENDIF ELSE BEGIN
        IF catcount GT 1 AND NOT silentflag THEN $
          PRINT,"WARNING in build_derived_db: multiple catalog name matches for ",dbs.name,catcount

        dbs.entry_hopcat = hopent[catindex[0]]
        dbs.entry_rc3 = rc3ent[catindex[0]]
        dbs.entry_n_gal = nbgent[catindex[0]]
        dbs.entry_p_gal = pgent[catindex[0]]
        dbs.entry_q_agn = qaent[catindex[0]]
        dbs.entry_iras = irent[catindex[0]]
      ENDELSE

      IF dbs.entry_rc3 GT 0 THEN BEGIN
        dbs.morphology = STRTRIM(rcmorph[dbs.entry_rc3-1],2)
      ENDIF ELSE BEGIN
        dbs.morphology = 'N/A'
      ENDELSE

      optindex = WHERE(optname EQ dbs.name,optcount)
      IF optcount EQ 0 THEN BEGIN
        optindex = WHERE(optname EQ (STRTRIM(dbs.name,2)+":S1"),optcount)
        IF optcount EQ 0 THEN BEGIN
          IF NOT silentflag THEN $
            PRINT,"WARNING in build_derived_db: no optid name match for ",dbs.name
          dbs.optid = "NO_MATCH"
          dbs.source = "NONE"
          dbs.entry_opt = LONG(-1)
        ENDIF ELSE BEGIN
          dbs.optid = optid[optindex[0]]
          dbs.source = source[optindex[0]]
          dbs.entry_opt = oent[optindex[0]]
        ENDELSE
      ENDIF ELSE BEGIN
        dbs.optid = optid[optindex[0]]
        dbs.source = source[optindex[0]]
        dbs.entry_opt = oent[optindex[0]]
      ENDELSE

      vindex = WHERE(STRTRIM(vname,2) EQ dbs.name,vcount)
      IF vcount EQ 0 THEN vindex = WHERE(vname EQ (STRTRIM(dbs.name,2)+":S1"),vcount)
      IF vcount EQ 0 THEN BEGIN
        IF NOT silentflag THEN $
          PRINT,"WARNING in build_derived_db: no voptid name match for ",dbs.name
      ENDIF ELSE BEGIN
        dbs.v_best = vbest[vindex[0]]
        dbs.ref_best = vref[vindex[0]] ; will have tons of '-' attached
        dbs.entry_sdss = vsid[vindex[0]]
        dbs.v_sdss = vsv[vindex[0]]
        dbs.entry_2df = v2id[vindex[0]]
        dbs.name_2df = v2nam[vindex[0]]
        dbs.v_2df = v2v[vindex[0]]
        dbs.specqual_2df = v2sq[vindex[0]]
        dbs.entry_6df = v6id[vindex[0]]
        dbs.name_6df = v6nam[vindex[0]]
        dbs.v_6df = v6v[vindex[0]]
        dbs.specqual_6df = v6sq[vindex[0]]
        dbs.v_ned = vnv[vindex[0]]
        dbs.err_v_ned = vnerr[vindex[0]]
        dbs.ref_ned = vnref[vindex[0]]
      ENDELSE

      dbs.runid = STRTRIM(runid[netindex[ii]],2)
      dbs.object = STRTRIM(fobject[netindex[ii]],2)
      dbs.ra = ra[rindex[0]]
      dbs.dec = dec[rindex[0]]
      dbs.axerat = axerat[netindex[ii]]
      sini = (SQRT((1.0 - (1.0/dbs.axerat)^2)/(1.0 - q0^2)) < 1.0)
      dbs.inclination = ASIN(sini)/!dtor
      dbs.pa = pa[netindex[ii]]

; Copy the photometric keyword from the flux database, not the header.
      dbs.photmtrc = photmtrc[netindex[ii]]

      dbs.filter_r = filter[rindex[0]]
      dbs.filter_n = filter[nindex[0]]

      hindex = WHERE(STRUPCASE(STRTRIM(hiname,2)) EQ STRUPCASE(STRTRIM(fobject[netindex[ii]],2)),hicount)
      IF hicount GT 0 THEN BEGIN
        dbs.cfsd_flg = cfsd_flg[hindex[0]]
        dbs.ext_flg = ext_flg[hindex[0]]
        dbs.hcat_comm = LONG(hcom[hindex[0]] GT 0)
      ENDIF ELSE BEGIN
        dbs.cfsd_flg = -999
        dbs.ext_flg = -999
        dbs.hcat_comm = -999
      ENDELSE

      dbs.vel_match = 0
      IF vcount EQ 0 THEN BEGIN
        dbs.vel_match = 1
      ENDIF ELSE BEGIN
        dvel = ABS(vhel[refindex[0]] - dbs.v_best)
        IF dvel GT 500.0 THEN BEGIN
          dbs.vel_match = 3
        ENDIF ELSE BEGIN
          IF dvel GT 0.5*W50[refindex[0]] THEN dbs.vel_match = 2
        ENDELSE
      ENDELSE

      gcirc,1,(dbs.ra/15.0),dbs.dec,(hra[refindex[0]]/15.0),hdec[refindex[0]],ang
      dbs.sep_hipass = ang/60.0

;;      dbs.edge_flg = 
;;      dbs.overlap_flg = 
;;      dbs.manycr_flg = 
;;      dbs.hump_flg = 

      mind = WHERE(STRTRIM(mobj,2) EQ dbs.object AND $
                   STRTRIM(mfile,2) EQ STRTRIM(filename[netindex[ii]],2) AND $
                   STRTRIM(mrun,2) EQ dbs.runid,mcount)
      IF mcount EQ 1 THEN dbs.mult_priority = mpr[mind[0]] $
                     ELSE dbs.mult_priority = 1

      correction,fobject[netindex[ii]],niicorr,dustcorr,niierr,dusterr, $
                 W6583=W6583,RMAG=rmag,KNII=knii, $
                 run=dbs.runid,filt=[dbs.filter_r,dbs.filter_n]

      dbs.a_r_gal = 2.5*ALOG10(runred[refindex[0]])
      dbs.a_n_gal = 2.5*ALOG10(nunred[refindex[0]])
      dbs.a_r_int = dustcorr[galind,0]*0.5
      dbs.a_n_int = dustcorr[galind,0]

      dbs.w_6583 = W6583[galind]
      dbs.knii = knii[galind]
      dbs.niicorr = 2.5*ALOG10(1.d0 - niicorr[galind,0]) ; Will be negative

      dbs.logmhi = logmhi[refindex[0]]
      dbs.distance = distance[refindex[0]] ; Hubble constant correction?
      dbs.distance_lg = vlg[refindex[0]]/hipassH0 ; Hubble constant correction?
      distcm = dbs.distance * cm_Mpc ; convert from Mpc to cm
      dbs.rmax_s = fluxrad_s_iso[netindex[ii]]
      dbs.rmax_f = fluxrad_f_iso[netindex[ii]]
      dbs.rmax_c = fluxrad_c_iso[netindex[ii]]
      dbs.radius_s = (fluxrad_s_iso[netindex[ii]]/3600.0) * !dtor * (dbs.distance*1000.0)
      dbs.radius_f = (fluxrad_f_iso[netindex[ii]]/3600.0) * !dtor * (dbs.distance*1000.0)
      dbs.radius_c = (fluxrad_c_iso[netindex[ii]]/3600.0) * !dtor * (dbs.distance*1000.0)

      IF STRPOS(filter[netindex[ii]],'6568') GE 0 THEN err_logf_ha_cal = 0.016 $
                                                  ELSE err_logf_ha_cal = 0.008
      err_logf_r_cal = 0.008

      mag_base = magzpt1[Rheadindex[0]] - 5.0*(ALOG10(dbs.distance)+5.0) - dbs.a_r_gal
      IF STRTRIM(contfilter,2) EQ "C" THEN mag_base = mag_base + 0.114

      IF flux_s_iso[rindex[0]] GT 0.0 THEN BEGIN
        dbs.logl_r_s = ALOG10(flux_s_iso[rindex[0]]) + $
                      dbs.a_r_gal/2.5+ ALOG10(4.d0*!pi*distcm^2.d0)
        dbs.logl_r0_s = dbs.logl_r_s + dbs.a_r_int/2.5
        dbs.err_logl_r_s = SQRT((err_flux_s_sky_iso[rindex[0]] / (flux_s_iso[rindex[0]] * al10))^2 + err_logf_r_cal^2)
        dbs.mabs_r_s = mag_base - 2.5*ALOG10(flux_s_iso[rindex[0]]/flux_scale[rindex[0]])
        dbs.mabs_r0_s = dbs.mabs_r_s - dbs.a_r_int
        dbs.mapp_r_s = dbs.mabs_r_s + 5.0*(ALOG10(dbs.distance) + 5.0)
        dbs.err_mag_r_s = dbs.err_logl_r_s * 2.5
      ENDIF

      IF flux_f_iso[rindex[0]] GT 0.0 THEN BEGIN
        dbs.logl_r_f = ALOG10(flux_f_iso[rindex[0]]) + $
                      dbs.a_r_gal/2.5+ ALOG10(4.d0*!pi*distcm^2.d0)
        dbs.logl_r0_f = dbs.logl_r_f + dbs.a_r_int/2.5
        dbs.err_logl_r_f = SQRT((err_flux_f_sky_iso[rindex[0]] / (flux_f_iso[rindex[0]] * al10))^2 + err_logf_r_cal^2)
        dbs.mabs_r_f = mag_base - 2.5*ALOG10(flux_f_iso[rindex[0]]/flux_scale[rindex[0]])
        dbs.mabs_r0_f = dbs.mabs_r_f - dbs.a_r_int
        dbs.mapp_r_f = dbs.mabs_r_f + 5.0*(ALOG10(distance[refindex[0]]) + 5.0)
        dbs.err_mag_r_f = dbs.err_logl_r_f * 2.5
      ENDIF

      IF flux_t_iso[rindex[0]] GT 0.0 THEN BEGIN
        dbs.logl_r_t = ALOG10(flux_t_iso[rindex[0]]) + $
                      dbs.a_r_gal/2.5+ ALOG10(4.d0*!pi*distcm^2.d0)
        dbs.logl_r0_t = dbs.logl_r_t + dbs.a_r_int/2.5
        dbs.err_logl_r_t = SQRT((err_flux_t_sky_iso[rindex[0]] / (flux_t_iso[rindex[0]] * al10))^2 + err_logf_r_cal^2)
        dbs.mabs_r_t = mag_base - 2.5*ALOG10(flux_t_iso[rindex[0]]/flux_scale[rindex[0]])
        dbs.mabs_r0_t = dbs.mabs_r_t - dbs.a_r_int
        dbs.mapp_r_t = dbs.mabs_r_t + 5.0*(ALOG10(distance[refindex[0]]) + 5.0)
        dbs.err_mag_r_t = dbs.err_logl_r_t * 2.5
      ENDIF

      dbs.re_r_f = re_f_brt[rindex[0]]
      dbs.re_r_t = re_t_brt[rindex[0]]
      dbs.err_re_r_f = err_re_f_sky_brt[rindex[0]]
      dbs.err_re_r_t = err_re_t_sky_brt[rindex[0]]
      dbs.re_ha_f = re_f_brt[netindex[ii]]
      dbs.re_ha_t = re_t_brt[netindex[ii]]
      dbs.err_re_ha_f_sky = err_re_f_sky_brt[netindex[ii]]
      dbs.err_re_ha_t_sky = err_re_t_sky_brt[netindex[ii]]
      dbs.err_re_ha_f_cont = err_re_f_cont_brt[netindex[ii]]
      dbs.err_re_ha_t_cont = err_re_t_cont_brt[netindex[ii]]
      IF (dbs.err_re_ha_f_sky GT 0.0) AND (dbs.err_re_ha_f_sky GT 0.0) THEN $
          dbs.err_re_ha_f = SQRT((dbs.err_re_ha_f_sky>0.0)^2 + (dbs.err_re_ha_f_cont>0.0)^2)
      IF (dbs.err_re_ha_t_sky GT 0.0) AND (dbs.err_re_ha_t_sky GT 0.0) THEN $
          dbs.err_re_ha_t = SQRT((dbs.err_re_ha_t_sky>0.0)^2 + (dbs.err_re_ha_t_cont>0.0)^2)

      dbs.r90_r_f = r90_f_iso[rindex[0]]
      dbs.r90_r_t = r90_t_iso[rindex[0]]
      dbs.err_r90_r_f = err_r90_f_sky_iso[rindex[0]]
      dbs.err_r90_r_t = err_r90_t_sky_iso[rindex[0]]
      dbs.r90_ha_f = r90_f_iso[netindex[ii]]
      dbs.r90_ha_t = r90_t_iso[netindex[ii]]
      dbs.err_r90_ha_f_sky = err_r90_f_sky_iso[netindex[ii]]
      dbs.err_r90_ha_t_sky = err_r90_t_sky_iso[netindex[ii]]
      dbs.err_r90_ha_f_cont = err_r90_f_cont_iso[netindex[ii]]
      dbs.err_r90_ha_t_cont = err_r90_t_cont_iso[netindex[ii]]
      dbs.err_r90_ha_f = SQRT(dbs.err_r90_ha_f_sky^2 + dbs.err_r90_ha_f_cont^2)
      dbs.err_r90_ha_t = SQRT(dbs.err_r90_ha_t_sky^2 + dbs.err_r90_ha_t_cont^2)

      dbs.mdyn_method = 1
      IF dbs.axerat LT 1.25 THEN dbs.mdyn_method = 2
      IF STRPOS(dbs.name,':S') GT 0 THEN dbs.mdyn_method = 2
      junk = WHERE(STRTRIM(badlist,2) EQ STRTRIM(dbs.object,2),badcount)
      IF badcount GT 0 THEN dbs.mdyn_method = 0

      IF dbs.mdyn_method EQ 1 THEN BEGIN
        vcirc = 0.5*W50[refindex[0]]/sini
        dbs.logmdyn = ALOG10((vcirc^2/G) * dbs.radius_f * 1E3)
        r90 = dbs.r90_r_t
        IF r90 LT 0.0 THEN r90 = dbs.rmax_f
        dbs.logmdyn_90 = ALOG10((vcirc^2/G) * (r90/3600.0 * !dtor * dbs.distance * 1E6))
      ENDIF ELSE BEGIN
; Note: if the data is reworked or added to, the constants here will
; need to be altered.
        loglr = (dbs.mabs_r0_t-4.64)/(-2.5)
        dbs.logmdyn = 0.794266*loglr + 2.82815
        dbs.logmdyn_90 = 0.854185*loglr + 2.05562
      ENDELSE

      IF flux_s_iso[netindex[ii]] GT 0.0 THEN BEGIN
        dbs.logf_ha_s = ALOG10(flux_s_iso[netindex[ii]]*stellabs) + $
                      (dbs.niicorr+dbs.a_n_gal)/2.5
        dbs.logf_ha0_s = dbs.logf_ha_s + dbs.a_n_int/2.5
        dbs.err_logf_ha_s_sky = err_flux_s_sky_iso[netindex[ii]] / (flux_s_iso[netindex[ii]] * al10)
        dbs.err_logf_ha_s_cont = err_flux_s_cont_iso[netindex[ii]] / (flux_s_iso[netindex[ii]] * al10)
        dbs.err_logf_ha_s  = SQRT(dbs.err_logf_ha_s_sky^2 + dbs.err_logf_ha_s_cont^2 + err_logf_ha_cal^2)
        dbs.logl_ha_s = dbs.logf_ha_s + ALOG10(4.d0*!pi*distcm^2.d0)
        dbs.logl_ha0_s = dbs.logl_ha_s + dbs.a_n_int/2.5
      ENDIF

      IF flux_f_iso[netindex[ii]] GT 0.0 THEN BEGIN
        dbs.logf_ha_f = ALOG10(flux_f_iso[netindex[ii]]*stellabs) + $
                      (dbs.niicorr+dbs.a_n_gal)/2.5
        dbs.logf_ha0_f = dbs.logf_ha_f + dbs.a_n_int/2.5
        dbs.err_logf_ha_f_sky = err_flux_f_sky_iso[netindex[ii]] / (flux_f_iso[netindex[ii]] * al10)
        dbs.err_logf_ha_f_cont = err_flux_f_cont_iso[netindex[ii]] / (flux_f_iso[netindex[ii]] * al10)
        dbs.err_logf_ha_f  = SQRT(dbs.err_logf_ha_f_sky^2 + dbs.err_logf_ha_f_cont^2 + err_logf_ha_cal^2)
        dbs.logl_ha_f = dbs.logf_ha_f + ALOG10(4.d0*!pi*distcm^2.d0)
        dbs.logl_ha0_f = dbs.logl_ha_f + dbs.a_n_int/2.5
      ENDIF

      IF flux_t_iso[netindex[ii]] GT 0.0 THEN BEGIN
        dbs.logf_ha_t = ALOG10(flux_t_iso[netindex[ii]]*stellabs) + $
                      (dbs.niicorr+dbs.a_n_gal)/2.5
        dbs.logf_ha0_t = dbs.logf_ha_t + dbs.a_n_int/2.5
        dbs.err_logf_ha_t_sky = err_flux_t_sky_iso[netindex[ii]] / (flux_t_iso[netindex[ii]] * al10)
        dbs.err_logf_ha_t_cont = err_flux_t_cont_iso[netindex[ii]] / (flux_t_iso[netindex[ii]] * al10)
        dbs.err_logf_ha_t  = SQRT(dbs.err_logf_ha_t_sky^2 + dbs.err_logf_ha_t_cont^2 + err_logf_ha_cal^2)
        dbs.logl_ha_t = dbs.logf_ha_t + ALOG10(4.d0*!pi*distcm^2.d0)
        dbs.logl_ha0_t = dbs.logl_ha_t + dbs.a_n_int/2.5
      ENDIF

      IF se_f_brt[netindex[ii]] GT 0.0 THEN BEGIN
        dbs.logse_ha_f = ALOG10(se_f_brt[netindex[ii]]*stellabs) + $
                        (dbs.niicorr+dbs.a_n_gal)/2.5
        dbs.logse_ha0_f = dbs.logse_ha_f + dbs.a_n_int/2.5
        dbs.err_logse_ha_f_sky = err_se_f_sky_brt[netindex[ii]] / (se_f_brt[netindex[ii]]*al10)
        dbs.err_logse_ha_f_cont = err_se_f_cont_brt[netindex[ii]] / (se_f_brt[netindex[ii]]*al10)
        dbs.err_logse_ha_f = SQRT(dbs.err_logse_ha_f_sky^2 + dbs.err_logse_ha_f_cont^2 + err_logf_ha_cal^2)
      ENDIF

      IF se_t_brt[netindex[ii]] GT 0.0 THEN BEGIN
        dbs.logse_ha_t = ALOG10(se_t_brt[netindex[ii]]*stellabs) + $
                        (dbs.niicorr+dbs.a_n_gal)/2.5
        dbs.logse_ha0_t = dbs.logse_ha_t + dbs.a_n_int/2.5
        dbs.err_logse_ha_t_sky = err_se_t_sky_brt[netindex[ii]] / (se_t_brt[netindex[ii]]*al10)
        dbs.err_logse_ha_t_cont = err_se_t_cont_brt[netindex[ii]] / (se_t_brt[netindex[ii]]*al10)
        dbs.err_logse_ha_t = SQRT(dbs.err_logse_ha_t_sky^2 + dbs.err_logse_ha_t_cont^2 + err_logf_ha_cal^2)
      ENDIF

      IF STRTRIM(flag_o[netindex[ii]],2) EQ "T" AND flux_o[netindex[ii]] GT 0.0 THEN BEGIN
        dbs.flag_o = "T"
        dbs.logf_ha_o = ALOG10(flux_o[netindex[ii]]*stellabs) + $
                      (dbs.niicorr+dbs.a_n_gal)/2.5
        dbs.logf_ha0_o = dbs.logf_ha_o + dbs.a_n_int/2.5
        dbs.err_logf_ha_o_sky = err_flux_o_sky[netindex[ii]] / (flux_o[netindex[ii]] * al10)
        dbs.err_logf_ha_o_cont = err_flux_o_cont[netindex[ii]] / (flux_o[netindex[ii]] * al10)
        dbs.err_logf_ha_o = SQRT(dbs.err_logf_ha_o_sky^2 + dbs.err_logf_ha_o_cont^2 + err_logf_ha_cal^2)
        dbs.logl_ha_o = dbs.logf_ha_o + ALOG10(4.d0*!pi*distcm^2.d0)
        dbs.logl_ha0_o = dbs.logl_ha_o + dbs.a_n_int/2.5
        IF se_o[netindex[ii]] GT 0.0 THEN BEGIN
          dbs.logse_ha_o = ALOG10(se_o[netindex[ii]]*stellabs) + $
                          (dbs.niicorr+dbs.a_n_gal)/2.5
          dbs.logse_ha0_o = dbs.logse_ha_o + dbs.a_n_int/2.5
          dbs.err_logse_ha_o_sky = err_se_o_sky[netindex[ii]] / (se_o[netindex[ii]]*al10)
          dbs.err_logse_ha_o_cont = err_se_o_cont[netindex[ii]] / (se_o[netindex[ii]]*al10)
          dbs.err_logse_ha_o = SQRT(dbs.err_logse_ha_o_sky^2 + dbs.err_logse_ha_o_cont^2)
        ENDIF
      ENDIF ELSE BEGIN
        dbs.flag_o = "F"
      ENDELSE

      IF re_f_brt[rindex[0]] GT 0.0 THEN BEGIN
        dbs.mu_e_r_f = dbs.mapp_r_f + 2.5*(ALOG10(2.0*!pi*re_f_brt[rindex[0]]^2)) - $
                      (dbs.a_r_gal)
        dbs.mu_e_r0_f = dbs.mu_e_r_f - dbs.a_r_int
; Obviously, we don't need the continuum error for an R image.
        dbs.err_mu_e_r_f = 2.5 * SQRT((err_se_f_sky_brt[rindex[0]] / (se_f_brt[rindex[0]]*al10))^2 + err_logf_r_cal^2)
      ENDIF

      IF re_t_brt[rindex[0]] GT 0.0 THEN BEGIN
        dbs.mu_e_r_t = dbs.mapp_r_t + 2.5*(ALOG10(2.0*!pi*re_t_brt[rindex[0]]^2)) - $
                      (dbs.a_r_gal)
        dbs.mu_e_r0_t = dbs.mu_e_r_t - dbs.a_r_int
        dbs.err_mu_e_r_t = 2.5 * SQRT((err_se_t_sky_brt[rindex[0]] / (se_t_brt[rindex[0]]*al10))^2 + err_logf_r_cal^2)
      ENDIF

      IF numgals[netindex[ii]] GT 1 THEN BEGIN
        totflux_s = 0.0
        totflux_f = 0.0
        totflux_t = 0.0
        totflux_s_dust = 0.0
        totflux_f_dust = 0.0
        totflux_t_dust = 0.0
        FOR kk = 0,numgals[netindex[ii]]-1 DO BEGIN
          index = WHERE(STRTRIM(fobject,2) EQ STRTRIM(fobject[netindex[ii]],2) AND $ 
                        galindex EQ kk+mingal AND STRTRIM(fimtype,2) EQ "net",count)
          IF count EQ 0 THEN BEGIN
            PRINT,"ERROR in build_derived_db: can't match source ",dbs.name,kk
            RETURN
          ENDIF
          totflux_s = totflux_s + MEAN(flux_s_iso[index])*nunred[refindex[0]]*(1.d0-niicorr[kk,0])*stellabs
          totflux_s_dust = totflux_s_dust + MEAN(flux_s_iso[index])*nunred[refindex[0]]* $
                                        (1.d0-niicorr[kk,0])*10.0^(dustcorr[kk,0]/2.5)*stellabs
          totflux_f = totflux_f + MEAN(flux_f_iso[index])*nunred[refindex[0]]*(1.d0-niicorr[kk,0])*stellabs
          totflux_f_dust = totflux_f_dust + MEAN(flux_f_iso[index])*nunred[refindex[0]]* $
                                        (1.d0-niicorr[kk,0])*10.0^(dustcorr[kk,0]/2.5)*stellabs
          totflux_t = totflux_t + MEAN(flux_t_iso[index])*nunred[refindex[0]]*(1.d0-niicorr[kk,0])*stellabs
          totflux_t_dust = totflux_t_dust + MEAN(flux_t_iso[index])*nunred[refindex[0]]* $
                                        (1.d0-niicorr[kk,0])*10.0^(dustcorr[kk,0]/2.5)*stellabs
;; Logic problem: if multiple galaxies exist in an image we used
;; multiple filters on, does it do it correctly?
        ENDFOR
      ENDIF ELSE BEGIN
        totflux_s = 10.0^dbs.logf_ha_s
        totflux_f = 10.0^dbs.logf_ha_f
        totflux_t = 10.0^dbs.logf_ha_t
        totflux_s_dust = 10.0^dbs.logf_ha0_s
        totflux_f_dust = 10.0^dbs.logf_ha0_f
        totflux_t_dust = 10.0^dbs.logf_ha0_t
      ENDELSE

      dbs.frac_ha_s = 10.0^dbs.logf_ha_s / totflux_s
      dbs.frac_ha_f = 10.0^dbs.logf_ha_f / totflux_f
      dbs.frac_ha_t = 10.0^dbs.logf_ha_t / totflux_t
      dbs.frac_ha0_s = 10.0^dbs.logf_ha0_s / totflux_s_dust
      dbs.frac_ha0_f = 10.0^dbs.logf_ha0_f / totflux_f_dust
      dbs.frac_ha0_t = 10.0^dbs.logf_ha0_t / totflux_t_dust

      tbase = (2.43E-18 * 10.0^dbs.logmhi) / dbs.distance^2.d0
      dbs.tgas_s = tbase / totflux_s
      dbs.tgas_f = tbase / totflux_f
      dbs.tgas_t = tbase / totflux_t
      dbs.tgas0_s = tbase / totflux_s_dust
      dbs.tgas0_f = tbase / totflux_f_dust
      dbs.tgas0_t = tbase / totflux_t_dust

      corrfactor = 10.0^(0.4*(dbs.a_n_int - dbs.a_r_int))
      IF ew50_f_brt[netindex[ii]] GT 0.0 THEN BEGIN
        dbs.ew50_f = ew50_f_brt[netindex[ii]] * (1.d0-niicorr[galind,0]) * stellabs
        dbs.ew50_0_f = dbs.ew50_f * corrfactor * stellabs
        dbs.err_ew50_f = err_ew50_f_brt[netindex[ii]] * (1.d0-niicorr[galind,0]) * stellabs
        dbs.err_ew50_0_f = dbs.err_ew50_f * corrfactor * stellabs
      ENDIF ELSE BEGIN
        dbs.ew50_f = -999.0
        dbs.ew50_0_f = -999.0
        dbs.err_ew50_f = -999.0
        dbs.err_ew50_0_f = -999.0
      ENDELSE

      IF ew50_t_brt[netindex[ii]] GT 0.0 THEN BEGIN
        dbs.ew50_t = ew50_t_brt[netindex[ii]] * (1.d0-niicorr[galind,0]) * stellabs
        dbs.ew50_0_t = dbs.ew50_t * corrfactor * stellabs
        dbs.err_ew50_t = err_ew50_t_brt[netindex[ii]] * (1.d0-niicorr[galind,0]) * stellabs
        dbs.err_ew50_0_t = dbs.err_ew50_t * corrfactor * stellabs
      ENDIF ELSE BEGIN
        dbs.ew50_t = -999.0
        dbs.ew50_0_t = -999.0
        dbs.err_ew50_t = -999.0
        dbs.err_ew50_0_t = -999.0
      ENDELSE

; When storing entry numbers, add 1 to make sure it's 1-N instead of
; the usual 0-(N-1)
      dbs.entry_sample = LONG(refindex[0])+LONG(1)
      dbs.entry_hdr_r = LONG(Rheadindex[0])+LONG(1)
      dbs.entry_hdr_n = LONG(Nheadindex[0])+LONG(1)
      dbs.entry_hdr_s = LONG(Sheadindex[0])+LONG(1)
      dbs.entry_flux_r = LONG(rindex[0])+LONG(1)
      dbs.entry_flux_n = LONG(nindex[0])+LONG(1)
      dbs.entry_flux_s = LONG(netindex[ii])+LONG(1)

;;print,dbs.runid,dbs.name+' ',dbs.filter_r,dbs.filter_n,flux_f_iso[netindex[ii]]

; Add the structure line to the database
;;      IF flux_f_iso[netindex[ii]] GT 0.0 THEN $ ;; quick patch
      singg_dbupdate,newdb,["NAME","FILTER_R","FILTER_N","RUNID"],[[dbs.name],[dbs.filter_r],[dbs.filter_n],[dbs.runid]],dbs
    ENDELSE
  ENDFOR

  PRINT,"Finished creating derived database"

  dbopen,newdb,1
  dbindex
  dbclose,dummy

  RETURN

END
