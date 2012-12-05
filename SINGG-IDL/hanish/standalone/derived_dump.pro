PRO derived_dump

  dbopen,"singg_derived",0
  dbext,-1,"NAME,OPTID,RA,DEC,DISTANCE,LOGMHI,FILTER_R,FILTER_N,EW50_T", $
            NAME,OPTID,RA,DEC,DISTANCE,LOGMHI,FILTER_R,FILTER_N,EW50_T
  dbext,-1,"LOGL_HA_T,LOGL_R_T,NIICORR,A_N_GAL,A_N_INT,RMAX_F,RE_R_T,RE_HA_T,PA,AXERAT,MABS_R_T", $
            LOGL_HA_T,LOGL_R_T,NIICORR,A_N_GAL,A_N_INT,RMAX_F,RE_R_T,RE_HA_T,PA,AXERAT,MABS_R_T
  dbext,-1,"LOGMDYN,LOGMDYN_90,MDYN_METHOD,RADIUS_F,R90_HA_T", $
            LOGMDYN,LOGMDYN_90,MDYN_METHOD,RADIUS_F,R90_HA_T
            
;  derind = good_derived()-1
  derind = INDGEN(N_ELEMENTS(name))

  dbclose

  n_gals = N_ELEMENTS(derind)

  dbopen,"singg_sample",0
  dbext,-1,"NAME,    W50", $
            sampname,W50
  dbclose
  sampname = update_name(sampname)

  G = (1.0/232.47) ; (km/s)^2 pc Msolar^-1
  q0 = 0.22

  OPENW,unit,"~/data/singg_observed.txt",/GET_LUN
  PRINTF,unit,"# Name                         opt ID       RA    DEC      Dist  logMHI R_filt   N_filt  logLHa logLR logLR NII   A_gal A_int  Rmax     R50(R) R50(Ha) PA    axerat EW",FORMAT='(A)'

  OPENW,unit2,"~/data/singg_mdyn.txt",/GET_LUN
  PRINTF,unit2,"# Name          Mdyn     Mdyn90 v_circ  W_50  Method"

  logl_r = (mabs_r_t-4.64)/(-2.5) ; in solar units

  FOR ii = 0,n_gals-1 DO BEGIN
    jj = derind[ii]
;    sini = SQRT((1.0 - (1.0/axerat[jj])^2)/(1.0 - q0^2)) < 1.0
;    inc = ASIN(sini)/!dtor
    PRINTF,unit,NAME[jj],STRTRIM(OPTID[jj],2),RA[jj],DEC[jj],DISTANCE[jj], $
                LOGMHI[jj],STRTRIM(FILTER_R[jj],2),STRTRIM(FILTER_N[jj],2), $
                LOGL_HA_T[jj],LOGL_R_T[jj],logl_r[jj],NIICORR[jj], $
                A_N_GAL[jj],A_N_INT[jj],RMAX_F[jj],RE_R_T[jj],RE_HA_T[jj], $
                PA[jj],axerat[jj],ew50_t[jj],FORMAT='(A12,A30,F7.2,F7.2,F7.2,F7.3,A10,A8,F6.2,F6.2,F6.2,F7.3,F6.3,F6.3,3F8.2,F7.2,F6.3,F7.2)'

    ind = WHERE(STRTRIM(sampname,2) EQ STRTRIM(name[jj],2),count)
    IF count GT 0 THEN BEGIN
      sini = SQRT((1.0 - (1.0/axerat[jj])^2)/(1.0 - q0^2)) < 1.0
      v50 = 0.5 * W50[ind[0]] / MIN([sini,1.0]) ; in km/s

      PRINTF,unit2,NAME[jj],logmdyn[jj],logmdyn_90[jj],v50,W50[ind[0]],mdyn_method[jj],FORMAT='(A12,2F9.3,F7.2,F7.1,"  ",I1)'
    ENDIF

  ENDFOR

  close,/all
  free_lun,unit
  free_lun,unit2

END
