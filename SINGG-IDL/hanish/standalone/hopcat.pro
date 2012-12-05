PRO hopcat

  dbopen,"singg_derived",0
  dbext,-1,"NAME,OBJECT,OPTID,SOURCE,RA,DEC,MAPP_R_T,RE_T_R,LOGFHA_T,RE_T_HA",$
            NAME,OBJECT,OPTID,SOURCE,RA,DEC,MAPP_R_T,RE_T_R,LOGFHA_T,RE_T_HA
  dbext,-1,"DISTANCE,A_R_GAL,A_N_GAL,A_R_INT,A_N_INT,MABS_R0_T,LOGLHA0_T,UPDATE",$
            DISTANCE,A_R_GAL,A_N_GAL,A_R_INT,A_N_INT,MABS_R0_T,LOGLHA0_T,UPDATE
  dbclose
  fobj = update_name(object)

  openw,unit,"~/data/hopcat_table.txt",/GET_LUN
  PRINTF,unit,"# NAME      OBJECT    OPTID                         SOURCE   RA    DEC      MAPP_R  RE_R  LOGFHA  RE_HA   DIST  A_R_G  A_N_G  A_R_I  A_N_I MABS_R0 LOGLHA UPDATE"

  FOR ii = 0,N_ELEMENTS(name)-1 DO BEGIN
    PRINTF,unit,NAME[ii],OBJECT[ii],OPTID[ii],SOURCE[ii],RA[ii],DEC[ii], $
                MAPP_R_T[ii],RE_T_R[ii],LOGFHA_T[ii],RE_T_HA[ii],DISTANCE[ii],$
                A_R_GAL[ii],A_N_GAL[ii],A_R_INT[ii],A_N_INT[ii],MABS_R0_T[ii],$
                LOGLHA0_T[ii],UPDATE[ii], $
                FORMAT='(A12,A10,A30,A8,2(F6.2," "),F7.2,F8.2,F7.2,F8.2,F7.2,4(F6.3," "),F7.2,F6.2,"  ",A30)'
  ENDFOR

  close,/all
  free_lun,unit

END
