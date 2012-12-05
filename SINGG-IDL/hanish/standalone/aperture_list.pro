PRO aperture_list

  dbopen,"singg_flux",0
  dbext,-1,"OBJECT,GALINDEX,NUMGALS,IMTYPE,PA_IMAGE,PA,AXERAT,FLUXRAD_S_BRT,FLUXRAD_F_BRT",object,galindex,numgals,imtype,pa_i,pa,rat,a_s,a_f

  dbext,-1,"RA,DEC,RA_ISO,DEC_ISO,XCENTER_BRT,YCENTER_BRT,XCENTER_ISO,YCENTER_ISO",ra,dec,ra_i,dec_i,x_b,y_b,x_i,y_i
  dbclose

  ind = WHERE(STRTRIM(imtype,2) EQ 'net',count)

  OPENW,unit,"aperture.dat",/GET_LUN
  PRINTF,unit,"#   name      pa_image pa_wcs  ra_brt dec_brt  ra_iso dec_iso  x_brt  y_brt    x_iso  y_is o   a_sky   b_sky   a_flux  b_flux"

  FOR ii = 0,count-1 DO BEGIN
    IF numgals[ind[ii]] EQ 1 THEN obj = STRTRIM(object[ind[ii]],2) $
                             ELSE obj = STRTRIM(object[ind[ii]],2)+':S'+STRTRIM(STRING(galindex[ind[ii]]),2)

    PRINTF,unit,obj,pa_i[ind[ii]],pa[ind[ii]],ra[ind[ii]],dec[ind[ii]],$
          ra_i[ind[ii]],dec_i[ind[ii]],x_b[ind[ii]],y_b[ind[ii]],$
          x_i[ind[ii]],y_i[ind[ii]],a_s[ind[ii]],a_s[ind[ii]]/rat[ind[ii]],$
          a_f[ind[ii]],a_f[ind[ii]]/rat[ind[ii]],FORMAT='(A12," ",6F8.3,8F8.2)'
  ENDFOR

  CLOSE,unit
  FREE_LUN,unit

END
