PRO temp2

  dbopen,'singg_flux',0
  dbext,-1,'object,galindex,imtype,flux_t_iso,err_flux_t_sky_iso,err_flux_t_cont_iso,flux_scale',a,b,c,d,e,f,g
  dbclose

  ind = where(strtrim(a,2) eq 'J1051-17' AND b EQ 2 AND strtrim(c,2) eq 'net')
  forprint,a[ind],b[ind],c[ind],d[ind]/g[ind],e[ind]/g[ind],f[ind]/g[ind]

  dbopen,'singg_derived',0
  dbext,-1,'NAME,LOGF_HA_T,ERR_LOGF_HA_T_SKY,ERR_LOGF_HA_T_CONT',a,b,c,d
  dbclose

  ind = where(strtrim(a,2) eq 'J1051-17:S2')
  forprint,a[ind],b[ind],c[ind],d[ind]

end
