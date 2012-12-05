PRO temp

;  read_profile_header,'/data1/acs22/hanish/Run12/Proc4/J0205-55a/J0205-55a_Rsub_ss_isophote.profile',netstr,lstart=lstart,lend=lend,netflag=netflag

;print,netstr.flux_o
;stop

  dbopen,'singg_flux',0
  dbext,-1,'OBJECT,GALINDEX,IMTYPE,FLUX_O,FLUX_T_ISO',a,b,c,d,e
  dbclose

  ind = WHERE(strtrim(a,2) eq 'J0205-55a' and strtrim(c,2) eq 'net',count)
  forprint,a[ind],b[ind],c[ind],d[ind],e[ind]

  RETURN

END
