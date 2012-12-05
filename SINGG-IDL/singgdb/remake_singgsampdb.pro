PRO remake_singgsampdb
   ;
   ; remake singg sample database and correlate with 
   ; hipass databases
   ;
   ; G. Meurer 08/2004
   ;
   rmatch  = 4.0                      ; match rad arcmin
   vmatch  = 100.0                    ; match vel. km/s
   odb     = 'singg_sample'           ; old database
   rdb     = 'hipass_remeasure'       ; remeasure database
   hdb     = 'hicat_feb04'            ; HICAT database
   bdb     = 'hipass_bgc'             ; BGC database
   sdb     = 'hipass_sccc'            ; SCCC database
   adb     = 'hipass_avcc'            ; AVOC database
   ;
   ; read in old database
   dbopen, odb
   dbext,-1,'name,optid,catalog,ra,dec,glat,glong,vhel,vlg,vcmb,vtonry,vshap', $
             name_old,optid_old,catalog_old,ra_old,dec_old,glat_old,glong_old,$
             vhel_old,vlg_old,vcmb_old,vtonry_old,vshap_old
   dbext,-1,'w50,sp,sint,distance,logmhi,observed,ebv', $
             w50_old,sp_old,sint_old,distance_old,logmhi_old,observed_old,ebv_old
END 
