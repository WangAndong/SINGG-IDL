PRO test_sr2web_objpage
   computer = 'singgpb1'
   sufpng   = ['sqrt1', 'sqrt3', 'sqrt8', 'lin8', 'log1']
   targ     = 'J2152-55'
   fnet     = ['J2152-55_Rsub_ss.fits']
   fcnt     = ['J2152-55_R_ss.fits']
   fnb      = ['J2152-55_6628_ss.fits']
   fcat     = ['J2152-55_Rsub_ss_r04_catcalib.dat']
   rootlong = 'J2152-55_Rsub_ss_r04'
   rootshrt = 'J2152-55_r04'
   filt_cnt = ['R']
   filt_nb  = ['6628/33']
   runid    = '04'
   IF keyword_set(computer) THEN BEGIN 
      CASE computer OF 
         'acs36'    : BEGIN 
                         baseout  = '/data2/acs36/meurer/SINGG/SR2_QA/'
                         baseimg  = '/data2/acs36/meurer/SINGG/SR2_QA/Images/'
                         basedata = '/data2/acs36/meurer/SINGG/SR2_QA/Data/'
                      END
         'singgpb1' : BEGIN
                         baseout  = '/Users/meurer/SINGG/SR2_QA/'
                         baseimg  = '/Users/meurer/SINGG/SR2_QA/Images/'
                         basedata = '/Users/meurer/SINGG/SR2_QA/Data/'
                      END
         'sungg'    : BEGIN 
                         baseout  = '/data1/sungg/www/protected/SINGG/SR2QA/'
                         baseimg  = '/data1/sungg/www/protected/SINGG/SR2QA/Images/'
                         basedata = '/data1/sungg/www/protected/SINGG/SR2QA/Data/'
                      END
         ELSE       : BEGIN
                         baseout  = '/Users/meurer/SINGG/SR2_QA/'
                         baseimg  = '/Users/meurer/SINGG/SR2_QA/Images/'
                         basedata = '/Users/meurer/SINGG/SR2_QA/Data/'
                      END
      ENDCASE 
   ENDIF 
   ;
   sr2web_objpage, targ, baseout, baseimg, basedata, sufpng, fnet, fcnt, fnb, fcat, $
                    rootlong, rootshrt, runid, filt_cnt, filt_nb, filo
   print, filo
END 
