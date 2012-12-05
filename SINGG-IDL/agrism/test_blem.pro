PRO test_blem
   filgr = 'j8eb01g800l_drz_sci.fits'
   filgf = 'j8eb01g800l_sh133_msk.fits'
   fildr = 'detectionImage.fits'
   fildf = 'detectionImage_sharp133.fits'
   filsg = 'detectionImage_SEGM.fits'
   catgf = 'j8eb01g800l_sh133_msk_eda_12.cat'
   dxmed = 13
   abcdef = [-111.170174320, 1.002702559, -0.002938893, -1.605385306, 0.000471192, 0.999927372]
   dxbeam = [-10, 150]
   apmin  = 5.0
   fwag   = 2.5
   szstmp = 50
   dzgr    = [-20, 500]
   dzgf    = [-50, 100]
   dzdr    = [-20, 300]
   dzdf    = [-50, 160]
   pfx1d   = 'extract1d_'
   pfxgs   = 'extract2d_'
   pfxcc   = 'ccresults_'
   filout  = 'hdfn_blem.out'
   thresh_rms = 3.0
   dldp_a_0   = [4764.93, 0.00126154, -0.00221065]
   dldp_a_1   = [42.4377, 0.000795975, -0.00110326]
   aper       = 5
   ;filsens    = '/home/meurer/acs/axe/conf/ACS.WFC.1st.sens.fits' 
   filsens    = 'ACS.WFC.1st.sens.fits' 
   extsens    = 1

   blind_emfind, filgr, filgf, fildr, fildf, filsg, catgf, $
    dldp_a_0, dldp_a_1, aper, filsens, extsens, $
    dxmed, abcdef, dxbeam, thresh_rms, apmin, fwag, szstmp, $
    dzgr, dzgf, dzdr, dzdf, pfx1d, pfxgs, pfxcc, filout, /invmat

END 
