PRO blem_pears_testdata
   filgr      = 'grism_edgemsk.fits'
   filgf      = 'grism_sharp_msk1.fits'
   fildr      = 'direct_edgemsk.fits'
   fildf      = 'direct_sharp.fits'
   filsg      = 'direct_SEGM.fits'
   catgf      = 'grism_sharp_msk1_02ed.cat'
   catdr      = 'direct_edgemsk_02.cat'
   filsgoodg  = ['grism_ccd1.dat', 'grism_ccd2.dat']
   filsgoodd  = ['direct_ccd1.dat', 'direct_ccd2.dat']
   dxmed      = 13
   abcdef     = [-105.673035610, 1.002578038, -0.003091192, -0.353078513, 0.000706450, 0.999755280]
   dxbeam     = [-10, 150]
   apmin      = 5.0
   fwag       = 2.5
   szstmp     = 50
   dzgr       = [-20, 500]
   dzgf       = [-50, 100]
   dzdr       = [-20, 300]
   dzdf       = [-50, 160]
   pfx1d      = 'extract1d'
   pfxgs      = 'extract2d'
   pfxcc      = 'ccresults'
   filout     = 'pears_testdata_blem3.out'
   thresh_rms = 3.0
   xref       = 2284.0
   yref       = 2168.0
   dldp_a_0   = [4771.42, -0.00121212, -0.00247013, -4.42695e-6, -2.06821e-6, 7.93817e-7]
   dldp_a_1   = [41.3419, 0.000786183, -0.00112364, -2.47549e-8, -2.74095e-8, 2.61703e-8]
   dldp_a_2   = [7.71266e-04, 1.3607e-7, 1.53419e-7, 4.75e-10, -6.3533e-11, 6.58144e-11]
   dydx_a_1   = [-4.5065e-4, -3.52335e-6, 2.1394e-6, 2.17438e-10, -7.81162e-10, 4.49999e-11]
   dydx_a_0   = 0.0*dydx_a_1
   yoff_a     = [-0.765986, 7.31305e-5, 1.24362e-4, -9.86897e-8, 2.01405e-7, 2.8342e-8]
   yoff_a[0]  = yoff_a[0] + 1.95
   aper       = 5
   filsens    = '/home/meurer/ACS/Grism/aXe/conf/ACS.WFC.1st.sens.4.fits'
   outdir     = '/home/meurer/ACS/Grism/PEARS/Testdata/BLEM/'
   awkgrcat   = 'blem_rdsex.awk'
   awkdrcat   = 'blem_rddetimcat.awk'
   ; fil_1stqa  = 'blem_rej2.out'
   extsens    = 1

   blind_emfind, filgr, filgf, fildr, fildf, filsg, catgf, catdr, filsgoodg, filsgoodd, $
    xref, yref, dldp_a_0, dldp_a_1, dldp_a_2, dydx_a_0, dydx_a_1, yoff_a, $
    aper, filsens, extsens, dxmed, abcdef, dxbeam, thresh_rms, apmin, fwag, szstmp, $
    dzgr, dzgf, dzdr, dzdf, pfx1d, pfxgs, pfxcc, filout, outdir=outdir, $
    awkgrcat=awkgrcat, awkdrcat=awkdrcat, /invmat

END 
