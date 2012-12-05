PRO hdfn_all_1
   cd,'/data3/acs27/meurer/spec/output'
   filimg  = '../detectionImage.fits'
   filspc = 'j8eb01g800l_drz_sci_1.SPC.fits'
   filrib = 'j8eb01g800l_drz_sci_1.STP.fits'
   filcat = 'j8eb01g800l_drz_sci_1.cat'
   outdir = 'plots/'
   mkall_grplots_0, filimg, filspc, filrib, filcat, outdir
   rd_grdirct_cat, filcat, id, xim, yim, magauto, aim, bim, thetaim, w50, class
   cd,'plots'
   posim = [[xim], [yim]] 
   pstmp = 'stmp_'
   pribn = 'ribn_'
   pspec = 'spec_'
   ;
   pageprefix  = 'hdfn_all_1_'
   rowsperpage = 100
   npage       = 1
   i0          = 0
   WHILE i0 LE n_elements(id)-1 DO  BEGIN 
      i1       = min([i0 + rowsperpage,n_elements(id)])-1
      elm      = i0 + indgen(i1-i0+1)
      filhtml  = pageprefix + trim(string(npage),2) + '.html'
      title    = 'Grism spectra - page : ' + trim(string(npage),2) 
      grism_page, filhtml, title, elm, id, posim, aim, bim, thetaim, $
                  w50, magauto, class,  pstmp, pspec, pribn
      npage    = npage + 1
      i0       = i1 + 1
   ENDWHILE 

END 

