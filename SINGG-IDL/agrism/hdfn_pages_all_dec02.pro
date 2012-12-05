PRO hdfn_pages_all_dec02
   outdir   = 'plots/'
   pstmp    = 'stmp_'
   pspcc    = 'spcc_'
   pspcf    = 'spcf_'
   pribn    = 'ribn_'
   pscii    = 'spec_'
   workdir  = '/data3/acs27/meurer/grism/hdfn_dec02/Axe/output/'
   filimg   = '/data3/acs27/meurer/grism/hdfn_dec02/Apsis/detectionImage.fits'
   filgrim  = '/data3/acs27/meurer/grism/hdfn_dec02/Apsis/j8eb01g800l_drz_sci.fits'
   filspc   = 'hdfn_g800l_2.SPC.fits'
   filrib   = 'hdfn_g800l_2.STP.fits'
   filcat   = 'hdfn_g800l_2.cat'
   ; filsens  = '/data3/acs27/meurer/grism/sens_G800L_wfc_smov.fits'
   filsens  = '/data3/acs27/meurer/grism/sensG800Lwfc_gd153_ave.fits'
   pgpfx    = 'hdfn_all_'
   extsens  = 1
   pgpfx    = 'hdfn_all_'
   lrange   = [5800.0, 9800.0]
   dark     = 8.0/3600.0
   covar    = 1.0
   rowsperpage = 50
   ;
   cd, workdir
   ;
   print,'Setting error model ... '
   fits_read,filgrim,dum,hgrim,/header_only
   exptime = sxpar(hgrim, 'EXPTIME')
   seterrmod2, hgrim, dark=dark, covar=covar, /verbose
   setsensmod, filsens, extsens
   ;
   print,'Making plots & data files ... '
   mkall_grplots, filimg, filspc, filrib, filcat, lrange, outdir, exptime=exptime
   ;
   print,'Making web pages ... '
   rd_grdirct_cat, filcat, id, xim, yim, magauto, aim, bim, thetaim, w50, class
   cd,outdir
   posim       = [[xim], [yim]] 
   npage       = 1
   i0          = 0
   WHILE i0 LE n_elements(id)-1 DO  BEGIN 
      i1       = min([i0 + rowsperpage,n_elements(id)])-1
      elm      = i0 + indgen(i1-i0+1)
      filhtml  = pgpfx + trim(string(npage),2) + '.html'
      title    = 'Grism spectra - page : ' + trim(string(npage),2) 
      grism_page_sep02, filhtml, title, elm, id, posim, aim, bim, thetaim, $
       w50, magauto, class,  pstmp, pspcf, pspcc, pribn, pscii
      npage    = npage + 1
      i0       = i1 + 1
      print, ' '
      print, ' completed page : ', filhtml
      print, ' '
   ENDWHILE 
END 

