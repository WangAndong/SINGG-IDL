PRO a1689_pages
   outdir   = 'plots/'
   pstmp    = 'stmp_'
   pspcc    = 'spcc_'
   pspcf    = 'spcf_'
   pribn    = 'ribn_'
   pscii    = 'spec_'
   workdir  = '/data3/acs27/meurer/grism/a1689/axe/'
   filimg   = '/data3/acs27/meurer/grism/a1689/apsis/detectionImage.fits'
   filgrim  = '/data3/acs27/meurer/grism/a1689/apsis/j8e952g800l_drz_sci.fits'
   filspc   = 'j8e952g800l_drz_sci_1.SPC.fits'
   filrib   = 'j8e952g800l_drz_sci_1.STP.fits'
   filcat   = 'j8e952g800l_drz_sci_1.cat'
   filsens  = '/data3/acs27/meurer/grism/sensG800Lwfc_gd153_ave.fits'
   extsens  = 1
   pgpfx    = 'a1689_'
   lrange   = [5800.0, 9400.0]
   dark     = 8.0/3600.0
   covar    = 1.62
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
   mkall_grplots_0, filimg, filspc, filrib, filcat, lrange, $
    outdir+pstmp, outdir+pspcc, outdir+pspcf, outdir+pribn, outdir+pscii, exptime=exptime
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
      title    = 'Grism spectra Abell1689 - page : ' + trim(string(npage),2) 
      grism_page_sep02, filhtml, title, elm, id, posim, aim, bim, thetaim, $
       w50, magauto, class,  pstmp, pspcf, pspcc, pribn, pscii
      npage    = npage + 1
      i0       = i1 + 1
      print, ' '
      print, ' completed page : ', filhtml
      print, ' '
   ENDWHILE 
   
END 

