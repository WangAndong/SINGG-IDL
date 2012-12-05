PRO hdfs_pages_oct02
   outdir   = 'plots/'
   pstmp    = 'stmp_'
   pspcc    = 'spcc_'
   pspcf    = 'spcf_'
   pribn    = 'ribn_'
   pscii    = 'spec_'
   workdir  = '/data3/acs27/meurer/grism/hdfs/axe'
   filimg   = '/data3/acs27/meurer/grism/hdfs/apsis/detectionImage.fits'
   filgrim  = '/data3/acs27/meurer/grism/hdfs/apsis/j8eb03g800l_drz_sci.fits'
   filspc   = 'j8eb03g800l_drz_sci_1.SPC.fits'
   filrib   = 'j8eb03g800l_drz_sci_1.STP.fits'
   filcat   = 'j8eb03g800l_drz_sci_1.cat'
   filcato  = 'j8eb03g800l_drz_sci_1.revised.cat'
   ; filsens  = '/data3/acs27/meurer/grism/sens_G800L_wfc_smov.fits'
   filsens  = '/data3/acs27/meurer/grism/sensG800Lwfc_gd153_ave.fits'
   pgpfx    = 'hdfs_all_'
   extsens  = 1
   pgpfx    = 'hdfs_all_'
   lrange   = [5800.0, 9400.0]
   dark     = 8.0/3600.0
   covar    = 1.62
   rowsperpage = 50
   ;
   cd, workdir
   ;
   print,'Reading direct image header...'
   fits_read,filimg,dum,himg,/header_only
   ;
   print,'Setting error model ... '
   fits_read,filgrim,dum,hgrim,/header_only
   exptime = sxpar(hgrim, 'EXPTIME')
   seterrmod2, hgrim, dark=dark, covar=covar, /verbose
   setsensmod, filsens, extsens
   ;
   print,'Making plots & data files ... '
   ; mkall_grplots, filimg, filspc, filrib, filcat, lrange, outdir, /kludge_mrd, exptime=exptime
   ; mkall_grplots, filimg, filspc, filrib, filcat, lrange, outdir, exptime=exptime
   ;
   print,'Reading direct catalog, converting positions...'
   rd_grdirct_cat, filcat, id, xim, yim, magauto, aim, bim, thetaim, w50, class
   xyad, himg, xim, yim, radeg, decdeg
   rdstr = adstring(radeg, decdeg, 2)
   cd,outdir
   print,'Writing revised catalog ... '
   wr_grcat, filcato, id, xim, yim, magauto, aim, bim, thetaim, w50, class, $
             radeg, decdeg, rdstr
   print,'Making web pages ... '
   posim       = [[xim], [yim]] 
   npage       = 1
   i0          = 0
   WHILE i0 LE n_elements(id)-1 DO  BEGIN 
      i1       = min([i0 + rowsperpage,n_elements(id)])-1
      elm      = i0 + indgen(i1-i0+1)
      filhtml  = pgpfx + trim(string(npage),2) + '.html'
      title    = 'Grism spectra - page : ' + trim(string(npage),2) 
      grism_page_oct02, filhtml, title, elm, id, posim, aim, bim, thetaim, $
       w50, magauto, class, radeg, decdeg, rdstr, pstmp, pspcf, pspcc, pribn, pscii
      npage    = npage + 1
      i0       = i1 + 1
      print, ' '
      print, ' completed page : ', filhtml
      print, ' '
   ENDWHILE 
   
END 

PRO hdfs_pages_aug02
   workdir = '/data3/acs27/meurer/grism/hdfs/axe'
   filimg = '../apsis/detectionImage.fits'
   filspc = ['j8eb03g800l_drz_sci_0.SPC.fits', 'j8eb03g800l_drz_sci_1.SPC.fits']
   filrib = ['j8eb03g800l_drz_sci_0.STP.fits', 'j8eb03g800l_drz_sci_1.STP.fits']
   filcat = ['j8eb03g800l_drz_sci_0.cat', 'j8eb03g800l_drz_sci_1.cat']
   pgpfx  = ['hdfs_all_0_', 'hdfs_all_1_']
   outdir = 'plots/'
   ;
   seterrmod, 4.85, 1.0, (8.0/3600.0), 4000.0, 4, 381.9
   ;
   FOR  k = 0, 1 DO BEGIN 
      cd,workdir
      mkall_grplots_aug02, filimg, filspc[k], filrib[k], filcat[k], outdir 
      rd_grdirct_cat, filcat[k], id, xim, yim, magauto, aim, bim, thetaim, w50, class
      cd,outdir
      posim = [[xim], [yim]] 
      pstmp = 'stmp_'
      pribn = 'ribn_'
      pspec = 'spec_'
   ;
      rowsperpage = 100
      npage       = 1
      i0          = 0
      WHILE i0 LE n_elements(id)-1 DO  BEGIN 
         i1       = min([i0 + rowsperpage,n_elements(id)])-1
         elm      = i0 + indgen(i1-i0+1)
         filhtml  = pgpfx[k] + trim(string(npage),2) + '.html'
         title    = 'Grism spectra - page : ' + trim(string(npage),2) 
         grism_page, filhtml, title, elm, id, posim, aim, bim, thetaim, $
          w50, magauto, class,  pstmp, pspec, pribn
         npage    = npage + 1
         i0       = i1 + 1
         print, ' '
         print, ' completed page : ', filhtml
         print, ' '
      ENDWHILE 
   ENDFOR 
END 
