PRO cl1252_pages
   outdir   = 'plots/'
   workdir  = '/data3/acs27/meurer/grism/cl1252/axe/'
   apdir    = '/data3/acs27/meurer/grism/cl1252/apsis/'
   filimg   = apdir + ['detectionImage_pos1.fits', 'detectionImage_pos2.fits', 'detectionImage_pos3.fits', 'detectionImage_pos4.fits']
   filgrim  = apdir + ['j8eh22g800l_drz_sci.fits', 'j8eh24g800l_drz_sci.fits', 'j8eh26g800l_drz_sci.fits', 'j8eh28g800l_drz_sci.fits']
   filspc   = ['j8eh22g800l_drz_sci_1.SPC.fits', 'j8eh24g800l_drz_sci_1.SPC.fits', 'j8eh26g800l_drz_sci_1.SPC.fits', 'j8eh28g800l_drz_sci_1.SPC.fits']
   filrib   = ['j8eh22g800l_drz_sci_1.STP.fits', 'j8eh24g800l_drz_sci_1.STP.fits', 'j8eh26g800l_drz_sci_1.STP.fits', 'j8eh28g800l_drz_sci_1.STP.fits']
   filcat   = ['j8eh22g800l_drz_sci_1.cat', 'j8eh24g800l_drz_sci_1.cat', 'j8eh26g800l_drz_sci_1.cat', 'j8eh28g800l_drz_sci_1.cat']
   filsens  = '/data3/acs27/meurer/grism/sensG800Lwfc_gd153_ave.fits'
   extsens  = 1
   lrange   = [5800.0, 9400.0]
   dark     = 8.0/3600.0
   covar    = 1.62
   rowsperpage = 50
   ;
   cd, workdir
   setsensmod, filsens, extsens
   ;
   FOR J = 0, n_elements(filgrim)-1 DO BEGIN 
      cd, workdir
      filg     = filgrim[j]
      fili     = filimg[j]
      fspc     = filspc[j]
      frib     = filrib[j]
      fcat     = filcat[j]
      strpos   = 'pos' + strtrim(string(j+1),2) + '_'
      pgpfx    = 'cl1252_' + strpos
      pstmp    = 'stmp_' + strpos
      pspcc    = 'spcc_' + strpos
      pspcf    = 'spcf_' + strpos
      pribn    = 'ribn_' + strpos
      pscii    = 'spec_' + strpos

      print,'Setting error model ... '
      fits_read,filg,dum,hgrim,/header_only
      exptime = sxpar(hgrim, 'EXPTIME')
      seterrmod2, hgrim, dark=dark, covar=covar, /verbose
      ;
      print,'Making plots & data files ... '
      
      mkall_grplots_0, fili, fspc, frib, fcat, lrange, $
       outdir+pstmp, outdir+pspcc, outdir+pspcf, outdir+pribn, outdir+pscii, exptime=exptime
      ;
      print,'Making web pages ... '
      rd_grdirct_cat, fcat, id, xim, yim, magauto, aim, bim, thetaim, w50, class
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
   ENDFOR       
   
END 

