PRO hdfn_pages_all_aug02
   workdir = '/data3/acs27/meurer/grism/hdfn/axe'
   filimg = '../apsis/detectionImage.fits'
   filspc = ['j8eb01g800l_drz_sci_0.SPC.fits', 'j8eb01g800l_drz_sci_1.SPC.fits']
   filrib = ['j8eb01g800l_drz_sci_0.STP.fits', 'j8eb01g800l_drz_sci_1.STP.fits']
   filcat = ['j8eb01g800l_drz_sci_0.cat', 'j8eb01g800l_drz_sci_1.cat']
   pgpfx  = ['hdfn_all_0_', 'hdfn_all_1_']
   outdir = 'plots/'
   ;
   seterrmod, 4.85, 1.0, (8.0/3600.0), 6870.0, 4, 701.3
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

