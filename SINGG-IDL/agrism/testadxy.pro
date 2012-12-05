PRO testadxy
   workdir  = '/data3/acs27/meurer/grism/hdfs/axe'
   filimg   = '/data3/acs27/meurer/grism/hdfs/apsis/detectionImage.fits'
   filgrim  = '/data3/acs27/meurer/grism/hdfs/apsis/j8eb03g800l_drz_sci.fits'
   filcat   = 'j8eb03g800l_drz_sci_1.cat'
   print,'Setting error model ... '
   fits_read,filgrim,dum,hgrim,/header_only
   exptime = sxpar(hgrim, 'EXPTIME')
   rd_grdirct_cat, filcat, id, xim, yim, magauto, aim, bim, thetaim, w50, class
   ;
   xyad, hgrim, xim, yim, a, d, /print
   ;
END 
