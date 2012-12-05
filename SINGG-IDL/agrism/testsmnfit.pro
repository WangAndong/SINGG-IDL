PRO testsmnfit
   COMMON sensmod, lams, sens, esens, hsens
   window   = 0
   wd       = '/data3/acs27/meurer/grism/hdfn/axe'
   filsens  = '/data3/acs27/meurer/grism/sensG800Lwfc_gd153_ave.fits'
   exts     = 1
   filspc   = 'j8eb01g800l_drz_sci_1.SPC.fits'
   filcat   = 'j8eb01g800l_drz_sci_1.cat'
   filimg   = '/data3/acs27/meurer/grism/hdfn/apsis0208/detectionImage.fits'
   filgrim  = '/data3/acs27/meurer/grism/hdfn/apsis/j8eb01g800l_drz_sci.fits'
   logf     = '/data3/acs27/meurer/grism/hdfn/axe/testf.log'
   logm     = '/data3/acs27/meurer/grism/hdfn/axe/testm.log'
   logi     = '/data3/acs27/meurer/grism/hdfn/axe/testi.log'
   ;
   fnorm    = 1.0e-18
   ;
   disp     = 40.0
   lrange   = [6000.0, 9400.0]
   nsmooth  = 19
   nsig     = 4.0
   minsigma = 2.2*disp/2.3548
   dxfit    = 2.0*nsmooth*disp
   ;
   testid   = [37, 91, 123, 190, 229, 288, 295, 348, 351, 359, 360, 531, $
               57, 198, 231, 353, 107, 327, 330, 6, 89, 170, 251, 297, 54, 223]
   ; testid   = [531]
   j        = sort(testid)
   testid   = temporary(testid[j])
   ;
   cd, wd
   setplotcolors
   ;
   print,'Setting error model ... '
   fits_read,filgrim,dum,hgrim,/header_only
   exptime = sxpar(hgrim, 'EXPTIME')
   seterrmod2, hgrim, dark=dark, /verbose
   setsensmod, filsens, extsens, fudge=sfudge
   ;
   rd_grdirct_cat, filcat, idcat, xim, yim, magauto, aim, bim, thetaim, w50, class
   theta = thetaim * !pi / 180.0
   idcat = fix(temporary(idcat))
   ;
   ; open spectra image
   luns  = fxposit(filspc, 1)
   oldid = 0
   ;
   ; open logs
   grcl_openlog, luf, logf, /fitlog
   grcl_openlog, lum, logm
   grcl_openlog, lui, logi
   ;
   ; loop through test spectra: update fluxes, plot, plot residuals
   ;
   FOR i = 0, n_elements(testid)-1 DO BEGIN 
      id      = testid[i]
      skip    = id - oldid - 1
      bintab  = kludge_mrdfits(luns, skip)
      grism_updflux, bintab, exptime=exptime
      xtitle  = 'Wavelength [Angstroms]'
      ytitle  = 'flux [1e-18 erg/cm^2/s/AA]'
      ;
      j       = id - 1
      objpar  = [xim[j], yim[j], magauto[j], aim[j], bim[j], theta[j], w50[j], class[j]]
      ;
      grism_classify, bintab, id, objpar, lrange, nsmooth, nsig, luf, lum, lui, $
       minsn, maxsn, result, fnorm=fnorm
      ; keywait, 'Press any key to continue'
      ;
      oldid  = id
   ENDFOR 
   free_lun, luns
   free_lun, luf
   free_lun, lum
   free_lun, lui
END
