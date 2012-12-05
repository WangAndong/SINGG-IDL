PRO hdfs_classify
   COMMON sensmod, lams, sens, esens, hsens
   wd       = '/data3/acs27/meurer/grism/hdfs/axe/'
   outdir   = '/data3/acs27/meurer/grism/hdfs/axe/plots/'
   filsens  = '/data3/acs27/meurer/grism/sensG800Lwfc_gd153_ave.fits'
   exts     = 1
   filspc   = 'j8eb03g800l_drz_sci_1.SPC.fits'
   filcat   = 'j8eb03g800l_drz_sci_1.cat'
   ;filcat   = 'test.cat'
   filimg   = '/data3/acs27/meurer/grism/hdfs/apsis/detectionImage.fits'
   filgrim  = '/data3/acs27/meurer/grism/hdfs/apsis/j8eb03g800l_drz_sci.fits'
   logf     = outdir + 'emsource.cat'
   logm     = outdir + 'mstar_sn.cat'
   logi     = outdir + 'spurious.cat'
   ;
   fnorm    = 1.0e-18
   ;
   lrange   = [6000.0, 9400.0]
   nsmooth  = 19
   nsig     = 4.0
   w50lim   = 8.0
   maglim   = 28.0
   covar    = 1.62
   xoff0    = -111.5
   yoff0    = -0.26
   xbuff    = 0.0
   ybuff    = 2.0
   ;
   pfxg     = outdir + 'spec_gfit_'
   ;
   cd, wd
   setplotcolors
   ;
   print,'Setting error model ... '
   fits_read,filgrim,dum,hgrim,/header_only
   exptime = sxpar(hgrim, 'EXPTIME')
   seterrmod2, hgrim, dark=dark, covar=covar, /verbose
   setsensmod, filsens, extsens
   ;
   print,'Reading direct image catalog ...'
   rd_grdirct_cat, filcat, idcat, xim, yim, magauto, aim, bim, thetaim, w50, class
   idcat = fix(temporary(idcat))
   print,'Reading direct image header...'
   fits_read,filimg,dum,himg,/header_only
   print,'Calculating RA & Dec ...'
   xyad, himg, xim, yim, radeg, decdeg
   theta = thetaim * !pi / 180.0
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
   FOR i = 0, n_elements(idcat)-1 DO BEGIN 
      id      = idcat[i]
      skip    = id - oldid - 1
      ;bintab  = kludge_mrdfits(luns, skip, hdr)
      bintab  = mrdfits(luns, skip, hdr)
      grism_updflux, bintab, exptime=exptime
      ;
      j       = id - 1
      objpar  = [xim[j], yim[j], magauto[j], aim[j], bim[j], theta[j], w50[j], class[j], $
                 radeg[j], decdeg[j]]
      ;
      ; check contamination
      grism_boundbox, hdr, boxx, boxy, warn
      grism_zerocontam, boxx, boxy, idcat, xim, yim, magauto, w50, $
       xoff0, yoff0, maglim, w50lim, xbuff, ybuff, ncontam, idc, xgr, ygr
      ;
      ; classify
      grism_classify, bintab, id, objpar, lrange, nsmooth, nsig, luf, lum, lui, $
       minsn, maxsn, ncontam, result, fnorm=fnorm, pfx=pfxg
      ; keywait, 'Press any key to continue'
      ;
      oldid  = id
   ENDFOR 
   free_lun, luns
   free_lun, luf
   free_lun, lum
   free_lun, lui
END
