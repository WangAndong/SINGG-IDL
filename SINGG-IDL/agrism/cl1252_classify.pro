PRO cl1252_classify
   COMMON sensmod, lams, sens, esens, hsens
   wd       = '/data3/acs27/meurer/grism/cl1252/axe/'
   outdir   = '/data3/acs27/meurer/grism/cl1252/axe/plots/'
   filsens  = '/data3/acs27/meurer/grism/sensG800Lwfc_gd153_ave.fits'
   exts     = 1
   apdir    = '/data3/acs27/meurer/grism/cl1252/apsis/'
   filspc   = ['j8eh22g800l_drz_sci_1.SPC.fits', 'j8eh24g800l_drz_sci_1.SPC.fits', 'j8eh26g800l_drz_sci_1.SPC.fits', 'j8eh28g800l_drz_sci_1.SPC.fits']
   filcat   = ['j8eh22g800l_drz_sci_1.cat', 'j8eh24g800l_drz_sci_1.cat', 'j8eh26g800l_drz_sci_1.cat', 'j8eh28g800l_drz_sci_1.cat']
   filimg   = apdir + ['detectionImage_pos1.fits', 'detectionImage_pos2.fits', 'detectionImage_pos3.fits', 'detectionImage_pos4.fits']
   filgrim  = apdir + ['j8eh22g800l_drz_sci.fits', 'j8eh24g800l_drz_sci.fits', 'j8eh26g800l_drz_sci.fits', 'j8eh28g800l_drz_sci.fits']
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
   setsensmod, filsens, extsens
   setplotcolors
   ;
   FOR k = 0, n_elements(filgrim)-1 DO BEGIN 
      print, ' **** '
      print, ' **** classifying objects in position ', k
      print, ' **** '
      ;
      cd, wd
      filg     = filgrim[k]
      fili     = filimg[k]
      fspc     = filspc[k]
      fcat     = filcat[k]
      strpos   = 'pos' + strtrim(string(k+1),2) + '_'
      pgpfx    = 'cl1252_' + strpos
      pstmp    = 'stmp_' + strpos
      pspcc    = 'spcc_' + strpos
      pspcf    = 'spcf_' + strpos
      pscii    = 'spec_' + strpos
      logf     = outdir + strpos + 'emsource.cat'
      logm     = outdir + strpos + 'mstar_sn.cat'
      logi     = outdir + strpos + 'spurious.cat'
      ;
      print,'Setting error model ... '
      fits_read,filg,dum,hgrim,/header_only
      exptime = sxpar(hgrim, 'EXPTIME')
      seterrmod2, hgrim, dark=dark, covar=covar, /verbose
      ;
      rd_grdirct_cat, fcat, idcat, xim, yim, magauto, aim, bim, thetaim, w50, class
      theta = thetaim * !pi / 180.0
      idcat = fix(temporary(idcat))
      ;
      ; open spectra image
      luns  = fxposit(fspc, 1)
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
         bintab  = mrdfits(luns, skip, hdr)
         grism_updflux, bintab, exptime=exptime
         ;
         j       = id - 1
         objpar  = [xim[j], yim[j], magauto[j], aim[j], bim[j], theta[j], w50[j], class[j]]
         ;
         ; check conatmination
         grism_boundbox, hdr, boxx, boxy, warn
         grism_zerocontam, boxx, boxy, idcat, xim, yim, magauto, w50, $
          xoff0, yoff0, maglim, w50lim, xbuff, ybuff, ncontam, idc, xgr, ygr
         ;
         ; classify
         grism_classify, bintab, id, objpar, lrange, nsmooth, nsig, luf, lum, lui, $
          minsn, maxsn, ncontam, result, fnorm=fnorm
         ;
         oldid  = id
      ENDFOR 
      free_lun, luns
      free_lun, luf
      free_lun, lum
      free_lun, lui
      ENDFOR 
END
