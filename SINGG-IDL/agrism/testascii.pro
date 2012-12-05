PRO testascii
   COMMON sensmod, lams, sens, esens, hsens
   window   = 0
   wd       = '/data3/acs27/meurer/grism/hdfn/axe0208'
   fils     = '/data3/acs27/meurer/grism/sens_G800L_wfc_smov.fits'
   exts     = 1
   exptime  = 6870.0
   filspc   = 'j8eb01g800l_drz_sci_0.SPC.fits'
   filcat   = 'j8eb01g800l_drz_sci_0.cat'
   filimg   = '/data3/acs27/meurer/grism/hdfn/apsis0208/detectionImage.fits'
   filgrim  = '/data3/acs27/meurer/grism/hdfn/apsis0208/j8eb01g800l_drz_sci.fits'
   ext      = [104, 329, 600, 703]
   mag      = [22.582,  23.355, 23.389,  22.464]
   zspec    = [0.936,  0.961,  0.952,  0.321]
   xsizepl  = 400
   ysizepl  = 180
   sizstp   = [60,60]
   sfudge   = 1.0/40.0
   expnd    = 2
   pstmp    = 'junks_'
   pspec    = 'craps_'
   pfspc    = 'poohs_'
   ;
   setsensmod, fils, exts, fudge=sfudge
   seterrmod, 4.85, 1.0, (8.0/3600.0), exptime, 4, 701.3
   plot,lams,sens,xrange=[5500,10000.],xstyle=1
   keywait,'Press any key to continue'
   ;
   fits_read,filimg,img,hdr
   rd_grdirct_cat, filcat, id, xim, yim, magauto, aim, bim, thetaim, w50, class
   FOR j = 0, n_elements(ext)-1 DO BEGIN 
      i = ext[j] -1 
      print,i
   ;
      loadct,0
      plot_dirctstamp, img, id[i], [xim[i], yim[i]], sizstp, $
       expnd, window, pstmp, stampim
      print,i,id[i],xim[i],yim[i]
      keywait,'Press any key to continue'
   ;
      luns = fxposit(filspc, 1)
      setplotcolors
      window,window,xsize=xsizepl,ysize=ysizepl
      bintab  = mrdfits(luns, i, hdr)
      close,luns
      free_lun,luns
      grism_plot1d, bintab, id[i], pspec, orient=[aim[i], bim[i], thetaim[i]]
      keywait,'Press any key to continue'
   ;
   ; mean counts, flux density
      lam     = bintab.lambda
      cnt     = bintab.count
      flx     = bintab.flux
      m       = where(lam GE 6000 AND lam LE 9000)
      mom     = moment(cnt[m])
      flxmom  = moment(flx[m])
      mndlam  = (max(lam[m]) - min(lam[m]))/(n_elements(m)-1)
      flam    = 10.0^(-0.4*(mag[j] + 5.0*alog10(8000.0/5470.0) + 21.1))
      crd     = mom[0]/(exptime*mndlam)
      print,'mean counts 6000 -- 9000 angs: ', mom[0]
      print,'mean aXe flux                : ', flxmom[0]
      print,'mean dlam                    : ', mndlam
      print,'mean CR density              : ', crd
      print,'f_8000 (from i+z mag)        : ', flam
      print,'mean sensitivity             : ', crd / flam
   ;
      oflux   = bintab.flux / (exptime * sfudge)
      oferror = bintab.ferror / (exptime * sfudge)
      grism_updflux, bintab, exptime=exptime
      flx     = bintab.flux
      flxmom  = moment(flx[m])
      print,'mean hand flux               : ', flxmom[0]
      xrange  = [5600.0, 10200.0]
      cmaxdef = 150.0
      grism_fplot1d, bintab, id[i], xrange, pfspc, orient=[aim[i], bim[i], thetaim[i]], lnorm=[6000,9500]
   ;
      filcii  = 'source_'+trim(string(id[i])) + '.dat'
      print,'Making : ',filcii
      grism_outascii, filcii, bintab, mag[j], zspec[j], xim[i], yim[i], lamlim=[5600,10000.0]
      keywait,'Press any key to continue'
   ENDFOR 
END 
