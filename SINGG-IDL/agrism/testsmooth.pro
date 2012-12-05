PRO testsmooth
   COMMON sensmod, lams, sens, esens, hsens
   window   = 0
   wd       = '/data3/acs27/meurer/grism/hdfn/axe'
   filsens  = '/data3/acs27/meurer/grism/sensG800Lwfc_gd153_ave.fits'
   exts     = 1
   filspc   = 'j8eb01g800l_drz_sci_1.SPC.fits'
   filcat   = 'j8eb01g800l_drz_sci_1.cat'
   filimg   = '/data3/acs27/meurer/grism/hdfn/apsis0208/detectionImage.fits'
   filgrim  = '/data3/acs27/meurer/grism/hdfn/apsis/j8eb01g800l_drz_sci.fits'
   ;
   dofit    = 'N'
   disp     = 40.0
   lrange   = [6000.0, 9400.0]
   nsmooth  = 19
   nsig     = 5.0
   minsigma = 2.2*disp/2.3548
   dxfit    = 2.0*nsmooth*disp
   ;
   testid   = [37, 91, 123, 190, 229, 288, 295, 348, 351, 359, 360, 531, $
               57, 198, 231, 353, 107, 327, 330, 6, 89, 170, 251, 297, 54, 223]
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
   rd_grdirct_cat, filcat, id, xim, yim, magauto, aim, bim, thetaim, w50, class
   theta = thetaim * !pi / 180.0
   id    = fix(temporary(id))
   ;
   ; open spectra image
   luns  = fxposit(filspc, 1)
   oldid = 0
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
      ; ytitle  = 'counts'
      lam     = bintab.lambda
      flx     = bintab.flux / 1.0e-18
      eflx    = bintab.ferror / 1.0e-18
      ; flx     = bintab.count
      ; eflx    = bintab.error 
      emin    = flx - eflx
      emax    = flx + eflx
      IF keyword_set(lnorm) THEN w = where(lam GE min(lnorm) AND lam LE max(lnorm)) $
      ELSE w = where(lam GE min(lrange) AND lam LE max(lrange))
      fmax    = 1.2*max(emax[w])  
      fmin    = 1.2*min([0.0,min(emin[w])])
      yrange  = [fmin,fmax]
      window, 0
      plot, lam, flx, xrange=lrange, yrange=yrange, xstyle=1, ystyle=1, $
       xtitle=xtitle, ytitle=ytitle, title=trim(string(id),2), psym=10
      oplot, lam, emin, color=!pink, psym=10
      oplot, lam, emax, color=!pink, psym=10
      oplot, lam, flx, psym=10
      ;
      linefind, flx, eflx, 19, nsig, smspec, resid19, nem, nabs, $
       minsn, maxsn, lrange=lrange, lam=lam
      oplot, lam, smspec, color=!dgreen
      print, nem, nabs, minsn, maxsn
      ;linefind, flx, eflx, 21, nsig, smspec, resid21, nem, nabs
      ;oplot, lam, smspec, color=!green
      ;linefind, flx, eflx, 17, nsig, smspec, resid17, nem, nabs
      ;oplot, lam, smspec, color=!lgreen
      ;linefind, flx, eflx, 15, nsig, smspec, resid15, nem, nabs
      ;oplot, lam, smspec, color=!dcyan
      ;linefind, flx, eflx, 13, nsig, smspec, resid13, nem, nabs
      ;oplot, lam, smspec, color=!cyan
      ;linefind, flx, eflx, 11, nsig, smspec, resid11, nem, nabs
      ;oplot, lam, smspec, color=!purple
      ;
      window, 1
      plot, lam, resid19/eflx, xrange=lrange, xstyle=1, $
       xtitle=xtitle, ytitle=ytitle, title=trim(string(id),2), psym=10
      ;oplot, lam, resid19/eflx, color=!dgreen, psym=10
      ;oplot, lam, resid21/eflx, color=!green, psym=10
      ;oplot, lam, resid17/eflx, color=!lgreen, psym=10
      ;oplot, lam, resid15/eflx, color=!dcyan, psym=10
      ;oplot, lam, resid13/eflx, color=!cyan, psym=10
      ;oplot, lam, resid11/eflx, color=!purple, psym=10
      ;
      ; do fit if requested
      read,dofit,prompt='Do you want to do fit : ',format='(a)'
      IF strupcase(strmid(strtrim(dofit,2),0,1)) EQ 'Y' THEN BEGIN 
         window, 0
         grism_linefit, lam, flx, eflx, resid19, lrange, minsigma, dxfit, p
         grism_plotfit, lam, flx, eflx, lrange, p
         gfit = gauss1(lam, p[3:5])
         flx2 = flx - gfit
         linefind, flx2, eflx, nsmooth, nsig, smspec2, resid2, nem2, nabs2, $
          minsn2, maxsn2, lrange=lrange, lam=lam
         oplot, lam, smspec2, color=!dgreen
         print, nem, nabs, minsn, maxsn
         window, 1
         plot, lam, resid2/eflx, xrange=lrange, xstyle=1, $
          xtitle=xtitle, ytitle=ytitle, title=trim(string(id),2), psym=10
         keywait, 'Press any key to continue.'
      ENDIF 
      ;
      oldid  = id
   ENDFOR 
   free_lun, luns
END

