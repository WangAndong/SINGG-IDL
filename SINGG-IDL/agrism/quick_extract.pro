PRO quick_extract
   ;
   ; program to quickly extract a 1D spectrum from apsis
   ; processed grism image.
   ;
   ; G. Meurer 09/2006
   ;
   ; for now hardwire variables/input ; turn these into passed
   ; variables later
   COMMON sensmod, lams, sens, esens, hsens
   fgrism     = 'sn1_g800l_drz_sci.fits'
   egrism     = 'sn1_g800l_RMS.fits'
   pfxo       = 'sn1fld'
   xd         = [1676.69, 1094., 2965.4, 1094.]  ; direct image x pos
   yd         = [706.97, 2434., 4045.9, 2434.]   ; direct image y pos
   yg1        = [706, 2433, 4045, 2431]          ; first row of extraction in grism image
   yg2        = [707, 2434, 4046, 2437]          ; last row of extraction in grism image
   dysky1     = [-8, 12, -8, 12]                 ; y offset for first  sky spec
   dysky2     = [8, 19, 8, 19]                   ; y offset for second sky spec
   xref       = 2276.75                          ; center of grism image
   yref       = 2158.0                           ; measured as average of corner positions
   dldp_a_0   = [4771.42, -0.00121212, -0.00247013, -4.42695e-6, -2.06821e-6, 7.93817e-7]
   dldp_a_1   = [41.3419, 0.000786183, -0.00112364, -2.47549e-8, -2.74095e-8, 2.61703e-8]
   dldp_a_2   = [7.71266e-04, 1.3607e-7, 1.53419e-7, 4.75e-10, -6.3533e-11, 6.58144e-11]
   dydx_a_1   = [-4.5065e-4, -3.52335e-6, 2.1394e-6, 2.17438e-10, -7.81162e-10, 4.49999e-11]
   dydx_a_0   = 0.0*dydx_a_1
   yoff_a     = [-0.501446,-0.000243271,0.000225212] ; see NOTES_setup
   dxbeam     = [0, 150]
   filsens    = '/home/meurer/ACS/Grism/aXe/conf/ACS.WFC.1st.sens.6.fits'
   extsens    = 1
   thick      = 1
   aa         = angstsym()
   lprange    = [5800.0, 9800.0]
   charsize   = 1.5
   ;
   ; read in grism and error array images
   fits_read, fgrism, imgr, hdgr
   fits_read, egrism, imrms, hdrms
   ;
   ; convert rms to variance
   imvar      = imrms*imrms
   ;
   ; get exposure time
   texp       = sxpar(hdgr, 'exptime')
   print, 'exptime = ', texp
   ;
   ; set sensitivity model
   setsensmod, filsens, extsens
   ;
   ; find extraction region
   dydx_a0    = eval_axe_poly(dydx_a_0, xd, yd, xref=xref, yref=yref)
   dydx_a1    = eval_axe_poly(dydx_a_1, xd, yd, xref=xref, yref=yref)
   yoff_aa    = eval_axe_poly(yoff_a, xd, yd, xref=xref, yref=yref)
   dydx       = dydx_a0 + 0.5*float(dxbeam[0] + dxbeam[1])*dydx_a1
   dya        = yoff_aa+dydx
   xg         = xd + 0.5*float(dxbeam[0] + dxbeam[1])
   yg         = yd + dya
   ic         = fix(xd + 0.5) - 1
   jc         = fix(yd + 0.5 + dya) - 1
   i1         = fix(xd + 0.5 + float(dxbeam[0])) - 1
   i2         = fix(xd + 0.5 + float(dxbeam[1])) - 1
   j1         = fix(yg1) - 1
   j2         = fix(yg2) - 1
   j3         = fix(yg1 + dysky1) - 1
   j4         = fix(yg2 + dysky1) - 1
   j5         = fix(yg1 + dysky2) - 1
   j6         = fix(yg2 + dysky2) - 1
   ;
   ; loop through extractions
   nap        = n_elements(xd)
   FOR ii = 0, nap-1 DO BEGIN 
      jap     = ii + 1
      ;
      ; collapse arrays within ap and sky apertures to get 1d spectra
      tspec   = blem_collapsex(imgr[i1[ii]:i2[ii],j1[ii]:j2[ii]])
      vspec   = blem_collapsex(imvar[i1[ii]:i2[ii],j1[ii]:j2[ii]])
      sky1    = blem_collapsex(imgr[i1[ii]:i2[ii],j3[ii]:j4[ii]])
      sky2    = blem_collapsex(imgr[i1[ii]:i2[ii],j5[ii]:j6[ii]])
      vsky1   = blem_collapsex(imvar[i1[ii]:i2[ii],j3[ii]:j4[ii]])
      vsky2   = blem_collapsex(imvar[i1[ii]:i2[ii],j5[ii]:j6[ii]])
      ns      = n_elements(tspec)
      ;
      ; derive sky spec and subtract
      sky     = 0.5*(sky1 + sky2)
      vsky    = 0.5*(vsky1 + vsky2)
      ospec   = tspec - sky
      espec   = sqrt(vspec + vsky)
      ;
      ; calculate wavelength array
      xspec   = i1[ii] + 1 + indgen(i2[ii]-i1[ii]+1)
      dxspec  = float(xspec) - xd[ii]
      wl      = axe_wl(dxspec, xd[ii], yd[ii], dldp_a_0, dldp_a_1, dldp_a_2, xref=xref, yref=yref)
      dwl     = axe_wl(dxspec+1.0, xd[ii], yd[ii], dldp_a_0, dldp_a_1, dldp_a_2, xref=xref, yref=yref) - wl
      ;
      ; calculate flam
      slam    = 1.0/(interpol(sens,lams,wl)*dwl*float(texp))
      kk      = where(wl LT min(lams) OR wl GT max(wl), nkk)
      IF nkk GT 0 THEN slam[kk] = 0.0
      flx     = ospec * slam
      eflx    = espec * slam
      ;
      print, xd[ii], yd[ii]
      print, xg[ii], yg[ii]
      print, i1[ii],i2[ii],j1[ii],j2[ii]
      ; quick plots
      keywait, 'Type anything for next plot : '
      window, 0, xsize=600, ysize=600
      !P.MULTI = [0,1,2]
      setplotcolors
      yy1     = ospec - espec
      yy2     = ospec + espec
      aspec   = [ospec, yy1, yy2]
      xx      = wl
      xrange  = [min(wl - dwl), max(wl + dwl) + 1.0]
      dum     = max(aspec) - min(aspec)
      yrange  = [min(aspec) - 0.05*dum, max(aspec) + 0.05*dum]
      plot, wl, ospec, xrange=lprange, yrange=yrange, xstyle=1, ystyle=1, psym=10, $
       xtitle='!3 Wavelength ['+aa+']', ytitle='!3 Counts [DN]', xtickformat='(i5)', $
       thick=thick, charsize=charsize
      oplot, wl, yy1, psym=10, color=!gray, thick=thick
      oplot, wl, yy2, psym=10, color=!gray, thick=thick
      oplot, wl, ospec, psym=10, color=!black, thick=thick+1
      ; oplot, wl, sky, psym=10, color=!blue
      ;
      ; second panel
      qq      = where(wl GE min(lprange) AND wl LE max(lprange), nqq)
      xx      = wl[qq]
      yy      = 1.0e19*flx[qq]
      yy1     = 1.0e19*(flx[qq] - eflx[qq])
      yy2     = 1.0e19*(flx[qq] + eflx[qq])
      aspec   = [yy, yy1, yy2]
      dum     = max(aspec) - min(aspec)
      yrange  = [min(aspec) - 0.05*dum, max(aspec) + 0.05*dum]
      plot, xx, yy, xrange=lprange, yrange=yrange, xstyle=1, ystyle=1, psym=10, $
       xtitle='!3 Wavelength ['+aa+']', ytitle='!3 f!d!4k!3!n [10!u-19!n erg cm!u-2!n s!u-1!n'+aa+'!u-1!n]', $
       xtickformat='(i5)', thick=thick, charsize=charsize
      oplot, xx, yy1, psym=10, color=!gray, thick=thick
      oplot, xx, yy2, psym=10, color=!gray, thick=thick
      oplot, xx, yy, psym=10, thick=thick+1
      ;
      ; write output jpg file
      filj    = pfxo+'_'+strtrim(string(jap),2)+'.jpg'
      imj     = tvrd(true=3)
      WRITE_JPEG,filj,imj,TRUE=3,QUALITY=100
      ;
      ; write output file
      filo = pfxo+'_'+strtrim(string(jap),2)+'.dat'
      openw, lu, filo, /get_lun
      printf, lu, '# col    counts    err_counts  sky_counts   lambda   f_lambda  err(f_lambda)'
      FOR jj = 0, ns-1 DO $
       printf, lu, xspec[jj], ospec[jj], espec[jj], sky[jj], wl[jj], flx[jj], eflx[jj], $
       format='(i5, e12.3, e12.3, e12.3, f9.1, e12.3, e12.3)'
      free_lun, lu
   ENDFOR 
END
