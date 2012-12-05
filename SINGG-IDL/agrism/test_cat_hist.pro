PRO test_cat_hist, fcat
   readcol, fcat, id, xim, yim, magauto, emagauto, flux1, flux2, flux3, eflux1, eflux2, eflux3, $
    frad1, frad2, krad, bkgd, thresh, area, aim, eaim, bim, ebim, thim, ethim, elong, fwhm
   lelong = alog10(elong)
   mine    = min(lelong)
   maxe    = max(lelong)
   print, mine,maxe
   xr      = [-0.1, maxe]
   nlelong = histogram(lelong, min=-0.1, max=2.5, binsize=0.05)
   xlelong =-0.05 + 0.05*indgen(n_elements(nlelong))
   plot, xlelong, nlelong, psym=10, xtitle='log(a/b)', ytitle='N', xrange=xr, xstyle=1
   keywait, 'Press any key to continue.'
   ;
   plot, lelong, thim, psym=1, yrange=[-90,90], xrange=[0.0, 1.5], xstyle=1, ystyle=1, $
    xtitle='log(a/b)', ytitle='theta'
   keywait, 'Press any key to continue.'
   ;
   plot, fwhm, thim, psym=1, xrange=[0,1.1*max(fwhm)], yrange=[-90,90], xstyle=1, ystyle=1, $
    xtitle = 'FWHM', ytitle='theta'
END
