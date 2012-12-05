PRO firstord_finder_plots
   ;
   ; make exploratory plots of sextractor catalog 
   ; of grism_rebin.fits in order to find best star candidates.
   ; 
   ; parameters to look at
   ; a_image, b_image, theta_image
   ; fwhm_image
   ; class_star
   ; mag_auto
   ;
   fcat   = 'grism_rebin01.cat'
   fmt    = '(i,f,f,f,x,x,x,x,x,f,x,x,x,f,f,f,x,f,i,f)'
   ;
   ; read file
   readcol, fcat, id, xim, yim, mag, krad, aim, bim, thim, w50, flag, class, format=fmt
   axrat  = aim/bim
   ;
   setplotcolors
   plot, aim, bim, psym=sym(1), xrange=[0.0,40.0], yrange=[0.0,9.0], xstyle=1, ystyle=1, $
    xtitle='!3 a [pix]', ytitle='!3 b [pix]', charsize=1.5
   keywait, 'type anything for next plot'
   ;
   xrange = [0.0, 10.0]
   xstep  = 0.1
   nbin   = (xrange[1] - xrange[0])/xstep
   xhist  = xrange[0] + xstep*(0.5 + findgen(nbin))
   ahist  = histogram(aim, binsize=xstep, min=xrange[0], max=xrange[1])
   bhist  = histogram(bim, binsize=xstep, min=xrange[0], max=xrange[1])
   plot, xhist, ahist,xrange=xrange, yrange=[0., 100.], xstyle=1, ystyle=1, psym=10, $
    xtitle='!3 a!image!n , b!dimage!n [pixel]', ytitle='N', charsize=1.5
   oplot, xhist, ahist, psym=10, color=!dgreen
   oplot, xhist, bhist, psym=10, color=!dblue
   ;
   keywait, 'type anything for next plot'
   plot, bim, axrat, psym=sym(1), xrange=[0.0, 4.0], yrange=[0.9, 4.0], $
    xtitle='!3 b!dimage!n [pixel]', ytitle='!3 a/b', charsize=1.5
   ;
   keywait, 'type anything for next plot'
   xrange = [0.8, 5.0]
   xstep  = 0.1
   nbin   = (xrange[1] - xrange[0])/xstep
   xxhist  = xrange[0] + xstep*(0.5 + findgen(nbin))
   axhist = histogram(axrat, binsize=xstep, min=xrange[0], max=xrange[1])
   plot, xxhist, axhist, psym=10, xrange=[0.8, 4.0], yrange=[0., 80.], xstyle=1, ystyle=1, $ 
    xtitle='!3 a/b', ytitle='!3 N', charsize=1.5
   ;
   stop
END 
