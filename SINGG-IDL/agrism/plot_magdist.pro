PRO plot_magdist
   readcol, 'crossmatched.dat', id, xim, yim, mag, a, b, th, fwhm, cl, fnda, fndb, fndc, fndf, zcc, zff, $
    format='(i,f,f,f,f,f,f,f,i,i,i,i,f,f)'
   kab = where(fnda GT 0 OR fndb GT 0, nab)
   kc  = where(fndc GT 0, nc)
   kf  = where(fndf GT 0, nf)
   print, 'Number of grism emission line sources : ', nab
   print, 'Number of Cohen et al. matches        : ', nc
   print, 'Number of FLY99 matches               : ', nf
   ;
   !P.MULTI = [0,1,3]
   window,0,xsize=400,ysize=600
   ;
   ; Some stats
   medab    = median(mag[kab])
   medc     = median(mag[kc])
   medf     = median(mag[kf])
   ;
   ; calculate histograms
   mmin     = 18.0
   mmax     = 28.0
   dm       = 0.5
   nm       = (mmax - mmin)/dm
   xm       = mmin + dm*(0.5 + findgen(nm))
   nmagab   = histogram(mag[kab], min=mmin, max=mmax, binsize=dm)
   nmagc    = histogram(mag[kc],  min=mmin, max=mmax, binsize=dm)
   nmagf    = histogram(mag[kf],  min=mmin, max=mmax, binsize=dm)
   ;
   plot,xm,nmagab,xrange=[mmin,mmax],xstyle=1, psym=10, yrange=[0,10], ystyle=1, $
    xtitle='Magauto [ABmag]', ytitle='N', title='Combined emission line detections', charsize=1.8
   FOR i = 0, nm-1 DO BEGIN 
      xbx = xm[i] + dm*[-0.5, -0.5, 0.5, 0.5]
      ybx = nmagab[i]*[0.0, 1.0, 1.0, 0.0]
      polyfill, xbx, ybx, color=!lgreen
      oplot, xbx, ybx
   ENDFOR 
   xyouts, 18.5, 9.0, 'N = '+strtrim(string(nab),2)
   xyouts, 18.5, 8.0, 'median(mag) = '+strtrim(string(medab),2)
   ;
   plot,xm,nmagc,xrange=[mmin,mmax],xstyle=1, yrange=[0,40], ystyle=1, psym=10, $
    xtitle='Magauto [ABmag]', ytitle='N', title='Cohen et al. (2000) spectro-z catalog', charsize=1.8
   FOR i = 0, nm-1 DO BEGIN 
      xbx = xm[i] + dm*[-0.5, -0.5, 0.5, 0.5]
      ybx = nmagc[i]*[0.0, 1.0, 1.0, 0.0]
      polyfill, xbx, ybx, color=!lgreen
      oplot, xbx, ybx
   ENDFOR 
   xyouts, 18.5, 36.0, 'N = '+strtrim(string(nc),2)
   xyouts, 18.5, 32.0, 'median(mag) = '+strtrim(string(medc),2)
   ;
   plot,xm,nmagf,xrange=[mmin,mmax],xstyle=1, yrange=[0,120], ystyle=1, psym=10, $
    xtitle='Magauto [ABmag]', ytitle='N', title='Fernandez-Soto et al. (1999) photo-z catalog', charsize=1.8
   FOR i = 0, nm-1 DO BEGIN 
      xbx = xm[i] + dm*[-0.5, -0.5, 0.5, 0.5]
      ybx = nmagf[i]*[0.0, 1.0, 1.0, 0.0]
      polyfill, xbx, ybx, color=!lgreen
      oplot, xbx, ybx
   ENDFOR 
   xyouts, 18.5, 108.0, 'N = '+strtrim(string(nf),2)
   xyouts, 18.5,  96.0, 'median(mag) = '+strtrim(string(medf),2)
   makepng,'maghist.png',/color
END 

