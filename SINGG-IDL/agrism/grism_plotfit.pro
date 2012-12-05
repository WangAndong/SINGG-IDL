PRO grism_plotfit, x, y, err, xrange, p, title=title, pfile=pfile
   xtitle  = 'Wavelength [Angstroms]'
   ytitle  = 'flux '
   IF NOT keyword_set(title) THEN title = ' '
   ye1     = y - err
   ye2     = y + err
   w       = where(x GE min(xrange) AND x LE max(xrange))
   fmax    = 1.2*max(ye1[w])  
   fmin    = 1.2*min([0.0,min(ye2[w])])
   yrange  = [fmin,fmax]
   setplotcolors
   plot, x, y, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, $
    xtitle=xtitle, ytitle=ytitle, psym=10, title=title
   oplot, x, ye1, psym=10, color=!gray
   oplot, x, ye2, psym=10, color=!gray
   oplot, x, y, thick=1.5, psym=10
   ;
   yfit    = gauss_n_quad(x, p)
   oplot, x, yfit, color=!red
   IF keyword_set(pfile) THEN makepng,pfile,/color
END 


