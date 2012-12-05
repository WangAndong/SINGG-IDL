PRO plot_grposnmod
   ;
   ; plot position of direct image and 1st order 
   ; spectra, from a file and the model positions.
   ; calculate the observed offset in y.
   ;
   ; G. Meurer 08/2005
   ;
   fili    = 'grpos.dat'
   xrange  = [1., 4229.]
   yrange  = [1., 4250.]
   yoff_a     = [-0.765986, 7.31305e-5, 1.24362e-4, -9.86897e-8, 2.01405e-7, 2.8342e-8]
   dydx_a_1   = [-4.5065e-4, -3.52335e-6, 2.1394e-6, 2.17438e-10, -7.81162e-10, 4.49999e-11]
   dydx_a_0   = 0.0*dydx_a_1
   dxbeam     = [-10, 150]
   xref       = 2284.0
   yref       = 2168.0
   yoff_a[0]  = yoff_a[0] + 1.95
   ;
   readcol, fili, xd, yd, xb, yb, xr, yr, format='(f,f,f,f,f,f)'
   np = n_elements(xd)
   ;
   ; calculate position of grism spectra
   xd2    = xd + total(dxbeam)
   dydxa0 = eval_axe_poly(dydx_a_0, xd, yd, xref=xref, yref=yref)
   dydxa1 = eval_axe_poly(dydx_a_1, xd, yd, xref=xref, yref=yref)
   yoffa  = eval_axe_poly(yoff_a, xd, yd, xref=xref, yref=yref)
   dydx   = dydxa0 + 0.5*float(dxbeam[0] + dxbeam[1])*dydxa1
   dya    = yoffa + dydx
   xga    = xd + 0.5*(dxbeam[0] + dxbeam[1])
   yga    = yd + dya
   ygm    = 0.5*(yb + yr)
   offy   = ygm - yga
   forprint, xd, yd, dydxa0, dydxa1, yoffa, dydx, dya, xga, yga, ygm, offy
   grm_avsigclip, offy, 2.5, 50, mean_offy, sig_offy, nuse, nrej, nit, /verbose
   print, mean_offy, sig_offy, nuse, nrej, nit
   ;
   setplotcolors
   plot, xd, yd, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, psym=sym(1), $
    xtitle='Column [pixel]', ytitle='Row [pixel]'
   oplot, xb, yb, psym=sym(4), color=!blue
   oplot, xr, yr, psym=sym(4), color=!red
   FOR ii = 0, np-1 DO BEGIN 
      xp = [xb[ii], xr[ii]]
      yp = [yb[ii], yr[ii]]
      oplot, xp, yp, color=!black
   ENDFOR 
END 
