PRO calc_spectilt
   ;
   ; calculate spectral tilt across image, report
   ; min & max slope, slope in center of image.
   ;
   ; chip 1, raw
   xref_ch1 = 0.0
   yref_ch1 = 0.0
   xcen_ch1 = 2048.0
   ycen_ch1 = 2047.0
   ni_ch1   = 4096l
   nj_ch1   = 2048l
   xlen_ch1 = 75.0
   dydx_ch1 = [-0.0357549, 1.05903e-6, -5.09607e-6, -9.18057e-11, 6.21825e-11, 9.56794e-11]
   ; chip 2, raw
   xref_ch2 = 0.0
   yref_ch2 = 0.0
   xcen_ch2 = 2048.0
   ycen_ch2 = 0.0
   ni_ch2   = 4096l
   nj_ch2   = 2048l
   xlen_ch2 = 75.0
   dydx_ch2 = [-0.0246422, 9.28567e-7, -5.49754e-6, -9.18057e-11, 6.21825e-11, 9.56794e-11] 
   ; drizzled image adopted.
   xref_drz = 2183.5
   yref_drz = 2180.5
   xcen_drz = xref_drz
   ycen_drz = yref_drz
   ni_drz   = 4361l
   nj_drz   = 4360l
   xlen_drz = 75.0
   dydx_drz = [-4.5065e-4, -3.52335e-6, 2.1394e-6, 2.17438e-10, -7.81162e-10, 4.49999e-11]
   rmsim    = '/home/meurer/ACS/Grism/HDFN_jun04/Apsis/Work/hdf-north-wfpc2_g800l_RMS.fits'
   rmsmax   = 1.0e10
   ;
   ; make x & y maps
   dum       = findgen(ni_ch1*nj_ch1)
   xim_ch1   = reform((dum MOD ni_ch1), ni_ch1, nj_ch1)
   yim_ch1   = reform(float(fix(dum/float(ni_ch1))), ni_ch1, nj_ch1)
   slope_ch1 = eval_axe_poly(dydx_ch1, xim_ch1, yim_ch1, xref=xref_ch1, yref=yref_ch1)
   slcen_ch1 = eval_axe_poly(dydx_ch1, xcen_ch1, ycen_ch1, xref=xref_ch1, yref=yref_ch1)
   dum       = findgen(ni_ch2*nj_ch2)
   xim_ch2   = reform((dum MOD ni_ch2), ni_ch2, nj_ch2)
   yim_ch2   = reform(float(fix(dum/float(ni_ch2))), ni_ch2, nj_ch2)
   slope_ch2 = eval_axe_poly(dydx_ch2, xim_ch2, yim_ch2, xref=xref_ch2, yref=yref_ch2)
   slcen_ch2 = eval_axe_poly(dydx_ch2, xcen_ch2, ycen_ch2, xref=xref_ch2, yref=yref_ch2)
   dum       = findgen(ni_drz*nj_drz)
   xim_drz   = reform((dum MOD ni_drz), ni_drz, nj_drz)
   yim_drz   = reform(float(fix(dum/float(ni_drz))), ni_drz, nj_drz)
   slope_drz = eval_axe_poly(dydx_drz, xim_drz, yim_drz, xref=xref_drz, yref=yref_drz)
   slcen_drz = eval_axe_poly(dydx_drz, xcen_drz, ycen_drz, xref=xref_drz, yref=yref_drz)
   ;
   ; find good pixels in rms image
   rmsimg    = readfits(rmsim, hdr)
   good      = where(rmsimg LE rmsmax, ngood)
   ;
   ; print out results
   print, 'slope over length of spectra for 3 configurations'
   print, ''
   print, 'Config         center   min(abs(slope)) max(abs(slope))  average(slope)'
   print, 'Raw_ch1  ', xlen_ch1*slcen_ch1, xlen_ch1*min(abs(slope_ch1)), xlen_ch1*max(abs(slope_ch1)), $
    xlen_ch1*mean(slope_ch1)
   print, 'Raw_ch2  ', xlen_ch2*slcen_ch2, xlen_ch2*min(abs(slope_ch2)), xlen_ch2*max(abs(slope_ch2)), $
    xlen_ch2*mean(slope_ch2)
   print, 'Drizzled ', xlen_drz*slcen_drz, xlen_drz*min(abs(slope_drz[good])), xlen_drz*max(abs(slope_drz[good])), $
    xlen_drz*mean(slope_drz[good])
END 
