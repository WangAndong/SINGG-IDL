PRO calc_fit_rms, ffit, fcomb
   cvel       = 299792.458
   fmtv       = '(i,f,f,f,f,f,f,f,f,f,f,f,f,f,f)'
   readcol, ffit, col1, col2, format='(a,f)', /silent
   vsysloc    = where(col1 EQ 'vsys', nvsys)
   vsysloc    = vsysloc(nvsys-1)
   vsys       = col2(vsysloc)
   inc        = col2(vsysloc+1)
   xpos       = col2(vsysloc+2)
   ypos       = col2(vsysloc+3)
   pa         = col2(vsysloc+4)
   readcol, ffit, rr, vrot, err, format='(f,f,x,f)', /silent
   rrloc      = 0
   FOR ii = 1, n_elements(rr)-1 DO IF rr(ii) LT rr(ii-1) THEN rrloc = ii
   rr         = [0.0, rr(rrloc:*)]
   vrot       = [0.0, vrot(rrloc:*)]
   err        = [0.0, err(rrloc:*)]
   ;
   ; read observed data
   readcol, fcomb, obsapid, z1, f1, z2, f2, z3, f3, z4, f4, z5, f5, z6, f6, $
     obsmeanz, obsmeanv, format=fmtv
   nap        = n_elements(obsapid)
   modelv     = fibvel(vsys, inc, xpos, ypos, pa, rr, vrot, fibers=obsapid)
   residual   = modelv - cvel * obsmeanz
   rms        = sqrt(mean(residual^2))
   ;
   ; print result
   print, 'rms   ', rms
   openw, 1, ffit, /append
   printf, 1, ''
   printf, 1, '# File modified by calc_fit_rms.pro on ' + systime()
   printf, 1, ''
   printf, 1, 'rms   ', rms
   close, 1
END
