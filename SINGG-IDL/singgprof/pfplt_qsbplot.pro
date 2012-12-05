PRO pfplt_qsbplot, file, esky, hardfile=hardfile
   ;
   ; Quick surface brightness profile
   ;
   readcol, file, aint, fint, dfint, ngood, nbad, sb, sigsb, $
    format='(f,f,f,l,l,f,f)'
   ni0   = n_elements(aint)
   j     = 1 + indgen(ni0-1)
   a     = sqrt(0.5*(aint[j]^2 + aint[j-1]^2))
   sigsb = sigsb/sqrt(float(ngood)) + esky
   aint  = aint[j]
   fint  = fint[j]
   dfint = dfint[j]
   ngood = ngood[j]
   nbad  = nbad[j]
   sb    = sb[j]
   sigsb = sigsb[j]
   ni    = ni0 - 1
   ;
   lgsb     = make_array(ni, value=-99.0)
   lgsb0    = make_array(ni, value=-99.0)
   lgsb1    = make_array(ni, value=-99.0)
   k        = where(sb GT 0.0,nk)
   xrange   = [0.0, 1.25*max(aint[k])]
   IF nk GT 0 THEN lgsb[k]  = alog10(sb[k])
   sb0      = sb - sigsb
   sb1      = sb + sigsb
   k        = where(sb1 GT 0.0, nk)
   IF nk GT 0 THEN lgsb1[k] = alog10(sb1[k])
   minlsb   = alog10(0.05*abs(min(sigsb)))
   maxlsb   = max(lgsb1)
   dlsb     = max([0.1, 0.1*(maxlsb - minlsb)])
   yrange   = [minlsb-dlsb, maxlsb+dlsb]
   k        = where(sb0 GT 0.0, nk)
   IF nk GT 0 THEN lgsb0[k] = alog10(sb0[k])
   ;
   plot, a, lgsb, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, psym=sym(1), $
    xtitle='Semi-major axis [pixels]', ytitle='Log(surface brightnes [DN/s])', $
    title=file
   errplot, a, lgsb0, lgsb1, width=0.0
END 
