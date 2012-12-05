FUNCTION fitfibvel, fgaus, inguess, outfile=outfile, $
                    radians=radians, ppas=ppas, fast=fast, uncert=uncert, $
                    fixed=fixed, limited=limited, limits=limits
   exptimes   = 'exptimes.dat'
   fmtg       = '(i,a,a,x,x,x,f,f,f,f,f,f,f,i,f,f)'
   fmtv       =  '(i,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f)'
   cvel       = 299792.458
   lamzero    = [6548.1001,6562.8169,6583.6001,6678.15,6716.4702,6730.8501]
   nlines     = n_elements(lamzero)
   ;
   ; make other filenames
   gausspos   = strpos(fgaus, 'gauss')
   fitsfile   = strmid(fgaus, 0, gausspos) + 'obj' + $
     strmid(fgaus, gausspos+5, 13) + '.co.cr.ms.fits'
   fcomb      = strmid(fgaus, 0, gausspos) + 'comb' + $
     strmid(fgaus, gausspos+5)
   ;
   ; read gaussian fit positions
   readcol, fgaus, row, filstr, filstr2, chisq, rms, c1, c2, c3, $
     c4, c5, linnum, zz, rvel, format=fmtg, /silent
   vv = zz * cvel
   nr = n_elements(row)
   ;
   ; read exposure times
   readcol, exptimes, filename, nexp, expt, $
     format='(x,x,x,a,i,f)', /silent
   time  = 1800.0
   thisfile = 'obj'+strmid(fgaus,strpos(fgaus,'gauss')+5,13)
   fileline = where(filename EQ thisfile)
   IF fileline NE -1 THEN time = expt(fileline)
   ;
   ; find the dispersion
   fitsheader = headfits(fitsfile)
   deltloc    = where(strmid(fitsheader,0,6) EQ 'CDELT1')
   disper     = float(strmid(fitsheader(deltloc), $
                             strpos(fitsheader(deltloc), '=')+1))
   disper     = disper(0)
   ;
   ; decode apertures from filstr
   gausid = make_array(nr, /int, value=0)
   FOR ii = 0, nr-1 DO BEGIN 
      k2   = strpos(filstr2[ii],']',/reverse_search)
      istr = strmid(filstr2[ii],0,k2)
      gausid[ii] = fix(istr)
   ENDFOR 
   ;
   ; calculate the linewidths, flux  and velocity dispersion
   fwhm       = make_array(nr)
   deltax     = make_array(nr)
   FOR ii = 0, nlines-1 DO BEGIN
       thisline = where(linnum EQ ii+1, nthisline)
       IF nthisline GT 0 THEN BEGIN
           fwhm(thisline) = c5(thisline)*cvel/lamzero(ii)
           deltax(thisline) = disper*cvel/lamzero(ii)
       ENDIF
   ENDFOR
   flux       = fwhm * c3
   ;
   ; calculate error in gaussian parameters
   errvel     = 0.692227211 * sqrt(deltax * fwhm) * rms / c3
   errfwhm    = 1.630070514 * sqrt(deltax * fwhm) * rms / c3
   errc3      = 1.411682475 * sqrt(deltax / fwhm) * rms
   errflux    = sqrt( (c3 * errfwhm)^2 + (fwhm * errc3)^2 )
   ;
   ; read flux and velocity data
   readcol, fcomb, combapid, z1, f1, z2, f2, z3, f3, z4, f4, z5, f5, z6, f6, $
     meanz, meanv, stddevv, format=fmtv
   ncombap    = n_elements(combapid)
   meanv      = meanz * cvel
   ;
   ; calculate error in each velocity measurement
   errmeanv   = make_array(ncombap)
   FOR ii = 0, ncombap-1 DO BEGIN
       thisap     = where(gausid EQ combapid(ii), nthisap)
       IF nthisap EQ 1 THEN errmeanv(ii) = errvel(thisap) ELSE BEGIN
           totalflux  = total(flux(thisap))
           alpha      = flux(thisap) / totalflux
           errmeanv(ii) = sqrt(total( (errflux(thisap) * (vv(thisap) - meanv(ii)) / $
                                      totalflux)^2 + (errvel(thisap) * alpha)^2 ))
       ENDELSE
   ENDFOR
   meanerrv    = mean(errmeanv)
   npar        = n_elements(inguess)
   nrr         = (npar - 5) / 2
   expression  = 'fibvel(P(0), P(1), P(2), P(3), P(4), P(5:' + $
     strcompress(string(4+nrr), /remove_all) + $
     '), P(' + strcompress(string(5+nrr), /remove_all) + ':' + $
     strcompress(string(4+2*nrr), /remove_all) + '), fibers=X'
   IF keyword_set(radians) THEN expression = expression + ', /radians'
   IF keyword_set(ppas) THEN expression = expression + ', ppas=ppas'
   IF keyword_set(fast) THEN expression = expression + ', /fast'
   expression  = expression + ')'
   print, expression
   ;
   ; set limitations on parameters
   parinfo     = replicate({fixed:0, limited:[0,0], limits:[0.0, 0.0]}, 5+2*nrr)
   IF keyword_set(fixed) THEN FOR ii = 0, npar-1 DO parinfo(ii).fixed = fixed(ii) $
   ELSE parinfo(5:5+nrr).fixed = 1
   IF keyword_set(limited) THEN FOR ii = 0, npar-1 DO parinfo(ii).limited = limited(*,ii) $
   ELSE parinfo(6+nrr:4+2*nrr).limited(0) = 1
   IF keyword_set(limits) THEN FOR ii = 0, npar-1 DO parinfo(ii).limits = limits(*,ii)
   ;
   ; perform fit
   fitresult   = mpfitexpr(expression, combapid, meanv, errmeanv, inguess, $
                           parinfo=parinfo, niter=niter, perror=perror, $
                           bestnorm=bestnorm)
   degfree     = n_elements(combapid) - (4 + nrr)
   uncert      = perror * sqrt(bestnorm / degfree)
   ;
   ; calculate rms of fit
   modelv     = fibvel(fitresult(0), fitresult(1), fitresult(2), fitresult(3), fitresult(4), $
                       fitresult(5:4+nrr), fitresult(5+nrr:*), fibers=combapid)
   residual   = modelv - meanv
   rms        = sqrt(mean(residual^2))
   ;
   ; print results
   IF keyword_set(outfile) THEN BEGIN
       openw, 1, outfile, /append
       printf, 1, ''
       printf, 1, '# File modified by fitfibvel.pro on ' + systime()
       printf, 1, ''
       printf, 1, 'niter ', niter
       printf, 1, ''
       printf, 1, '# Input guesses'
       printf, 1, ''
       printf, 1, 'vsys  ', inguess(0)
       printf, 1, 'inc   ', inguess(1)
       printf, 1, 'xpos  ', inguess(2)
       printf, 1, 'ypos  ', inguess(3)
       printf, 1, 'pa    ', inguess(4)
       printf, 1, ''
       printf, 1, '              rr            vrot'
       FOR ii = 0, nrr-1 DO printf, 1, inguess(5+ii), inguess(5+nrr+ii), format='(2f16)'
       printf, 1, ''
       printf, 1, '# Fit result'
       printf, 1, ''
       parnames = ['vsys  ', 'inc   ', 'xpos  ', 'ypos  ', 'pa    ']
       FOR ii = 0, 4 DO IF parinfo(ii).fixed EQ 0 THEN printf, 1, $
         parnames(ii), fitresult(ii), ' +/- ', uncert(ii) ELSE printf, 1, $
         parnames(ii), fitresult(ii)
       printf, 1, ''
       printf, 1, '              rr            vrot'
       FOR ii = 0, nrr-1 DO IF parinfo(5+nrr+ii).fixed EQ 0 THEN printf, 1, $
         fitresult(5+ii), fitresult(5+nrr+ii), ' +/- ', $
         uncert(5+nrr+ii), format='(2f16,a,f16)' ELSE printf, 1, $
         fitresult(5+ii), fitresult(5+nrr+ii), format='(2f16)'
       printf, 1, ''
       printf, 1, 'mean error   ', meanerrv
       printf, 1, 'residual rms ', rms
       close, 1
   ENDIF
   RETURN, fitresult
END
