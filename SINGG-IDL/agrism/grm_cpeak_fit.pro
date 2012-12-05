PRO grm_cpeak_fit, cfunc, crms, cfit, icen, lev, height, shift, width, window=window
   ;
   ; Fit a Gaussian to the peak in the correlation function.
   ; cfunc  -> correlation function
   ; crms   -> clipped rms of cfunc, used as error
   ; cfit   <- The Gaussian fit to the correlation function
   ; icen   <- center of source to nearest pixel.  
   ;           shifts are relative to this poistion
   ; height <- Height (amplitude) of correlation function peak
   ; shift  <- Shift in peak compared to channel 0
   ; width  <- width of correlation peak in channels
   ; window -> If set the window in pixels (array elements) 
   ;           around the peak to do the fit.
   ;
   ; G.R. Meurer 12/2002
   minsigma = 1.5/2.35
   ;
   n      = n_elements(cfunc)
   nhalf  = n_elements(cfunc)/2
   k      = reverse(sort(cfunc))
   ish    = fix(nhalf) - k[0]
   ysh    = shift(cfunc, ish)
   eysh   = 0.0*ysh + crms
   xsh    = findgen(n)
   ;
   ; initial guess and constraints to fit
   guess_gaussp_n_const, xsh, ysh, minsigma, pstart
   constrain_gaussp_n_const, minsigma, parinfo
   ;
   ; set good pixels for fitting
   IF keyword_set(window) THEN k = where(xsh GE pstart[1]-0.5*window AND xsh LE pstart[1]+0.5*window) $
    ELSE k = indgen[n]
   p      = pstart
   p      = mpfitfun('GAUSSP_N_CONST', xsh[k], ysh[k], eysh[k], pstart, parinfo=parinfo, /quiet)
   ;
   ; remove shift, evaluate
   cfit   = shift(gaussp_n_const(xsh, p), -1*ish)
   lev    = p[0]
   height = p[3]
   shift  = (float(icen) + p[1] - ish) MOD float(n) - float(icen)
   width  = p[2]*2.35
   print,lev,height,shift,width
END 

