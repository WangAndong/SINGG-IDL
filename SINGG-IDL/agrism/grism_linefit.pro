PRO grism_linefit, x, y, err, resid, xrange, minsigma, dxfit, p, flag=flag
   ;
   ; x        -> x points 
   ; y        -> y points
   ; err      -> errors
   ; resid    -> residual of continuum fit
   ; xrange   -> range of x values to fit
   ; minsigma -> minimimum sigma
   ; dxfit    -> width of x area to fit
   ; p        <- Gaussian + quadratic fit parameters
   ; 
   ; G.R. Meurer 9/2001
   ;
   ; get good points.  First pass use all of xrange
   ;
   ;forprint, x, y, err, resid
   ;print, xrange
   xcen = 0.5*(min(xrange) + max(xrange))
   ;print,xcen
   dx   = max(xrange) - min(xrange)
   ;print,dx
   good = useinfit(x, xrange, xcen, dx, flag=flag)
   IF good[1] EQ -1 THEN BEGIN
      print, '**** Error no points to fit '
      return
   ENDIF 
   ;
   ; first guess model parameters, better fit points
   modguess, x[good], y[good], resid[good], err[good], minsigma, pstart
   xcen = pstart[3]
   good = useinfit(x, xrange, xcen, dxfit, flag=flag)
   IF good[1] EQ -1 THEN BEGIN
      print, '**** Error no points to fit '
      return
   ENDIF 
   ;
   constrain_modpar, minsigma, xrange, parinfo
   ;
   p = pstart
   p = mpfitfun('GAUSS_N_QUAD', x[good], y[good], err[good], pstart, parinfo=parinfo, /quiet)
END 


