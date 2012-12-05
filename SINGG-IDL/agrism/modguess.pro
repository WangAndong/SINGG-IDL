PRO modguess, x, y, resid, err, minsigma, p
   ;
   ; initial guess model parameters
   ;
   ; x        -> x positions to fit
   ; y        -> y positions to fit
   ; resid    -> residuals of continuum fit
   ; err      -> error in y
   ; minsigma -> minimimum sigma
   ; p        <- model parameters
   ;
   ; G.R. Meurer 9/2002
   ; G.R. Meurer 11/2004 pick line by peak in resid/err
   t       = reverse(sort(resid/err))
   p       = make_array(6,/double)
   p[0]    = y[t[0]] - resid[t[0]]
   p[1]    = 0.0d0
   p[2]    = 0.0d0
   p[3]    = x[t[0]]
   p[4]    = minsigma
   p[5]    = resid[t[0]] * sqrt(2.0d0 * !dpi) * p[4]
END 

