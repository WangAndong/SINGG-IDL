PRO guessp_gauss_n_quad, x, y, minsigma, p
   ;
   ; initial guess model parameters
   ;
   ; x        -> x positions to fit
   ; y        -> y positions to fit
   ; minsigma -> minimimum sigma
   ; p        <- model parameters
   ;
   ; G.R. Meurer 9/2002
   t       = reverse(sort(y))
   p       = make_array(6,/double)
   p[0]    = y[t[0]]
   p[1]    = 0.0d0
   p[2]    = 0.0d0
   p[3]    = x[t[0]]
   p[4]    = minsigma
   p[5]    = resid[t[0]] * sqrt(2.0d0 * !dpi) * p[4]
END 

