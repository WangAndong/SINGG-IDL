PRO guess_gaussp_n_const, x, y, minsigma, p
   ;
   ; initial guess model parameters for a Gaussian + constant
   ;
   ; x        -> X positions to fit
   ; y        -> y positions to fit
   ; minsigma -> minimimum sigma
   ; p        <- model parameters
   ;
   ; G.R. Meurer 12/2002
   grm_avsigclip, y, 3.0, 3, ym, yrms, nuse, nrej, nit, /verbose
   k       = reverse(sort(y))
   p       = make_array(4,/double)
   p[0]    = ym
   p[1]    = x[k[0]]
   p[2]    = minsigma
   p[3]    = y[k[0]] - ym
   print,p
END 

