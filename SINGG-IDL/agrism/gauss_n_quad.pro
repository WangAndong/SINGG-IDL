FUNCTION gauss_n_quad, x, p
   ;
   ; Gaussian + Quadratic polynomial to be used with mpfit
   ;
   ; G.R. Meurer
   ;
   ; p[0] : constant term
   ; p[1] : linear term
   ; p[2] : quadratic term
   ; p[3] : center of gaussian
   ; p[4] : sigma of gaussian
   ; p[5] : area of gaussian
   ;
   return, p[0] + p[1]*x + p[2]*x*x + gauss1(x,p[3:5])
END 

