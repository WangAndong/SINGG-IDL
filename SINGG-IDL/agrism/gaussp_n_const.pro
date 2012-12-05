FUNCTION gaussp_n_const, x, p
   ;
   ; Gaussian + Quadratic polynomial to be used with mpfit
   ;
   ; G.R. Meurer
   ;
   ; p[0] : constant term
   ; p[1] : center of gaussian
   ; p[2] : sigma of gaussian
   ; p[3] : peak of gaussian
   ;
   return, p[0] + gauss1(x,p[1:3],/peak)
END 

