FUNCTION myfun_lin2d, x, p
   ;
   ; 2d linear function for use with mpfit
   ; zz = a + b*xx + c*yy
   ;
   ; G. Meurer  10/2005
   a = p[0]
   b = p[1]
   c = p[2]
   xx = x[*,0]
   yy = x[*,1]
   return, a + b*xx + c*yy
END 
