FUNCTION myfun_lin1d, x, p
   ;
   ; 1d linear function for use with mpfit
   ; zz = a + b*xx 
   ;
   ; G. Meurer  10/2005
   a = p[0]
   b = p[1]
   return, a + b*x
END 
