FUNCTION ssqrt, x
   ;
   ; Signed sqrt function = sign(x)*sqrt(abs(x))
   ;
   ; G. Meurer 03/2005
   jj = where(x LT 0.0, njj)
   y = sqrt(abs(x))
   IF njj GT 0 THEN y[jj] = -1.0*y[jj]
return, y
END 
