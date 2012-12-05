FUNCTION shunt_distortmod, x, p
   ;
   ; Extract, xx, yy, mag, and color from concatenated independent variables
   nn    = n_elements(x)
   jj    = nn / 4
   xx    = x[0:jj-1]
   yy    = x[jj:2*jj-1]
   mag   = x[2*jj:3*jj-1]
   color = x[3*jj:4*jj-1]
   zz    = p[0] + p[1]*xx + p[2]*yy + p[3]*color 
   return, zz
END 
