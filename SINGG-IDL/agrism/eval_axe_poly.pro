FUNCTION eval_axe_poly, coef, x, y, xref=xref, yref=yref
   ;
   ; evaluate one of those axe polynomial at position(s) x,y
   ;
   ; coef      -> polynomial coeficients in order specified in aXe manual.
   ; x,y       -> col,row pixel position(s) (first pixel = 0,0)
   ; xref,yref -> (optional) reference pixel position.  If set then 
   ;              polynomial is wrt to that position.
   ; output    <- polynomial evaluated at x,y
   ;
   ; G. Meurer, 06/2004
   IF NOT keyword_set(xref) THEN xref = 0.0
   IF NOT keyword_set(yref) THEN yref = 0.0
   dx  = x - xref
   dy  = y - yref
   nc  = n_elements(coef)
   val = 0.0
   IF nc GE 1 THEN val = val + coef[0]
   IF nc GE 3 THEN val = val + coef[1]*dx + coef[2]*dy
   IF nc GE 6 THEN val = val + coef[3]*dx^2 + coef[4]*dx*dy + coef[5]*dy^2
   IF nc GE 10 THEN val = val + coef[6]*dx^3 + coef[7]*dx^2*dy + coef[8]*dx*dy^2 + coef[9]*dy^3
   return, val
END 
