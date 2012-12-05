FUNCTION polysurf, xx, yy, par, describe=describe
  ;
  ; function defining a 2d polynomial surface.
  ;   xx  -> X coordinate
  ;   yy  -> Y coordinate
  ;   par -> polynomial paramaters.  The number of elements
  ;          in this array detrmine order of the fit.
  ;           1: constant level
  ;           3: plane
  ;           6: quadratic
  ;          10: cubic
  ;          If the number of parameters does not equal one of these
  ;          then the stop is used to halt processing.
  ;   describe -> if this is set then a string array describing each 
  ;               of the parameters is returned
  ;
  ; The surface is then 
  ;
  ; polysurf = par[0] + par[1]*xx + par[2]*yy + $
  ;            par[3]*xx^2 + par[4]*xx*yy +par[5]*yy^2 + $
  ;            par[6]*xx^3 + par[7]*xx^2*yy + par[8]*xx*yy^2 + par[9]*yy^3
  ;
  ; This function is designed to be used by mpfit2dfun
  ;
  ; G. Meurer (7/2010)
  goodnp    = [1,3,6,10]
  dstr      = ['constant', 'x', 'y', 'x^2', 'x*y', 'y^2', 'x^3', 'x^2*y', 'x*y^2', 'y^3' ]
  ;
  ; determine whether a legal number of parameters has been passed
  np        = n_elements(par)
  jj        = where(goodnp EQ np, njj)
  IF njj NE 1 THEN stop, 'POLYSURF: Wrong number of parameters sent.  Should be 1,3,6, or 10. Np = '+numstr(np)
  if not keyword_set(describe) then begin 
     ; 
     zz        = par[0]
     IF np GE 3 THEN zz = zz + par[1]*xx + par[2]*yy
     IF np GE 6 THEN zz = zz + par[3]*xx^2 + par[4]*xx*yy +par[5]*yy^2
     IF np EQ 10 THEN zz = zz + par[6]*xx^3 + par[7]*xx^2*yy + par[8]*xx*yy^2 + par[9]*yy^3
  endif else begin
     zz        = dstr[0:np-1]
  endelse 
  ;
  return, zz
END 
