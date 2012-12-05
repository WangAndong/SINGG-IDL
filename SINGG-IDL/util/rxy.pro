FUNCTION rxy, x, y, prob, tt
   ;
   ; Return Pearson's correlation coefficient, of two vectors x y
   ;
   ; G. Meurer ??/200?
   ; G. Meurer 02/2008 improved comments
   ; G. Meurer 08/2008 calculate significance level following 
   ;                   methods in numerical recipes & 
   ;                   http://faculty.vassar.edu/lowry/rsig.html
   ;
   tiny = 1.0d-20  ; to regularize when data perfectly correlated following numerical recipes
   xx   = double(x - total(x)/float(n_elements(x)))
   yy   = double(y - total(y)/float(n_elements(y)))
   ;
   ; enhancements 08/2008
   nn   = double(min([n_elements(x), n_elements(y)]))
   _rxy = total(xx*yy)/sqrt(total(xx*xx)*total(yy*yy))
   ;
   ; calculate probability that correlation is not 0
   df   = double(nn-2)
   tt   = _rxy*sqrt(df/(((1.0-_rxy)+tiny)*((1+_rxy)+tiny)))
   prob = 1.0d0 - t_pdf(tt,df)
   ;
   return, float(_rxy)
END 
