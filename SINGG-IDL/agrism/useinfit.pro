FUNCTION useinfit, x, xrange, xcen, dxfit, flag=flag
   ;
   ; determine which points to use in fit
   ;
   ; x      -> x points 
   ; xrange -> range of x values to fit
   ; xcen   -> initial guess of gaussian center
   ; dxfit  -> width of x area to fit
   ; good   <- indecis of points to use in fit
   ; flag   -> if set this is a vector where 0 are good 
   ;           points, others are excluded from fit.
   ;
   ; G.R. Meurer 9/2002
   xmin = max([min(xrange), xcen-0.5*dxfit])
   xmax = min([max(xrange), xcen+0.5*dxfit])
   IF NOT keyword_set(flag) THEN BEGIN 
      good = where(x GE xmin AND x LE xmax, ngood)
   ENDIF ELSE BEGIN 
      good = where(x GE xmin AND x LE xmax AND flag EQ 0, ngood)
   ENDELSE 
   IF ngood LE 0 THEN good = -1
   ;
   return, good
END 

