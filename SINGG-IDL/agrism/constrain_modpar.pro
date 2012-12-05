PRO constrain_modpar, minsigma, xrange, parinfo
   ;
   ; Constrain model : sigma GE minsigma, area GE 0.0
   ;
   ; G.R. Meurer 9/2002
   parinfo               = replicate({fixed:0, limited:[0,0], limits:[0.D,0.D]},6)
   parinfo[3].limited[0] = 1
   parinfo[3].limited[1] = 1
   parinfo[3].limits[0]  = min(xrange)
   parinfo[3].limits[1]  = max(xrange)
   parinfo[4].limited[0] = 1
   parinfo[4].limits[0]  = minsigma
   parinfo[5].limited[0] = 1
   parinfo[5].limits[0]  = 0.0
END 

