PRO constrain_gaussp_n_const, minsigma, parinfo
   ;
   ; Constrain model : sigma GE minsigma, peak GE 0.0
   ;
   ; G.R. Meurer 9/2002
   parinfo               = replicate({fixed:0, limited:[0,0], limits:[0.D,0.D]},4)
   parinfo[2].limited[0] = 1
   parinfo[2].limits[0]  = minsigma
   parinfo[3].limited[0] = 1
   parinfo[3].limits[0]  = 0.0
END 

