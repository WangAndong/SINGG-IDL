PRO calc_cumulative_plt, arr, xplt, yplt, xrange=xrange
   ;
   ; Calculate quantities to plot in a cumulative
   ; distribution plot.
   ;
   ; G. Meurer 09/2004
   ;
   na           =  n_elements(arr)
   kk           =  sort(arr)
   jj           =  lindgen(na)
   xplt         =  make_array(2*na, /float, value=0.0)
   yplt         =  make_array(2*na, /float, value=0.0)
   xplt[jj*2]   =  arr[kk]
   xplt[jj*2+1] =  arr[kk]
   yplt[jj*2]   =  findgen(na)/float(na)
   yplt[jj*2+1] = (1.0 + findgen(na))/float(na)
   IF keyword_set(xrange) THEN BEGIN 
      xplt      = [min(xrange), xplt, max(xrange)]
      yplt      = [0.0, yplt, 1.0]
   ENDIF 
END 
