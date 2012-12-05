PRO blem_ccplot_old, x, cfunc, cfit, cmean, crms, ytit=ytit
   ; 
   ; Plot cross-correlation function and it's fit.
   ;
   IF keyword_set(ytit) THEN ytitle=ytit ELSE ytitle='Cross correlation'
   xr       = [min(x), max(x)]
   y0       = min([cfunc, cfit])
   y1       = max([cfunc, cfit])
   dy       = y1 - y0
   yr       = [y0 - 0.05*dy, y1 + 0.05*dy]
   plot,x,cfunc,xrange=xr,xstyle=1,yrange=yr,ystyle=1,$
    xtitle='X',ytitle=ytitle
   ;
   yp       = cmean + 0.0*xr
   oplot,xr,yp,linestyle=0,color=!black
   yp       = cmean + crms + 0.0*xr
   oplot,xr,yp,linestyle=2,color=!red
   yp       = cmean - crms + 0.0*xr
   oplot,xr,yp,linestyle=2,color=!red
   yp       = cmean + 3.0*crms + 0.0*xr
   oplot,xr,yp,linestyle=1,color=!blue
   yp       = cmean - 3.0*crms + 0.0*xr
   oplot,xr,yp,linestyle=1,color=!blue
   oplot,x, cfit, color=!dgreen
END 

