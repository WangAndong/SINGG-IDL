PRO blem_ccplot, icen, cfunc, cfit, cmean, crms, title=title
   ; 
   ; Plot cross-correlation function and it's fit.
   ;
   ; because CRcors wrap put zeropt at icen
   x        = findgen(n_elements(cfunc)) - icen
   cfunc2   = shift(cfunc, icen)
   cfit2    = shift(cfit, icen)
   ytitle   = 'Cross correlation'
   xr       = [min(x), max(x)]
   y0       = min([cfunc, cfit])
   y1       = max([cfunc, cfit])
   dy       = y1 - y0
   yr       = [y0 - 0.05*dy, y1 + 0.2*dy]
   plot,x,cfunc2,xrange=xr,xstyle=1,yrange=yr,ystyle=1,$
    xtitle='Offset',ytitle=ytitle, title=title
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
   oplot,x, cfit2, color=!dgreen
END 

