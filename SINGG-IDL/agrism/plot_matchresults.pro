PRO plot_matchresults
   ;
   ; Make diagnostic plots of match results
   ;
   ; G. Meurer 08/2005
   ;
   filma   = 'matched.mtA'
   filmb   = 'matched.mtB'
   filua   = 'matched.unA'
   filub   = 'matched.unB'
   expand  = 20.0
   ;
   ; read in files
   readcol, filma, idma, xma, yma, magma
   readcol, filmb, idmb, xmb, ymb, magmb
   readcol, filua, idua, xua, yua, magma
   readcol, filub, idub, xub, yub, magub
   ;
   ; Calculate plot limits
   xx      = [xma, xmb, xua, xub]
   yy      = [yma, ymb, yua, yub]
   xrange  = [min(xx), max(xx)]
   yrange  = [min(yy), max(yy)]
   dx      = xrange[1] - xrange[0]
   dy      = yrange[1] - yrange[0]
   xrange  = xrange + 0.05*dx*[-1.0, 1.0]
   yrange  = yrange + 0.05*dy*[-1.0, 1.0]
   ;
   ; calculate offset vectors
   dx      = expand*(xma - xmb)
   dy      = expand*(yma - ymb)
   ;
   ; plot the matched objects with vectors, as well as
   ; unmatched objects
   setplotcolors
   plot, xmb, ymb, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, psym=sym(1), $
         xtitle='X [pixel]', ytitle='Y pixel', symsize=1.5
   grm_partvelvec, dx, dy, xmb, ymb, /over
   oplot, xua, yua, psym=sym(6), symsize=0.8, color=!blue
   oplot, xub, yub, psym=sym(9), symsize=0.8, color=!red

   
END 
