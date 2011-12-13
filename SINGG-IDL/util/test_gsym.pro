PRO test_gsym, fill=fill, flop=flop, angle=angle, hardfile=hardfile
   ;
   ; plot symbols that can be created with gsym
   ;
   ; G. Meurer 11/2010
   xrange    = [-0.5, 9.5]
   yrange    = [-5.0, 109.0]
   xtitle    = '!3 <opt>'
   ytitle    = '!3 <nside>*10'
   title     = '!3 gsym(<nside><opt>)'
   charsize  = 1.5
   symsize   = 4.5
   thick     = 1
   aspect    = 1.5
   ;
   ; set plot parameters
   IF keyword_set(hardfile) THEN BEGIN 
      xs    = 8.0
      ys    = xs/aspect
      yoff  = 6.0
      xoff  = 0.0
      thick = 1
      set_plot,'ps',/copy, /interpolate
      IF strpos(strlowcase(hardfile), '.eps') GT 0 THEN $
         device,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color,/encapsulated ELSE $
         device,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color
      charsize = 0.6*charsize
      symsize  = 0.6*symsize
   ENDIF ELSE BEGIN 
      wxsize   = 800
      wysize   = fix(float(wxsize/aspect))
      charsize = charsize*wxsize/800.0
      symsize  = symsize*wxsize/800.0
      thick    = 1
      window, 0, xsize=wxsize, ysize=wysize
   ENDELSE 
   ;
   plot, xrange, yrange, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, /nodata, $
         xtitle=xtitle, ytitle=ytitle, title=title, charsize=charsize, $
         thick=thick, charthick=thick, xthick=thick, ythick=thick
   ;
   FOR ns = 0, 10 DO BEGIN 
      yy     = 10*[float(ns)]
      FOR ii = 0, 9 DO BEGIN 
         xx  = [float(ii)]
         sel = 10*ns+ii
         oplot, xx, yy, psym=gsym(sel, fill=fill, flop=flop, angle=angle, thick=thick), symsize=symsize
      ENDFOR 
   ENDFOR 
END 
