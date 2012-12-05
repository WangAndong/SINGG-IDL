PRO plot_rmag_w, hardfile=hardfile, jpg=jpg
   fdata      = '../rmag_lineratio.dat'
   xsize      = 600
   winratio   = 1.0
   charsize   = 1.5
   symsize    = 1.5
   position   = [0.16, 0.1, 0.95, 0.9]
   xtitle1    = 'Absolute R-band magnitude (AB scale)'
   xtitle2    = 'Estimated log(w!l6583!n)'
   ytitle     = 'log(w!l6583!n)'
   xrange2    = [-1.3, -0.4]
   xrange1    = (xrange2 + 3.30) / (-0.13)
;   xrange1    = [-15, -22]
;   xrange2    = -0.13 * xrange1 - 3.30
   ;
   ; read data
   readcol, fdata, rmag, estw, ww, format='(x,x,f,f,f)'
   toplot     = where(ww GT 0)
   ;
   ; initialise window
   IF keyword_set(hardfile) THEN BEGIN 
      xs = xsize / 150.0
      ys = xs/winratio
      yoff=1.0
      xoff=1.0
      thick=2
      set_plot,'ps',/copy, /interpolate
      IF strpos(strlowcase(hardfile(0)), '.eps') GT 0 THEN $
         device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
         /encapsulated, bits_per_pixel=8 ELSE $
         device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
         bits_per_pixel=8
      charsize = 0.6*charsize
      symsize  = 0.6*symsize
   ENDIF ELSE BEGIN 
      ysize    = fix(float(xsize)/winratio)
      thick    = 1
      window, 0, xsize=xsize, ysize=ysize
   ENDELSE 
   ;
   ; plot graph
   plot, alog10(estw(toplot)), alog10(ww(toplot)), psym=sym(12), xtitle=xtitle1, $
     ytitle=ytitle, xstyle=5, charsize=charsize, position=position, $
     xrange=xrange2, /isotropic
   oplot, xrange2, xrange2, linestyle=2
   axis, xaxis=0, xtitle=xtitle1, xstyle=1, xrange=xrange1, charsize=charsize
   axis, xaxis=1, xtitle=xtitle2, xstyle=1, xrange=xrange2, charsize=charsize
;   plot, rmag(toplot), alog10(ww(toplot)), psym=sym(12), xtitle=xtitle1, $
;     ytitle=ytitle, xstyle=9, charsize=charsize, position=position, xrange=xrange1
;   oplot, xrange1, xrange2, linestyle=2
;   axis, xaxis=1, xtitle=xtitle2, xstyle=1, xrange=xrange2, charsize=charsize
   ;
   ; make a .jpg file if required
   IF keyword_set(jpg) AND NOT(keyword_set(hardfile)) THEN BEGIN
       im = tvrd(true=3)
       WRITE_JPEG,jpg,im,TRUE=3,QUALITY=100
   ENDIF
   ;
   ; make a .ps file if required
   IF keyword_set(hardfile) THEN BEGIN 
      psend, hardfile, /noprint, /clobber
      set_plot,'x'
   ENDIF 
END
