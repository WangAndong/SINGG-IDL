PRO plot_widths, hardfile=hardfile, jpg=jpg
   fhipass    = '../hipass_widths.dat'
   fhalpha    = '../halpha_widths.dat'
   xsize      = 600
   winratio   = 1.0
   charsize   = 1.5
   symsize    = 1.5
   legendpos  = [0.2,0.95]
   ;
   ; read data
   readcol, fhipass, filename, hipassid, hw50max, hw50min, hw20max, $
     hw20min, format='(a,a,f,f,f,f)'
   readcol, fhalpha, filename, hipassid, w50max, w50min, w20max, $
     w20min, format='(a,a,f,f,f,f)'
   toplot50max   = where(hw50max GT 0)
   toplot50min   = where(hw50min GT 0)
   toplot20max   = where(hw20max GT 0)
   toplot20min   = where(hw20min GT 0)
   ;
   ; select ranges to plot
   upper50    = 100.0 * ceil(max([hw50max, w50max] / 100.0))
;   upper20    = 100.0 * ceil(max([hw20max, w20max] / 100.0))
   upper20    = 600.0
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
   ; plot graph 1
   plot, hw50max(toplot50max), w50max(toplot50max), psym=sym(11), $
     xtitle='HI W!d50!n (km/s)', ytitle='H!7a!6 W!d50!n (km/s)', $
     charsize=charsize, symsize=symsize, /isotropic, $
     xrange=[0,upper50], yrange=[0,upper50]
   oplot, hw50min(toplot50min), w50min(toplot50min), psym=sym(12), $
     symsize=symsize
   oplot, [0,1000], [0,1000], linestyle=2
   legend, ['W!d50!umax!n', 'W!d50!umin!n'], psym=[sym(11), sym(12)], $
     symsize=[symsize, symsize], charsize=charsize, $
     position=legendpos*upper50
   ;
   ; make a .jpg file if required
   IF keyword_set(jpg) AND NOT(keyword_set(hardfile)) THEN BEGIN
       im = tvrd(true=3)
       WRITE_JPEG,jpg(0),im,TRUE=3,QUALITY=100
   ENDIF
   ;
   ; make a .ps file if required
   IF keyword_set(hardfile) THEN BEGIN 
      psend, hardfile(0), /noprint, /clobber
      set_plot,'x'
   ENDIF 
   ;
   ; initialise window
   IF keyword_set(hardfile) THEN BEGIN 
      set_plot,'ps',/copy, /interpolate
      IF strpos(strlowcase(hardfile(1)), '.eps') GT 0 THEN $
         device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
         /encapsulated, bits_per_pixel=8 ELSE $
         device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
         bits_per_pixel=8
   ENDIF ELSE BEGIN 
      window, 0, xsize=xsize, ysize=ysize
   ENDELSE 
   ;
   ; plot graph 2
   plot, hw20max(toplot20max), w20max(toplot20max), psym=sym(11), $
     xtitle='HI W!d20!n (km/s)', ytitle='H!7a!6 W!d20!n (km/s)', $
     charsize=charsize, symsize=symsize, /isotropic, $
     xrange=[0,upper20], yrange=[0,upper20]
   oplot, hw20min(toplot20min), w20min(toplot20min), psym=sym(12), $
     symsize=symsize
   oplot, [0,1000], [0,1000], linestyle=2
   legend, ['W!d20!umax!n', 'W!d20!umin!n'], psym=[sym(11), sym(12)], $
     symsize=[symsize, symsize], charsize=charsize, $
     position=legendpos*upper20
   ;
   ; make a .jpg file if required
   IF keyword_set(jpg) AND NOT(keyword_set(hardfile)) THEN BEGIN
       im = tvrd(true=3)
       WRITE_JPEG,jpg(1),im,TRUE=3,QUALITY=100
   ENDIF
   ;
   ; make a .ps file if required
   IF keyword_set(hardfile) THEN BEGIN 
      psend, hardfile(1), /noprint, /clobber
      set_plot,'x'
   ENDIF 
END
