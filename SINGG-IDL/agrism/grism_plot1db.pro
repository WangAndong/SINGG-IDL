PRO grism_plot1db, x, y, err, ysm, ygmod, xrange, $
                   xtitle=xtitle, ytitle=ytitle, title=title, pfile=pfile, vline=vline
   ;
   ; plot grism spectrum with errors, smooth spectrum and 
   ; gaussian fit
   ;
   ; x      -> X values to plot (wavelength)
   ; y      -> Y values to plot (spectrum)
   ; err    -> error on y
   ; ysm    -> smooth spectrum to plot
   ; ygmod  -> Gaussian models (not including continuum)
   ; xrange -> range of xvalues to plot.
   ; xtitle -> plot xtitle
   ; ytitle -> plot ytitle
   ; title  -> plot title
   ; pfile  -> output hardcopy if set
   ; vline  -> position to draw vertical line.
   ;
   ; G. Meurer 2002 (originally written)
   ; G. Meurer 07/2004 - pfile now set to be a postscript file, 
   ;                     doc written.
   ; G. Meurer 07/2006 - force move with /bin/mv -f
   ;
   cmd_ps2png = '~meurer/bin/mb_ps2png'   ; ps -> png conversion command
   aa         = angstsym(0)               ; angstrom symbol
   ;
   IF NOT keyword_set(title) THEN title = ' ' 
   IF NOT keyword_set(xtitle) THEN xtitle  = 'Wavelength ['+aa+']'
   IF NOT keyword_set(ytitle) THEN ytitle  = 'flux'
   ye1     = y + err
   ye2     = y - err
   w       = where(x GE min(xrange) AND x LE max(xrange))
   fmax    = 1.2*max(ye1[w])  
   fmin    = 1.2*min([0.0,min(ye2[w])])
   yrange  = [fmin,fmax]
   IF keyword_set(pfile) THEN BEGIN 
      xs       = 14.14 ; cm
      ys       =  6.4  ; cm
      xoff     =  0. ; cm
      yoff     =  0. ; cm
      charsize =  1.0
      set_plot,'ps'
      device,/portrait,xs=xs,ys=ys,yo=yoff,xo=xoff,/color
   ENDIF ELSE BEGIN 
      charsize = 1.0
   ENDELSE 
   setplotcolors
   plot, x, y, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, $
    xtitle=xtitle, ytitle=ytitle, title=title, psym=10, charsize=charsize
   oplot, x, ye1, psym=10, color=!gray
   oplot, x, ye2, psym=10, color=!gray
   oplot, x, y, psym=10, thick=1.5
   ytot    = ysm + ygmod
   oplot, x, ytot, color=!red
   oplot, x, ysm, color=!blue
   ;
   ; plot vertical dotted lines if necessary 
   IF n_elements(vline) GT 0 THEN BEGIN 
      FOR j = 0, n_elements(vline)-1 DO BEGIN 
         xv = [vline[j], vline[j]]
         oplot, xv, yrange, linestyle=2, color=!dgreen
      ENDFOR 
   ENDIF 
   ;
   IF keyword_set(pfile) THEN BEGIN 
      ;
      ; finish postscript file
      device,/close
      spawn,'/bin/mv -f idl.ps '+pfile 
      ;
      ; make png version, may necissitate moving file to
      ; output directory if prefix includes a directory name.
      spawn, cmd_ps2png+' '+pfile
      k = strpos(pfile,'/',/reverse_search)
      IF k GE 0 THEN BEGIN
         j    = strpos(pfile,'.',/reverse_search)
         dnam = strmid(pfile,0,k+1)
         spawn,'/bin/mv '+strmid(pfile,k+1,j-k)+'png '+dnam
      ENDIF 
      ;
      ; reset for X device line plots
      device,/portrait
      set_plot,'X'
      setbgfg,!white,!black
      setplotcolors
   ENDIF 
   ; makepng,pfile,/color
END 

