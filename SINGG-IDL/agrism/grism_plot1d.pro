PRO grism_plot1d, bintab, id, prefix=prefix, orient=orient, lrange=lrange
   ;
   ; 1d plot of grism spectra in count units.
   ;
   ; bintab -> aXe binary table containing spectrum etc.
   ; id     -> ID number to be printed as spectrum title
   ; prefix -> if set, hardcopy ps and jpg files are made with this prefix.
   ; orient -> if set, this contains a three element array [a, b, theta]
   ;           where a,b are the semi major and semi-minor axis lengths
   ;           and theta is the position angle CCW from horizontal.
   ;           This determines the resolution of the spectrum which is 
   ;           plotted as a red horizontal bar.
   ; lrange -> if set, a two vector array giving the wavelength
   ;           range to plot.
   ;
   ; G. Meurer late 2002 - originally written
   ;           07/2004 - now makes ps & png versions if prefix set.
   ;                     code documented, angstrom symbol used in plots
   ;           08/2005 - changing from png to jpg
   ;
   cmd_convrt = 'convert'   ; ps -> jpg conversion command
   aa         = angstsym(0)               ; angstrom symbol
   ;
   IF n_elements(lrange) EQ 2 THEN xrange = lrange ELSE xrange = [5600.0, 10200.0]
   cmaxdef = 150.0
   xtitle  = 'Wavelength ['+aa+']'
   ytitle  = 'Counts'
   count   = 0
   ;
   errspec, bintab, espec
   cmax    = max([cmaxdef,1.2*max(bintab.count),2.2*max(espec)])
   yrange  = [-cmaxdef,cmax]
   ;
   ; set postscript hardcopy parameters if prefix set
   IF keyword_set(prefix) THEN BEGIN 
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
   ;
   ; First dummy plot 
   setplotcolors
   plot, bintab.lambda, bintab.count, xrange=xrange, yrange=yrange, charsize=charsize, $
         xstyle=1, ystyle=1, xtitle=xtitle, ytitle=ytitle, title=strtrim(string(id),2), psym=10
   ;
   ; Contamination plot, underneath everything...
   grism_oplotcontam, bintab, xrange, yrange, color=!pink
   ;
   ; plot errors, then replot line and box
   oplot, bintab.lambda, espec, color=!purple, psym=10
   oplot, bintab.lambda, 2.0*espec, color=!dgreen, psym=10
   oplot, xrange, 0.0*xrange
   plot, bintab.lambda, bintab.count, xrange=xrange, yrange=yrange, charsize=charsize, $
         xstyle=1, ystyle=1, xtitle=xtitle, ytitle=ytitle, title=strtrim(string(id),2), psym=10, $
         /noerase
   ;
   ; show resolution if orient set
   IF keyword_set(orient) THEN BEGIN 
      respix   = max([2.0*sqrt((0.5*((orient[0]*cos(orient[2]))^2 + (orient[1]*sin(orient[2]))^2))),2.5])
      resang   = respix*(max(bintab.lambda) - min(bintab.lambda))/(float(n_elements(bintab.lambda)) - 1.0)
      ; print,respix,resang
      xlin     = 0.5*(xrange[0] + xrange[1]) + resang*[-0.5, 0.5]
      ylin     = yrange[0] + 0.9*(yrange[1] - yrange[0]) + [0.0,0.0]
      oplot, xlin, ylin, color=!red, thick=3.0
   ENDIF 
   ;
   ; if prefix set, finish postscript and write jpg file
   IF keyword_set(prefix) THEN BEGIN 
      ;
      ; finish postscript file
      pfile    = namps(strtrim(prefix,2),id)
      jfile    = namjpg(strtrim(prefix,2),id)
      device,/close
      spawn,'mv -f idl.ps '+pfile 
      ;
      ; make jpg version, may necissitate moving file to
      ; output directory if prefix includes a directory name.
      spawn, cmd_convrt+' '+pfile+' '+jfile
      ;k = strpos(prefix,'/',/reverse_search)
      ;IF k GE 0 THEN BEGIN
      ;   pfx  = strmid(prefix,k+1)
      ;   spawn,'mv -f '+namjpg(pfx,id)+' '+namjpg(strtrim(prefix,2),id)
      ;ENDIF 
      ;
      ; reset for X device line plots
      device,/portrait
      set_plot,'X'
      setbgfg,!white,!black
      setplotcolors
   ENDIF 
END 
