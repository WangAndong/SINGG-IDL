PRO grism_fplot1d, bintab, id, lrange, prefix=prefix, orient=orient, lnorm=lnorm
   ;
   ; 1d plot of grism spectra in flux density units
   ;
   ; bintab -> aXe binary table containing spectrum etc.
   ; id     -> ID number to be printed as spectrum title
   ; lrange -> a two vector array giving the wavelength
   ;           range to plot.
   ; prefix -> if set, hardcopy ps and png files are made with this prefix.
   ; orient -> if set, this contains a three element array [a, b, theta]
   ;           where a,b are the semi major and semi-minor axis lengths
   ;           and theta is the position angle CCW from horizontal.
   ;           This determines the resolution of the spectrum which is 
   ;           plotted as a red horizontal bar.
   ; lnorm  -> If set, this is a two element array giving
   ;           wavelength range for determining y limits of plot.
   ;
   ; G. Meurer late 2002 - originally written
   ;           07/2004 - now makes ps & png versions if prefix set.
   ;                     code documented, angstrom symbol used in plots
   ;           08/2005 - changing from png to jpg
   ;           07/2006 - force move using /bin/mv -f
   ;
   cmd_convrt = 'convert'   ; ps -> jpg conversion command
   aa         = angstsym(0)               ; angstrom symbol
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
   xtitle  = 'Wavelength ['+aa+']'
   ytitle  = 'flux [1e-18 erg/cm^2/s/'+aa+']'
   lam     = bintab.lambda
   flx     = bintab.flux / 1.0e-18
   eflx    = bintab.ferror / 1.0e-18
   emin    = flx - eflx
   emax    = flx + eflx
   IF keyword_set(lnorm) THEN w = where(lam GE min(lnorm) AND lam LE max(lnorm),nw) $
    ELSE w = where(lam GE min(lrange) AND lam LE max(lrange),nw)
   IF nw LE 0 THEN BEGIN 
      print, '**** Warning in GRISM_FPLOT1D, no valid pixels for spectrum with id=', id
      print, 'lrange = ', lrange, '  lambda = ...'
      print, lam
      return 
   ENDIF 
   fmax    = 1.2*max(emax[w])  
   fmin    = 1.2*min([0.0,min(emin[w])])
   yrange  = [fmin,fmax]
   ;
   ; initial plot
   setplotcolors
   plot, lam, flx, xrange=lrange, yrange=yrange, xstyle=1, ystyle=1, charsize=charsize, $
    xtitle=xtitle, ytitle=ytitle, title=strtrim(string(id),2), psym=10
   ;
   ; Overplot contamination
   grism_oplotcontam, bintab, lrange, yrange, color=!pink
   ;
   ; plot errors, then replot line and box
   oplot, lam, emin, color=!gray, psym=10
   oplot, lam, emax, color=!gray, psym=10
   plot, lam, flx, xrange=lrange, yrange=yrange, xstyle=1, ystyle=1, charsize=charsize, $
    xtitle=xtitle, ytitle=ytitle, title=strtrim(string(id),2), psym=10, /noerase
   ;
   ; plot resolution
   IF keyword_set(orient) THEN BEGIN 
      respix   = max([2.0*sqrt((0.5*((orient[0]*cos(orient[2]))^2 + (orient[1]*sin(orient[2]))^2))),2.5])
      resang   = respix*(max(lam) - min(lam))/(float(n_elements(lam)) - 1.0)
      ; print,respix,resang
      xlin     = 0.5*(lrange[0] + lrange[1]) + resang*[-0.5, 0.5]
      ylin     = yrange[0] + 0.9*(yrange[1] - yrange[0]) + [0.0,0.0]
      oplot, xlin, ylin, color=!red, thick=3.0
   ENDIF 
   ;
   ; if prefix set, finish postscript and write png file
   IF keyword_set(prefix) THEN BEGIN 
      ;
      ; finish postscript file
      pfile    = namps(strtrim(prefix,2),id)
      jfile    = namjpg(strtrim(prefix,2),id)
      device,/close
      spawn,'/bin/mv -f idl.ps '+pfile 
      ;
      ; make png version, may necissitate moving file to
      ; output directory if prefix includes a directory name.
      spawn, cmd_convrt+' '+pfile+' '+jfile
      ;k = strpos(prefix,'/',/reverse_search)
      ;IF k GE 0 THEN BEGIN
      ;   pfx  = strmid(prefix,k+1)
      ;   spawn,'mv '+nampng(pfx,id)+' '+nampng(strtrim(prefix,2),id)
      ;ENDIF 
      ;
      ; reset for X device line plots
      device,/portrait
      set_plot,'X'
      setbgfg,!white,!black
      setplotcolors
   ENDIF 
END 
