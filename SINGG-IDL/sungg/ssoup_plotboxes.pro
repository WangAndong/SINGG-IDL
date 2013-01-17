PRO ssoup_plotboxes, ll, bxsiz, sname, band, fbox, fjpg, feps, outline=outline, epilepsy=epilepsy
  ;
  ;  ll     -> logical unit of log file
  ;  bxsiz  -> size of boxes
  ;  sname  -> name of singg/hipass source
  ;  band   -> name of band
  ;  fimage -> Name of image
  ;  fbox   -> name of file containing box data. 
  ;  fjpg   -> name of output file (JPG)
  ;  feps   -> name of output file (EPS)
  ;  outline -> if set outline each box otherwise just the grayscale
  ;             in each box is shown with no outline.
  ;
  ; G. Meurer 6/2011  (ICRAR/UWA)
  ; G. Meurer 9/2012  (ICRAR/UWA)
  ;    * add outline keyword
  ;
  fmtb      = '(f,f,f,f,f,f)'
  prog      = 'SSOUP_PLOTBOXES: '
  charsize  = 2.1
  aspect    = 1.1
  xtitle    = 'x [pixel]'
  ytitle    = 'y [pixel]'
  plog,ll,prog,'--------------------- starting '+prog+'---------------------------------'
  ;
  ; make title
  title     = 'Source: '+sname+' filter: '+band+' file: '+fbox
  ;
  ; read file
  readcol, fbox, bavg, bx, by, bsig, bfit, bresid, format=fmtb
  nb        = n_elements(bavg)
  ;
  ; get rms of residuals
  rms       = sqrt(total(bresid^2)/float(nb))
  ;
  ; determine limits
  xrange    = [min(bx), max(bx)] + [-1.0,1.0]*bxsiz
  yrange    = [min(by), max(by)] + [-1.0,1.0]*bxsiz
  dum1      = [bavg, bfit]
  brange1   = [min(dum1), max(dum1)] ;+ rms*[-0.5, 0.5]
  brange2   = [min(bresid), max(bresid)]
  ;
  plog,ll,prog,'Title = '+title
  plog,ll,prog,'xrange  = '+numstr(xrange[0])+' , '+numstr(xrange[1])
  plog,ll,prog,'yrange  = '+numstr(yrange[0])+' , '+numstr(yrange[1])
  plog,ll,prog,'brange1 = '+numstr(brange1[0])+' , '+numstr(brange1[1])
  plog,ll,prog,'brange2 = '+numstr(brange2[0])+' , '+numstr(brange2[1])
  ;
  ; set window parameters
  xs    = 8.0
  ys    = xs/(3.0*aspect)
  yoff  = 6.0
  xoff  = 0.0
  thick = 1
  charsize = 0.6*charsize
  wxsize   = 1200
  ssoup_plot_init,xs,ys,xoff,yoff
  ;
  ; set grayscale and background, foreground
  loadct, 0
  ;ncolor    = !d.n_colors-1  ; this doesn't work
  ncolor    = 256
  setbgfg, ncolor-1, 0
  ;
  ; turn quantities to plot in to gray levels
  db1       = (brange1[1]-brange1[0])/float(ncolor-1) > 0.0
  db2       = (brange2[1]-brange2[0])/float(ncolor-1) > 0.0
  ibavg     = long((bavg - brange1[0])/db1)
  ibfit     = long((bfit - brange1[0])/db1)
  ibresid   = long((bresid - brange2[0])/db2)
  ;
  ; left panel
  !p.noerase = 0
  !p.multi   = [3, 3, 1] ; left panel
  plot, bx, by, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, $
        xtitle=xtitle, ytitle=ytitle, title='', charsize=charsize, $
        thick=thick, xthick=thick, ythick=thick, charthick=thick, $
        /nodata, /isotropic
  ;
  ; loop through boxes and plot as a gray level with 
  ; the perimeter outlined
  FOR ii = 0, nb-1 DO BEGIN 
     xx      = bx[ii] + bxsiz*[-0.5, 0.5, 0.5, -0.5, -0.5] + [0.5, -0.5, -0.5, 0.5, 0.5]
     yy      = by[ii] + bxsiz*[-0.5, -0.5, 0.5, 0.5, -0.5] + [0.5, 0.5, -0.5, -0.5, 0.5]
     polyfill, xx, yy, color=ibavg[ii]
     IF keyword_set(outline) THEN oplot, xx, yy, thick=1
  ENDFOR
  ;
  ; center panel
  !p.noerase = 0
  !p.multi   = [2, 3, 1] ; center panel
  plot, bx, by, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, $
        xtitle=xtitle, ytitle=ytitle, title=title, charsize=charsize, $
        thick=thick, xthick=thick, ythick=thick, charthick=thick, $
        /nodata, /isotropic
  ;
  ; loop through boxes and plot as a gray level with 
  ; the perimeter outlined
  FOR ii = 0, nb-1 DO BEGIN 
     xx      = bx[ii] + bxsiz*[-0.5, 0.5, 0.5, -0.5, -0.5] + [0.5, -0.5, -0.5, 0.5, 0.5]
     yy      = by[ii] + bxsiz*[-0.5, -0.5, 0.5, 0.5, -0.5] + [0.5, 0.5, -0.5, -0.5, 0.5]
     polyfill, xx, yy, color=ibfit[ii]
      IF keyword_set(outline) THEN oplot, xx, yy, thick=1
  ENDFOR
  ;
  ; right panel
  !p.noerase = 0
  !p.multi   = [1, 3, 1] ; left panel
  plot, bx, by, xrange=xrange, yrange=yrange, xstyle=1, ystyle=1, $
        xtitle=xtitle, ytitle=ytitle, title='', charsize=charsize, $
        thick=thick, xthick=thick, ythick=thick, charthick=thick, $
        /nodata, /isotropic
  ;
  ; loop through boxes and plot as a gray level with 
  ; the perimeter outlined
  FOR ii = 0, nb-1 DO BEGIN 
     xx      = bx[ii] + bxsiz*[-0.5, 0.5, 0.5, -0.5, -0.5] + [0.5, -0.5, -0.5, 0.5, 0.5]
     yy      = by[ii] + bxsiz*[-0.5, -0.5, 0.5, 0.5, -0.5] + [0.5, 0.5, -0.5, -0.5, 0.5]
     polyfill, xx, yy, color=ibresid[ii]
     ;kk      = long(float(!d.n_colors)*float(ii)/float(nb-1))
     ;kk      = long(float(300)*float(ii)/float(nb-1))
     ;polyfill, xx, yy, color=kk
      IF keyword_set(outline) THEN oplot, xx, yy, thick=1
  ENDFOR
  ;
  ; ------------------------------------------------------------------
  ; finish plot
  plog,ll,prog,'finishing. Will write plotfile: '+feps
  !p.multi   = 0
  !p.noerase = 0
  ssoup_plot_finish,fjpg,feps,wxsize,epilepsy=epilepsy
END 
