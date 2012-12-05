PRO ssoup_plotsprofs_old, ll, sname, fsprof, fsprof0, fplot
  ;
  ; plot surface brightness and surface colour profiles
  ;
  ; G. Meurer 6/2010
  snlimit   = 3.0
  fmti      = '(f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f)'
  prog      = 'SSOUP_PLOTSPROFS_OLD: '
  plog,ll,prog,'--------------------- starting '+prog+'---------------------------------'
  ;
  ; some plot parameters
  aa        = angstsym()
  rtitle    = '!3 semi-major axis [arcsec]'
  hftitle   = '!3 log(F!dH!4a!3!n/f!dFUV!n,f!dR!N ['+aa+'])'
  abtitle   = '!4 l!3 [ABmag arcssec!u-2!n]'
  abctitle  = '!3 surface colour'
  charsize  = 2.0
  symsize   = 1.3
  thick     = 2
  ;
  ; maximum allowed errors
  eloglim   = alog10(1.0+1.0/snlimit) ; max error in log fluxes or surface brightness
  emaglim   = 2.5*eloglim             ; max error in flux and surface brightness magnitude 
  ecloglim  = sqrt(2.0)*eloglim       ; max error in log ratios
  ecmaglim  = sqrt(2.0)*emaglim       ; max error in colors
  ;
  ; read in profile files
  plog,ll,prog,'reading in (not dust corrected) surface brightness profile file: '+fsprof
  readcol, fsprof, sma, sr, esr, sha, eshas, eshac, eshat, snuv, esnuv, sfuv, esfuv, $
           scfn, escfn, scnr, escnr, slewr, eslewr, slewf, eslewf, format=fmti
  np        = n_elements(sma)
  plog,ll,prog,'reading in (dust corrected) surface brightness profile file: '+fsprof0
  readcol, fsprof0, sma0, sr0, esr0, sha0, eshas0, eshac0, eshat0, snuv0, esnuv0, sfuv0, esfuv0, $
           scfn0, escfn0, scnr0, escnr0, slewr0, eslewr0, slewf0, eslewf0, format=fmti
  np0       = n_elements(sma0)
  ;
  ; determine max radii and corresponding pointers
  plog,ll,prog,'determining maxima radii, and points to plot'
  ;
  jr        = where(esr LE emaglim, njr)
  IF njr GT 0 THEN jmax_r = max(jr) - 1 ELSE jmax_r = np-1
  rmax_r    = sma[jmax_r]
  ;
  jh        = where(eshat LE eloglim, njh)
  IF njh GT 0 THEN jmax_h = max(jh) - 1 ELSE jmax_h = np-1
  rmax_h    = sma[jmax_h]
  ;
  jn        = where(esnuv LE emaglim, njn)
  IF njn GT 0 THEN jmax_n = max(jn) - 1 ELSE jmax_n = np-1
  rmax_n    = sma[jmax_n]
  ;
  jf        = where(esfuv LE emaglim, njf)
  IF njf GT 0 THEN jmax_f = max(jf) - 1 ELSE jmax_f = np-1
  rmax_f    = sma[jmax_f]
  ;
  jmax_fn   = min([jmax_f,jmax_n])
  rmax_fn   = sma[jmax_fn]
  ;
  jmax_nr   = min([jmax_n,jmax_r])
  rmax_nr   = sma[jmax_nr]
  ;
  jmax_hr   = min([jmax_h,jmax_r])
  rmax_hr   = sma[jmax_hr]
  ;
  jmax_hf   = min([jmax_h,jmax_f])
  rmax_hf   = sma[jmax_hf]
  ;
  jrn       = where(esr LE emaglim AND esfuv LE emaglim, njrn)
  jfn       = where(esfuv LE emaglim AND esnuv LE emaglim, njfn)
  jhr       = where(eshat LE eloglim AND esr LE emaglim, njhr)
  jhf       = where(eshat LE eloglim AND esfuv LE emaglim, njhf)
  ;
  rmax      = 1.05*max([rmax_r,rmax_h,rmax_n,rmax_f,rmax_fn,rmax_nr,rmax_hr,rmax_hf])
  plog,ll,prog,'maximum R band radius:      '+numstr(rmax_r)
  plog,ll,prog,'maximum Halpha band radius: '+numstr(rmax_h)
  plog,ll,prog,'maximum NUV band radius:    '+numstr(rmax_n)
  plog,ll,prog,'maximum FUV band radius:    '+numstr(rmax_f)
  plog,ll,prog,'maximum radius in plots:    '+numstr(rmax)
  ;
  ; determine file type
  pp        = strpos(fplot,'.')+1
  ftype     = strlowcase(strmid(fplot,pp))
  ;
  ; set plot limits
  rrange    = [0.0,rmax]
  hfrange   = [-0.2,3.2]
  abrange   = [32.0,14.0]
  shrange   = -18.5 + [0.0,-0.4*(abrange[1]-abrange[0])]
  abcrange  = [-1.0,6.0]
  ;
  ; put Halpha on mag scale
  mmm       = abrange[0] + 2.5*shrange[0]
  msha      = mmm - 2.5*sha
  meshat    = 2.5*eshat
  msha0     = mmm - 2.5*sha0
  meshat0   = 2.5*eshat0
  ;
  ; plot set up
  !p.multi = [0, 1, 3]          ; Three panel plot
  IF ftype NE 'jpg' THEN BEGIN 
     plog,ll,prog,'will write a postscript file'
     xs       = 6.5
     ys       = 1.25*xs
     yoff     = 3.
     xoff     = 1.2
     thick    = 3
     set_plot,'ps',/copy, /interpolate
     IF strpos(strlowcase(fplot), '.eps') GT 0 THEN $
           device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color,/encapsulated ELSE $
           device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color
     ansize   = 0.6
  ENDIF  ELSE BEGIN 
     plog,ll,prog,'will plot to screen and write a jpg file'
     ;set_plot,'X',/copy, /interpolate
     ;device, decomposed=0, retain=2
     ;device, true_color=24
     ;device, get_visual_depth=depth
     wxsize   = 600
     wysize   = fix(1.25*float(wxsize))
     window, 0, xsize=wxsize, ysize=wysize
     ansize   = 1.0
     thick    = 1
  ENDELSE 
  setplotcolors
  setbgfg,!white,!black
  ;
  ; ------------------------------------------------------------------
  ; panel 1
  ; R, FUV, NUV Halpha radial surface brightness profile
  plog,ll,prog,'plotting surface brightness profiles'
  !p.noerase = 1
  plot, sma[0:1], sr[0:1], xrange=rrange, yrange=abrange, xstyle=1, ystyle=1, $
        charsize=charsize, symsize=symsize, thick=thick, xthick=thick, ythick=thick, $
        xtitle=rtitle, ytitle=abtitle, title=sname, charthick=thick, $
        xmargin=[8,8], ymargin=[2,4], /nodata
  IF njr GT 0 THEN BEGIN 
     oplot, sma[jr], sr[jr], thick=thick, color=!dorange
     oplot, sma0[jr], sr0[jr], symsize=symsize, color=!dorange, psym=sym(1)
     oplot, sma0[jr], sr0[jr], thick=thick+1, color=!dorange
     oploterror_old, sma0[jr], sr0[jr], 0.0*sma0[jr], esr0[jr], $
                 /nohat, errthick=thick+1, psym=3, errcolor=!dorange
  ENDIF 
  ;
  IF njn GT 0 THEN BEGIN 
     oplot, sma[jn], snuv[jn], thick=thick, color=!cyan
     oplot, sma0[jn], snuv0[jn], symsize=symsize, color=!cyan, psym=sym(2)
     oplot, sma0[jn], snuv0[jn], thick=thick+1, color=!cyan
     oploterror_old, sma0[jn], snuv0[jn], 0.0*sma0[jn], esnuv0[jn], $
                 /nohat, errthick=thick+1, psym=3, errcolor=!cyan
  ENDIF 
  ;
  IF njf GT 0 THEN BEGIN 
     oplot, sma[jf], sfuv[jf], thick=thick, color=!blue
     oplot, sma0[jf], sfuv0[jf], symsize=symsize, color=!blue, psym=sym(3)
     oplot, sma0[jf], sfuv0[jf], thick=thick, color=!blue
     oploterror_old, sma0[jf], sfuv0[jf], 0.0*sma0[jf], esfuv0[jf], $
                 /nohat, errthick=thick, psym=3, errcolor=!blue
  ENDIF 
  ;
  IF njh GT 0 THEN BEGIN 
     oplot, sma[jh], msha[jh], thick=thick, color=!dpink
     oplot, sma0[jh], msha0[jh], symsize=symsize, color=!dpink, psym=sym(14)
     oplot, sma0[jh], msha0[jh], thick=thick+1, color=!dpink
     oploterror_old, sma0[jh], msha0[jh], 0.0*sma0[jh], meshat0[jh], $
                 /nohat, errthick=thick+1, psym=3, errcolor=!dpink
  ENDIF 
  ;
  ; ------------------------------------------------------------------
  ; panel 2
  ; FUV-NUV, NUV-R colour profiles
  plog,ll,prog,'plotting color profiles'
  !p.noerase = 0
  !p.multi = [2,1,3]
  plot, sma[0:1], scnr[0:1], xrange=rrange, yrange=abcrange, xstyle=1, ystyle=1, $
        charsize=charsize, symsize=symsize, thick=thick, xthick=thick, ythick=thick, $
        xtitle=rtitle, ytitle=abctitle, charthick=thick, xmargin=[8,8], ymargin=[3,3], /nodata
  IF njrn GT 0 THEN BEGIN 
     oplot, sma[jrn], scnr[jrn], color=!orange, thick=thick
     oplot, sma0[jrn], scnr0[jrn], symsize=symsize, psym=sym(4), color=!orange
     oplot, sma0[jrn], scnr0[jrn], thick=thick+1, color=!orange
     oploterror_old, sma0[jrn], scnr0[jrn], 0.0*sma0[jrn], escnr0[jrn], $
                 /nohat, errthick=thick+1, psym=3, errcolor=!orange
  ENDIF 
  ;
  IF njfn GT 0 THEN BEGIN 
     oplot, sma[jfn], scfn[jfn], thick=thick, color=!green
     oplot, sma0[jfn], scfn0[jfn], symsize=symsize, psym=sym(5), color=!green
     oplot, sma0[jfn], scfn0[jfn], thick=thick+1, color=!green
     oploterror_old, sma0[jfn], scfn0[jfn], 0.0*sma0[jfn], escfn0[jfn], $
                 /nohat, errthick=thick+1, psym=3, errcolor=!green
  ENDIF
  ;
  ; ------------------------------------------------------------------
  ; panel 3
  ; Halpha/FUV Halpha/R profiles
  ;keywait,'type anything for next plot'
  plog,ll,prog,'plotting flux ratio profiles'
  !p.noerase = 0
  !p.multi = [1,1,3]
  plot, sma[0:1], slewf[0:1], xrange=rrange, yrange=hfrange, xstyle=1, ystyle=1, $
        charsize=charsize, symsize=symsize, thick=thick, xthick=thick, ythick=thick, $
        xtitle=rtitle, ytitle=hftitle, charthick=thick, xmargin=[8,8], ymargin=[3,3], /nodata
  IF njhf GT 0 THEN BEGIN 
     oplot, sma[jhf], slewf[jhf], color=!purple, thick=thick
     oplot, sma0[jhf], slewf0[jhf], color=!purple, psym=sym(4), symsize=symsize
     oplot, sma0[jhf], slewf0[jhf], color=!purple, thick=thick+1
     oploterror_old, sma0[jhf], slewf0[jhf], 0.0*sma0[jhf], eslewf0[jhf], $
                 /nohat, errthick=thick+1, errcolor=!purple, psym=3
  ENDIF 
  ;
  IF njhr GT 0 THEN BEGIN 
     oplot, sma[jhr], slewr[jhr], color=!red, thick=thick
     oplot, sma0[jhr], slewr0[jhr], color=!red, psym=sym(23), symsize=symsize
     oplot, sma0[jhr], slewr0[jhr], color=!red, thick=thick+1
     oploterror_old, sma0[jhr], slewr0[jhr], 0.0*sma0[jhr], eslewf0[jhr], $
                 /nohat, errthick=thick+1, errcolor=!red, psym=3
  ENDIF 
  ;
  ; ------------------------------------------------------------------
  ; finish plot
  !p.multi   = 0
  !p.noerase = 0
  IF ftype NE 'jpg' THEN BEGIN 
     psend, fplot, /noprint, /clobber
  ENDIF ELSE BEGIN 
     snap_jpg, fplot
  ENDELSE 
END 


;msha = a - 2.5*sha
;mlim_0 = a - 2.5*shalim_0
;a = mlim_0 + 2.5*shalim_0
