PRO ssoup_plotsprofs, ll, sname, fsprof, fsprof0, fplot
  ;
  ; plot surface brightness and surface colour profiles
  ;
  ;   ll       -> logical unit number for plot
  ;   sname    -> Source name
  ;   fsprof   -> file containing surface brightness profiles prior to
  ;               internal dust correction
  ;   fsprof0  -> file containing internal dust corrected surface 
  ;               brightness profiles
  ;   fplot    -> output plot file name
  ;
  ; G. Meurer (ICRAR/UWA) 6/2010
  ; G. Meurer (ICRAR/UWA) 5/2011: 
  ;    * improve doc. 
  ;    * plot upper and lower limits, and handle flags
  ;    * reads new file format
  ;
  snlimit   = 2.0
  mflag   =  99.999  ; magnitude flag value
  emflag  =   9.999  ; error mag flag
  lflag   = -99.999  ; log flux flux value
  elflag  =   9.999  ; error log flag
  cuflag  =   8.888  ; colour upper limit flag
  clflag  =   7.777  ; colour lower limit flag
  fmti      = '(f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f)'
  prog      = 'SSOUP_PLOTSPROFS: '
  plog,ll,prog,'--------------------- starting '+prog+'---------------------------------'
  ;
  ; some plot parameters
  aa        = angstsym()
  rtitle    = '!3 semi-major axis [arcsec]'
  hftitle   = '!3 log(F!dH!4a!3!n/f!dFUV!n,f!dR!N ['+aa+'])'
  abtitle   = '!4 l!3 [ABmag arcssec!u-2!n]'
  abctitle  = '!3 surface colour'
  charsize  = 2.5
  symsize   = 1.3
  thick     = 2
  ;
  ; read in profile files
  plog,ll,prog,'reading in (not dust corrected) surface brightness profile file: '+fsprof
  readcol, fsprof, sma, sr, esr, sha, eshat, eshas, eshac, snuv, esnuv, sfuv, esfuv, $
           scfn, escfn, scnr, escnr, slewr, eslewr, slewf, eslewf, format=fmti
  np        = n_elements(sma)
  plog,ll,prog,'reading in (dust corrected) surface brightness profile file: '+fsprof0
  readcol, fsprof0, sma0, sr0, esr0, sha0, eshat0, eshas0, eshac0, snuv0, esnuv0, sfuv0, esfuv0, $
           scfn0, escfn0, scnr0, escnr0, slewr0, eslewr0, slewf0, eslewf0, format=fmti
  np0       = n_elements(sma0)
  ;
  ; determine max radii and corresponding pointers
  plog,ll,prog,'determining maxima radii, and points to plot'
  ;
  ; indecis below are named as follows:
  ; 1 [j,k,l,m] indicates how to plot points
  ;   j: plottable (good or an upper or lower limit)
  ;   k: good
  ;   l: lower limit
  ;   m: upper limit
  ; 2 [r,h,f,n,fn,nr,hr,hf] indicates band or colour
  ; 3 append 0 for dust corrected
  ; 4 prepend n to indicate count of the indecis
  ;
  ssoup_psp_indecis, ll, 'R', sma, sr, esr, mflag, emflag, clflag, emflag, $
                     rmax_r, jmax_r, jr, kr, lr, mr, njr, nkr, nlr, nmr
  ssoup_psp_indecis, ll, 'R0', sma0, sr0, esr0, mflag, emflag, clflag, emflag, $
                     rmax_r0, jmax_r0, jr0, kr0, lr0, mr0, njr0, nkr0, nlr0, nmr0
  ;
  ssoup_psp_indecis, ll, 'HALPHA', sma, sha, eshat, lflag, elflag, clflag, elflag, $
                     rmax_h, jmax_h, jh, kh, lh, mh, njh, nkh, nlh, nmh
  ssoup_psp_indecis, ll, 'HALPHA0', sma0, sha0, eshat0, lflag, elflag, clflag, elflag, $
                     rmax_h0, jmax_h0, jh0, kh0, lh0, mh0, njh0, nkh0, nlh0, nmh0
  ;
  ssoup_psp_indecis, ll, 'NUV', sma, snuv, esnuv, mflag, emflag, clflag, emflag, $
                     rmax_n, jmax_n, jn, kn, ln, mn, njn, nkn, nln, nmn
  ssoup_psp_indecis, ll, 'NUV0', sma0, snuv0, esnuv0, mflag, emflag, clflag, emflag, $
                     rmax_n0, jmax_n0, jn0, kn0, ln0, mn0, njn0, nkn0, nln0, nmn0
  ;
  ssoup_psp_indecis, ll, 'FUV', sma, sfuv, esfuv, mflag, emflag, clflag, emflag, $
                     rmax_f, jmax_f, jf, kf, lf, mf, njf, nkf, nlf, nmf
  ssoup_psp_indecis, ll, 'FUV0', sma0, sfuv0, esfuv0, mflag, emflag, clflag, emflag, $
                     rmax_f0, jmax_f0, jf0, kf0, lf0, mf0, njf0, nkf0, nlf0, nmf0
  ;
  ssoup_psp_indecis, ll, '(FUV-NUV)', sma, scfn, escfn, mflag, emflag, clflag, cuflag, $
                     rmax_fn, jmax_fn, jfn, kfn, lfn, mfn, njfn, nkfn, nlfn, nmfn
  ssoup_psp_indecis, ll, '(FUV-NUV)0', sma0, scfn0, escfn0, mflag, emflag, clflag, cuflag, $
                     rmax_fn0, jmax_fn0, jfn0, kfn0, lfn0, mfn0, njfn0, nkfn0, nlfn0, nmfn0
  ;
  ssoup_psp_indecis, ll, '(NUV-R)', sma, scnr, escnr, mflag, emflag, clflag, cuflag, $
                     rmax_nr, jmax_nr, jnr, knr, lnr, mnr, njnr, nknr, nlnr, nmnr
  ssoup_psp_indecis, ll, '(NUV-R)0', sma0, scnr0, escnr0, mflag, emflag, clflag, cuflag, $
                     rmax_nr0, jmax_nr0, jnr0, knr0, lnr0, mnr0, njnr0, nknr0, nlnr0, nmnr0
  ;
  ssoup_psp_indecis, ll, 'log(HALPHA/R)', sma, slewr, eslewr, lflag, emflag, clflag, cuflag, $
                     rmax_hr, jmax_hr, jhr, khr, lhr, mhr, njhr, nkhr, nlhr, nmhr
  ssoup_psp_indecis, ll, 'log(HALPHA/R)0', sma0, slewr0, eslewr0, lflag, emflag, clflag, cuflag, $
                     rmax_hr0, jmax_hr0, jhr0, khr0, lhr0, mhr0, njhr0, nkhr0, nlhr0, nmhr0
  ;
  ssoup_psp_indecis, ll, 'log(HALPHA/FUV)', sma, slewf, eslewf, lflag, emflag, clflag, cuflag, $
                     rmax_hf, jmax_hf, jhf, khf, lhf, mhf, njhf, nkhf, nlhf, nmhf
  ssoup_psp_indecis, ll, 'log(HALPHA/FUV)0', sma0, slewf0, eslewf0, lflag, emflag, clflag, cuflag, $
                     rmax_hf0, jmax_hf0, jhf0, khf0, lhf0, mhf0, njhf0, nkhf0, nlhf0, nmhf0
  ;
  rmax      = 1.05*max([rmax_r,rmax_h,rmax_n,rmax_f,rmax_fn,rmax_nr,rmax_hr,rmax_hf,rmax_r0,rmax_h0,rmax_n0,rmax_f0,rmax_fn0,rmax_nr0,rmax_hr0,rmax_hf0])
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
  hfrange   = [-1.0,3.2]
  abrange   = [32.0,14.0]
  shrange   = -18.5 + [0.0,-0.4*(abrange[1]-abrange[0])]
  abcrange  = [-1.5,6.0]
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
     oplot, sma[jr], sr[jr], thick=thick, color=!dorange, linestyle=1
     if nkr gt 0 then oplot, sma[kr], sr[kr], thick=thick, color=!dorange, linestyle=0
     if njr0 gt 0 then oplot, sma0[jr0], sr0[jr0], thick=thick+1, color=!dorange, linestyle=1
     if nkr0 gt 0 then begin 
        oplot, sma0[kr0], sr0[kr0], symsize=symsize, color=!dorange, psym=gsym(1)
        oplot, sma0[kr0], sr0[kr0], thick=thick+1, color=!dorange
        oploterror_old, sma0[kr0], sr0[kr0], 0.0*sma0[kr0], esr0[kr0], $
                 /nohat, errthick=thick+1, psym=3, errcolor=!dorange, color=!dorange
     endif 
     if nlr0 gt 0 then oplot, sma0[lr0], sr0[lr0], color=!dorange, psym=gsym(19), symsize=symsize
     if nmr0 gt 0 then oplot, sma0[mr0], sr0[mr0], color=!dorange, psym=gsym(18), symsize=symsize
  ENDIF 
  ;
  IF njn GT 0 THEN BEGIN 
     oplot, sma[jn], snuv[jn], thick=thick, color=!cyan, linestyle=1
     if nkn gt 0 then oplot, sma[kn], snuv[kn], thick=thick, color=!cyan, linestyle=0
     if njn0 gt 0 then oplot, sma0[jn0], snuv0[jn0], thick=thick+1, color=!cyan, linestyle=1
     if nkn0 gt 0 then begin 
        oplot, sma0[kn0], snuv0[kn0], symsize=symsize, color=!cyan, psym=gsym(2)
        oplot, sma0[kn0], snuv0[kn0], thick=thick+1, color=!cyan
        oploterror_old, sma0[kn0], snuv0[kn0], 0.0*sma0[kn0], esnuv0[kn0], $
                 /nohat, errthick=thick+1, psym=3, errcolor=!cyan, color=!cyan
     endif 
     if nln0 gt 0 then oplot, sma0[ln0], snuv0[ln0], color=!cyan, psym=gsym(19), symsize=symsize
     if nmn0 gt 0 then oplot, sma0[mn0], snuv0[mn0], color=!cyan, psym=gsym(18), symsize=symsize
  ENDIF 
  ;
  IF njf GT 0 THEN BEGIN 
     oplot, sma[jf], sfuv[jf], thick=thick, color=!blue, linestyle=1
     if nkf gt 0 then oplot, sma[kf], sfuv[kf], thick=thick, color=!blue, linestyle=0
     if njf0 gt 0 then oplot, sma0[jf0], sfuv0[jf0], thick=thick+1, color=!blue, linestyle=1
     if nkf0 gt 0 then begin 
        oplot, sma0[kf0], sfuv0[kf0], symsize=symsize, color=!blue, psym=gsym(3)
        oplot, sma0[kf0], sfuv0[kf0], thick=thick+1, color=!blue
        oploterror_old, sma0[kf0], sfuv0[kf0], 0.0*sma0[kf0], esfuv0[kf0], $
                 /nohat, errthick=thick+1, psym=3, errcolor=!blue, color=!blue
     endif 
     if nlf0 gt 0 then oplot, sma0[lf0], sfuv0[lf0], color=!blue, psym=gsym(19), symsize=symsize
     if nmf0 gt 0 then oplot, sma0[mf0], sfuv0[mf0], color=!blue, psym=gsym(18), symsize=symsize
  ENDIF 
  ;
  IF njh GT 0 THEN BEGIN 
     oplot, sma[jh], msha[jh], thick=thick, color=!dpink, linestyle=1
     if nkh gt 0 then oplot, sma[kh], msha[kh], thick=thick, color=!dpink, linestyle=0
     if njh0 gt 0 then oplot, sma0[jh0], msha0[jh0], thick=thick+1, color=!dpink, linestyle=1
     if nkh0 gt 0 then begin 
        oplot, sma0[kh0], msha0[kh0], symsize=symsize, color=!dpink, psym=gsym(14)
        oplot, sma0[kh0], msha0[kh0], thick=thick+1, color=!dpink
        oploterror_old, sma0[kh0], msha0[kh0], 0.0*sma0[kh0], meshat0[kh0], $
                 /nohat, errthick=thick+1, psym=3, errcolor=!dpink, color=!dpink
     endif 
     if nlh0 gt 0 then oplot, sma0[lh0], msha0[lh0], color=!dpink, psym=gsym(19), symsize=symsize
     if nmh0 gt 0 then oplot, sma0[mh0], msha0[mh0], color=!dpink, psym=gsym(18), symsize=symsize
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
  IF njnr GT 0 THEN BEGIN 
     oplot, sma[jnr], scnr[jnr], thick=thick, color=!orange, linestyle=1
     if nknr gt 0 then oplot, sma[knr], scnr[knr], thick=thick, color=!orange, linestyle=0
     if njnr0 gt 0 then oplot, sma0[jnr0], scnr0[jnr0], thick=thick+1, color=!orange, linestyle=1
     if nknr0 gt 0 then begin 
        oplot, sma0[knr0], scnr0[knr0], symsize=symsize, color=!orange, psym=gsym(2)
        oplot, sma0[knr0], scnr0[knr0], thick=thick+1, color=!orange
        oploterror_old, sma0[knr0], scnr0[knr0], 0.0*sma0[knr0], escnr0[knr0], $
                 /nohat, errthick=thick+1, psym=3, errcolor=!orange, color=!orange
     endif 
     if nlnr0 gt 0 then oplot, sma0[lnr0], scnr0[lnr0], color=!orange, psym=gsym(19), symsize=symsize
     if nmnr0 gt 0 then oplot, sma0[mnr0], scnr0[mnr0], color=!orange, psym=gsym(18), symsize=symsize
  ENDIF 
  ;
  IF njfn GT 0 THEN BEGIN 
     oplot, sma[jfn], scfn[jfn], thick=thick, color=!green, linestyle=1
     if nkfn gt 0 then oplot, sma[kfn], scfn[kfn], thick=thick, color=!green, linestyle=0
     if njfn0 gt 0 then oplot, sma0[jfn0], scfn0[jfn0], thick=thick+1, color=!green, linestyle=1
     if nkfn0 gt 0 then begin 
        oplot, sma0[kfn0], scfn0[kfn0], symsize=symsize, color=!green, psym=gsym(2)
        oplot, sma0[kfn0], scfn0[kfn0], thick=thick+1, color=!green
        oploterror_old, sma0[kfn0], scfn0[kfn0], 0.0*sma0[kfn0], escfn0[kfn0], $
                 /nohat, errthick=thick+1, psym=3, errcolor=!green, color=!green
     endif 
     if nlfn0 gt 0 then oplot, sma0[lfn0], scfn0[lfn0], color=!green, psym=gsym(19), symsize=symsize
     if nmfn0 gt 0 then oplot, sma0[mfn0], scfn0[mfn0], color=!green, psym=gsym(18), symsize=symsize
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
     oplot, sma[jhf], slewf[jhf], thick=thick, color=!purple, linestyle=1
     if nkhf gt 0 then oplot, sma[khf], slewf[khf], thick=thick, color=!purple, linestyle=0
     if njhf0 gt 0 then oplot, sma0[jhf0], slewf0[jhf0], thick=thick+1, color=!purple, linestyle=1
     if nkhf0 gt 0 then begin 
        oplot, sma0[khf0], slewf0[khf0], symsize=symsize, color=!purple, psym=gsym(2)
        oplot, sma0[khf0], slewf0[khf0], thick=thick+1, color=!purple
        oploterror_old, sma0[khf0], slewf0[khf0], 0.0*sma0[khf0], eslewf0[khf0], $
                 /nohat, errthick=thick+1, psym=3, errcolor=!purple, color=!purple
     endif 
     if nlhf0 gt 0 then oplot, sma0[lhf0], slewf0[lhf0], color=!purple, psym=gsym(19), symsize=symsize
     if nmhf0 gt 0 then oplot, sma0[mhf0], slewf0[mhf0], color=!purple, psym=gsym(18), symsize=symsize
  ENDIF 
  ;
  IF njhr GT 0 THEN BEGIN 
     oplot, sma[jhr], slewr[jhr], thick=thick, color=!red, linestyle=1
     if nkhr gt 0 then oplot, sma[khr], slewr[khr], thick=thick, color=!red, linestyle=0
     if njhr0 gt 0 then oplot, sma0[jhr0], slewr0[jhr0], thick=thick+1, color=!red, linestyle=1
     if nkhr0 gt 0 then begin 
        oplot, sma0[khr0], slewr0[khr0], symsize=symsize, color=!red, psym=gsym(2)
        oplot, sma0[khr0], slewr0[khr0], thick=thick+1, color=!red
        oploterror_old, sma0[khr0], slewr0[khr0], 0.0*sma0[khr0], eslewr0[khr0], $
                 /nohat, errthick=thick+1, psym=3, errcolor=!red, color=!red
     endif 
     if nlhr0 gt 0 then oplot, sma0[lhr0], slewr0[lhr0], color=!red, psym=gsym(19), symsize=symsize
     if nmhr0 gt 0 then oplot, sma0[mhr0], slewr0[mhr0], color=!red, psym=gsym(18), symsize=symsize
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
