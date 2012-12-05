PRO pfplt_3pan, filenet, filecnt, galindex, title, rad50, ew50, err_ew50, $
                hardfile=hardfile
   ;
   ; Make a 3 panel plot of the radial profiles of quantities.
   ; In addition this returns: (1) the half light radius in
   ; net line emission, (2) the line EW within this half light 
   ; radius, and (3) the error in this EW.
   ;
   ; G. Meurer 10/2004
   pixsize  = 1.0
   ferrclip = 0.0
   nsig     = 2.0
   netflag  = 0b
   lev      = 0.5
   ;
   xtitle     = '!3 Semi-major axis distance [arcsec]'
   ytitle1    = '!3 Log(surface brightness [cgs])'
   ytitle2    = '!3 Curve of Growth'
   ytitle3    = '!3 Equivalent Width ['+angstsym()+']'
   charsize   =  2.3
   ;
   ; read the headers of the net and continuum profiles
   pixsizenet = pixsize
   pixsizecnt = pixsize
   pfplt_rdhdr, filenet, pixsizenet, filenamenet, funitsnet, fscalenet, fluxcalnet, $
    ptypenet, numgalsnet, gindexnet, xcenternet, ycenternet, axeratnet, posangnet, $
    posangwcnet, skylevnet, skysigpxnet, skysigbxnet, fluxradnet, fluxnet, flsigskynet, $
    flsigcntnet, hlradiusnet, hlsigskynet, hlcrcfluxnet, lstartnet, lendnet, isoflagnet, netflag
   ;
   pfplt_rdhdr, filecnt, pixsizecnt, filenamecnt, funitscnt, fscalecnt, fluxcalcnt, $
    ptypecnt, numgalscnt, gindexcnt, xcentercnt, ycentercnt, axeratcnt, posangcnt, $
    posangwccnt, skylevcnt, skysigpxcnt, skysigbxcnt, fluxradcnt, fluxcnt, flsigskycnt, $
    flsigcntcnt, hlradiuscnt, hlsigskycnt, hlcrcfluxcnt, lstartcnt, lendcnt, isoflagcnt, cntflag
   ;
   ; find galindex to extract
   gn = where(gindexnet EQ galindex, ngn)
   gc = where(gindexcnt EQ galindex, ngc)
   IF ngc NE 1 OR ngn NE 1 THEN stop, 'Can not find unique profile ', ngn, ngc
   ;
   ; extract the profiles
   lstartn  = lstartnet[gn[0]]
   lendn    = lendnet[gn[0]]
   lstartc  = lstartcnt[gc[0]]
   lendc    = lendcnt[gc[0]]
   ;
   pfplt_extractprof, filenet, netflag, smanet, fintnet, dfintnet, ngoodnet, nbadnet, $
    sbnet, esbnet, dfrawnet, efintsknet, efintcnnet, pixsize=pixsizenet, $
    fscale=fscalenet, ferrclip=ferrclip, lstart=lstartn, lend=lendn
   pfplt_extractprof, filecnt, cntflag, smacnt, fintcnt, dfintcnt, ngoodcnt, nbadcnt, $
    sbcnt, esbcnt, dfrawcnt, efintskcnt, efintcncnt, pixsize=pixsizecnt, $
    fscale=fscalecnt, ferrclip=ferrclip, lstart=lstartc, lend=lendc
   ;
   ; clip profiles
   kk       = where(sbnet GE nsig*skysigbxnet[gn[0]] OR sbcnt GE nsig*skysigbxcnt[gn[0]], nkk)
   IF nkk GE 1 THEN BEGIN 
      ngoodnet     = ngoodnet[kk]
      nbadnet      = nbadnet[kk]
      smanet       = smanet[kk]
      fintnet      = fintnet[kk]
      dfintnet     = dfintnet[kk]
      sbnet        = sbnet[kk]
      esbnet       = esbnet[kk]
      dfrawnet     = dfrawnet[kk]
      efintsknet   = efintsknet[kk]
      efintcnnet   = efintcnnet[kk]
      ngoodcnt     = ngoodcnt[kk]
      nbadcnt      = nbadcnt[kk]
      smacnt       = smacnt[kk]
      fintcnt      = fintcnt[kk]
      dfintcnt     = dfintcnt[kk]
      sbcnt        = sbcnt[kk]
      esbcnt       = esbcnt[kk]
      dfrawcnt     = dfrawcnt[kk]
      efintskcnt   = efintskcnt[kk]
      efintcncnt   = efintcncnt[kk]
   ENDIF 
   print, skysigbxnet[gn[0]], skysigbxcnt[gc[0]]
   ferrsbnet = skysigbxnet[gn[0]] / sbnet
   ferrsbcnt = skysigbxcnt[gc[0]] / sbcnt
   ;
   ; calculate surface brightnesses
   logsbnet   = alog10(sbnet)
   logsbnet1  = alog10(sbnet-skysigbxnet[gn[0]])
   logsbnet2  = alog10(sbnet+skysigbxnet[gn[0]])
   logsbcnt   = alog10(sbcnt)
   logsbcnt1  = alog10(sbcnt-skysigbxcnt[gc[0]])
   logsbcnt2  = alog10(sbcnt+skysigbxcnt[gc[0]])
   ;forprint, smanet, logsbnet, logsbnet1, logsbnet2, logsbcnt, logsbcnt1, logsbcnt2
   ;
   ; calculate COGs
   cognet     = fintnet / fluxnet[gn[0]]
   cogcnt     = fintcnt / fluxcnt[gc[0]]
   print, fscalenet, fscalecnt
   print, fluxnet, fluxcnt
   ;
   ; calculate EW 
   efintnet   = sqrt(efintsknet^2 + efintcnnet^2)
   ew         = fintnet / fintcnt
   err_ew     = ew * sqrt(efintsknet^2/fintnet^2 + efintcncnt^2/fintcnt^2)
   ew1        = ew - err_ew
   ew2        = ew + err_ew
   ;
   ; calculate and print EW at rdaius
   ; enclosing ~50% of Halpha flux
   kk         = sort(abs(cognet - lev))
   rad50      = smanet[kk[0]]
   ew50       = ew[kk[0]]
   err_ew50   = err_ew[kk[0]]
   print, ' object : '+strtrim(title)+' at SMA50 = '+strtrim(string(rad50,format='(f7.2)'),2)+$
    ' arcsec    EW = '+strtrim(string(ew50,format='(f7.1)'),2)+' +/- '+strtrim(string(err_ew50,format='(f7.1)'),2)+$
    ' Angstroms '
   ;
   ; prepare for plot
   xrange     = [0, 1.05*max([smanet, smacnt])]
   dum        = [logsbnet1, logsbcnt1]
   kk         = where(finite(dum) EQ 1, nkk)
   IF nkk GT 0 THEN dum = dum[kk]
   yrange1    = [min(dum)-0.5, max([logsbnet2, logsbcnt2])+0.5]
   yrange2    = [0, 1.2]
   yrange3    = [0, 1.1*max(ew2)]
   kk         = where(finite(logsbnet1) NE 1, nkk)
   IF nkk GT 1 THEN logsbnet1[kk] = yrange1[0]
   kk         = where(finite(logsbcnt1) NE 1, nkk)
   IF nkk GT 1 THEN logsbcnt1[kk] = yrange1[0]
   !p.multi = [0, 1, 3]                  ; Three panel plot
   IF keyword_set(hardfile) THEN BEGIN 
      xs = 6.5
      ys = 1.25*xs
      yoff=3.
      xoff=1.2
      set_plot,'ps',/copy, /interpolate
      device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color
      ;psp, /eps, xsize=xs, ysize=ys
   ENDIF  ELSE BEGIN 
      wxsize   = 650
      wysize   = fix(1.25*float(wxsize))
      window, 0, xsize=wxsize, ysize=wysize
   ENDELSE 
   setplotcolors
   setbgfg,!white,!black
   erase
   ;
   ; plot surface brightnesses
   plot, smacnt, logsbcnt, xrange=xrange, yrange=yrange1, xstyle=1, ystyle=1, psym=sym(5), $
    xtitle=xtitle, ytitle=ytitle1, charsize=charsize, title=title
   ;errplot, smacnt, logsbcnt1, logsbcnt2, width=0, color=!blue
   ;errplot, smanet, logsbnet1, logsbnet2, width=0, color=!red
   errplot, smacnt, logsbcnt1, logsbcnt2, width=0
   errplot, smanet, logsbnet1, logsbnet2, width=0
   oplot, smacnt, logsbcnt, psym=sym(5), color=!blue
   oplot, smanet, logsbnet, psym=sym(1), color=!red
   ;
   ; plot COG
   plot, smacnt, cogcnt, xrange=xrange, yrange=yrange2, xstyle=1, ystyle=1, psym=1, $ 
    xtitle=xtitle, ytitle=ytitle2, charsize=charsize
   oplot, [0.0, smacnt], [0.0, cogcnt], psym=0, color=!blue
   oplot, [0.0, smanet], [0.0, cognet], psym=0, color=!red
   oplot, smacnt, cogcnt, psym=sym(5), color=!blue
   oplot, smanet, cognet, psym=sym(1), color=!red
   ;
   ; Plot EW
   plot, smanet, ew, xrange=xrange, yrange=yrange3, xstyle=1, ystyle=1, psym=1, $
    xtitle=xtitle, ytitle=ytitle3, charsize=charsize
   ;errplot, smanet, ew1, ew2, width=0, color=!red
   errplot, smanet, ew1, ew2, width=0
   oplot, smanet, ew, psym=sym(1),  color=!red
   ;
   ; finish
   !p.multi   = 0
   !p.noerase = 0
   IF keyword_set(hardfile) THEN BEGIN 
      psend, hardfile, /noprint, /clobber
   ENDIF 
END 
