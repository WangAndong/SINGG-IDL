PRO test_pfplt
   file     = 'J0156-68_Rsub_ss_isophote.profile'
   ;filenet  = 'J0454-53_Rsub_ss_isophote.profile'
   ;filecnt  = 'J0454-53_R_ss_isophote.profile'
   ;filenet  = 'J0507-37_Rsub_ss_isophote.profile'
   ;filecnt  = 'J0507-37_R_ss_isophote.profile'
   ;filenet  = 'J1337-29_Rsub_ss_brightness.profile'
   ;filecnt  = 'J1337-29_R_ss_brightness.profile'
   ;filenet  = 'J1339-31_Rsub_ss_brightness.profile'
   ;filecnt  = 'J1339-31_R_ss_brightness.profile'
   filenet  = 'J0156-68_Rsub_ss_isophote.profile'
   filecnt  = 'J0156-68_R_ss_isophote.profile'
   pixsize  = 1.0
   ferrclip = 0.0
   nsig     = 2.0
   netflag  = 0b
   ;
   xtitle     = '!3 Semi-major axis distance [arcsec]'
   ytitle1    = '!3 Log(surface brightness [cgs])'
   ytitle2    = '!3 Curve of Growth'
   ytitle3    = '!3 Equivalent Width ['+angstsym()+']'
   charsize   =  2.3
   ;
   pfplt_rdhdr, filenet, pixsize, fscalenet, funitsnet, filenamenet, xcenternet, ycenternet, $
    axeratnet, posangnet, posangwcnet, skylevnet, skysigpxnet, skysigbxnet, fluxradnet, $
    fluxnet, flsigskynet, flsigcntnet, hlradiusnet, hlsigskynet, $
    hlcrcfluxnet, isoflagnet, netflag
   ;
   pfplt_extractprof, filenet, netflag, smanet, fintnet, dfintnet, ngoodnet, nbadnet, $
    sbnet, esbnet, dfrawnet, efintsknet, efintcnnet, $
    pixsize=pixsize, fscale=fscalenet, ferrclip=ferrclip
   ;
   pfplt_rdhdr, filecnt, pixsize, fscalecnt, funitscnt, filenamecnt, xcentercnt, ycentercnt, $
    axeratcnt, posangcnt, posangwccnt, skylevcnt, skysigpxcnt, skysigbxcnt, fluxradcnt, $
    fluxcnt, flsigskycnt, flsigcntcnt, hlradiuscnt, hlsigskycnt, $
    hlcrcfluxcnt, isoflagcnt, netflag
   ;
   pfplt_extractprof, filecnt, netflag, smacnt, fintcnt, dfintcnt, ngoodcnt, nbadcnt, $
    sbcnt, esbcnt, dfrawcnt, efintskcnt, efintcncnt, $
    pixsize=pixsize, fscale=fscalecnt, ferrclip=ferrclip
   kk       = where(sbnet GE nsig*skysigbxnet OR sbcnt GE nsig*skysigbxcnt, nkk)
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
   print, skysigbxnet, skysigbxcnt
   ferrsbnet = skysigbxnet / sbnet
   ferrsbcnt = skysigbxcnt / sbcnt
   ;
   ; calculate surface brightnesses
   logsbnet   = alog10(sbnet)
   logsbnet1  = alog10(sbnet-skysigbxnet)
   logsbnet2  = alog10(sbnet+skysigbxnet)
   logsbcnt   = alog10(sbcnt)
   logsbcnt1  = alog10(sbcnt-skysigbxcnt)
   logsbcnt2  = alog10(sbcnt+skysigbxcnt)
   ;forprint, smanet, logsbnet, logsbnet1, logsbnet2, logsbcnt, logsbcnt1, logsbcnt2
   ;
   ; calculate COGs
   cognet     = fintnet / fluxnet
   cogcnt     = fintcnt / fluxcnt
   print, fscalenet, fscalecnt
   print, fluxnet, fluxcnt
   ; forprint, cognet, cogcnt
   ;
   ; calculate EW 
   efintnet   = sqrt(efintsknet^2 + efintcnnet^2)
   ew         = fintnet / fintcnt
   err_ew     = ew * sqrt(efintsknet^2/fintnet^2 + efintcncnt^2/fintcnt^2)
   ew1        = ew - err_ew
   ew2        = ew + err_ew
   ;
   ; prepare for plot
   xrange     = [0, 1.05*max([smanet, smacnt])]
   yrange1    = [min([logsbnet1, logsbcnt1])-0.5, max([logsbnet2, logsbcnt2])+0.5]
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
      device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color,/encapsulated
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
    xtitle=xtitle, ytitle=ytitle1, charsize=charsize
   errplot, smacnt, logsbcnt1, logsbcnt2, width=0, color=!blue
   errplot, smanet, logsbnet1, logsbnet2, width=0, color=!red
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
   errplot, smanet, ew1, ew2, width=0, color=!red
   oplot, smanet, ew, psym=sym(1),  color=!red
   ;
   ; finish
   !p.multi   = 0
   !p.noerase = 0
   IF keyword_set(hardfile) THEN BEGIN 
      psend, hardfile, /noprint, /clobber
   ENDIF 
END 
