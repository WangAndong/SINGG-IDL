PRO ptest_matchdirct_nplot
   ;
   ; Match direct image catalog to rebinned grism image catalog.
   ; Goals:
   ; + get faintness cut from unmatched direct sources
   ; + explore parameters of grism objects that match and do not 
   ;   match direct image sources.  Parameters to consider, a_image,
   ;   b_image, a_image/b_image, theta_image, fwhm_image, class_star,
   ;   mag_auto 
   ; + better transformation from direct to rebinned grism image
   ; 
   ; G. Meurer 09/2005
   ;
   dcat     = 'A_cayenne_goods_epoch1.cat'
   fmtd     = '(f,f,l,x,x,f,f,f,f,x,x,x,x,f,f)'
   gcat     = 'grism_rebin01.cat'
   fmtg     = '(l,f,f,f,x,x,x,x,x,x,x,x,x,f,f,f,x,f,x,f)'
   xbin     = 25.0
   xoff1    = 65.0   ; approximate x offset of first order in unbinned pixels
   mrad     = 2.0
   dxx      = 1.36   ; uniform offset in grism pixels to apply to calculated
   dyy      = 1.21   ; first order positions
   dmaglim  = [35.5, 41.0]
   ;
   charsize = 1.5
   symsize  = 1.0
   thick    = 1.0
   ;
   ; read direct catalog
   readcol, dcat, xd, yd, idd, magd, aad, bbd, thd, w50d, classd, format=fmtd
   ;
   ; find compact direct image objects
   goodd  = where(classd GT 0.9 OR (classd GT 0.8 AND magd GT 20.0),ngd)
   kk     = sort(magd[goodd])
   xd     = xd[goodd[kk]]
   yd     = yd[goodd[kk]]
   idd    = idd[goodd[kk]]
   magd   = magd[goodd[kk]]
   aad    = aad[goodd[kk]]
   bbd    = bbd[goodd[kk]]
   w50d   = w50d[goodd[kk]]
   classd = classd[goodd[kk]]
   idd2   = lindgen(ngd)
   ;
   ; rough transformation of direct image coords to grism coords
   ydg    = yd + dyy
   xdg    = xoff1/xbin + (xd - 0.5*(xbin + 1.0))/xbin + dxx
   ;
   ; read catalog of rebinned grism image
   readcol, gcat, idg, xg, yg, magg, aag, bbg, thg, w50g, classg, format=fmtg
   axratg = aag / bbg
   ngg    = n_elements(idg)
   ; 
   ; using knowledge gained from previous runs
   ; mark likely first order grism targets, using 
   ; a few trial selection limits
   gg1    = where(axratg LE 2.3 AND bbg LE 2.0 AND w50g LE 8.0, ngg1)
   gg2    = where(axratg LE 2.0 AND bbg LE 2.0 AND w50g GT 2.25 AND w50g LE 5.2, ngg2)
   print, 'Number of first ord candidates, liberal      selection: ', ngg1
   print, 'Number of first ord candidates, conservative selection: ', ngg2
   ;
   ; match
   mrad2  = mrad*mrad
   ptrg   = make_array(ngd, /long, value=-1)
   FOR ii = 0, ngd-1 DO BEGIN 
      r2  = (xg - xdg[ii])^2 + (yg - ydg[ii])^2
      kk  = sort(r2)
      dmm = magd[ii] - magg[kk[0]]
      IF r2[kk[0]] LT mrad2 AND dmm GE min(dmaglim) AND dmm LE max(dmaglim) THEN ptrg[ii] = kk[0]
   ENDFOR 
   km     = where(ptrg GE 0, nmatch)
   print, 'number of matches : ', nmatch
   ;
   ; store quantities for plotting
   xdgm   = xdg[km]
   ydgm   = ydg[km]
   xgm    = xg[ptrg[km]]
   ygm    = yg[ptrg[km]]
   dxdg   = xdgm - xgm
   dydg   = ydgm - ygm
   radm   = sqrt(dxdg^2 + dydg^2)
   dmagm  = magd[km] - magg[ptrg[km]]
   maggm  = magg[ptrg[km]]
   magdm  = magd[km]
   clgm   = classg[ptrg[km]]
   aagm   = aag[ptrg[km]]
   bbgm   = bbg[ptrg[km]]
   axrgm  = aagm/bbgm
   w50gm  = w50g[ptrg[km]]
   w50dm  = w50g[km]
   bbdm   = bbd[km]
   ;
   ; first plot: dx vs. dy
   plot, dxdg, dydg, xrange=mrad*[-1.0, 1.0], yrange=mrad*[-1.0, 1.0], xstyle=1, ystyle=1, psym=sym(4), $
    xtitle='!3 dx', ytitle='!3 dy', charsize=charsize, symsize=symsize, $
    thick=thick, xthick=thick, ythick=thick, /isotropic
   grm_avsigclip, dxdg, 2.5, 50.0, mean_dxdg, sig_dxdg, nuse_dxdg, nrej, nit
   grm_avsigclip, dydg, 2.5, 50.0, mean_dydg, sig_dydg, nuse_dydg, nrej, nit
   mean_dxdg = dxx - mean_dxdg
   mean_dydg = dyy - mean_dydg
   print, 'X offset + original offset. mean: ', mean_dxdg, ' sigma: ',sig_dxdg, ' Nuse: ', nuse_dxdg
   print, 'Y offset + original offset. mean: ', mean_dydg, ' sigma: ',sig_dydg, ' Nuse: ', nuse_dydg
   ;
   ; now plot mag difference versus offset from best fit offset
   keywait, 'Type anything for next plot:'
   dmrange = [30.0, 50.0]
   yplt   = dmagm > dmrange[0] < dmrange[1]
   dmrange = 0.5*[-1.0, 1.0] + dmrange
   plot, radm, yplt, xrange=([-0.1,0.1]+mrad*[0.0,1.0]), yrange=dmrange, xstyle=1, ystyle=1, psym=sym(4), $
    xtitle='!3 Radius from expected position [pixels]', ytitle='!3 mag(direct) - mag(grism)', $
    charsize=charsize, symsize=symsize, thick=thick, xthick=thick, ythick=thick
   ;
   ; plot direct image mag histogram of objects that match and 
   ; those that don't
   ;
   ; -> result very few matches for mag(direct) > 27.0
   keywait, 'Type anything for next plot:'
   mrange = [16.75, 28.25]
   dmd    = 0.25
   xhist  = mrange[0] + dmd*(findgen((mrange[1]-mrange[0])/dmd) + 0.5)
   mhistd = histogram(magd, binsize=dmd, min=mrange[0], max=mrange[1])
   mhistm = histogram(magdm, binsize=dmd, min=mrange[0], max=mrange[1])
   yrange = [0.0, 1.1*max(mhistd)]
   plot, xhist, mhistd, xrange=mrange, yrange=yrange, xstyle=1, ystyle=1, psym=10, $
    xtitle='!3 mag(direct)', ytitle='!3 Number', charsize=charsize, $
    thick=thick, xthick=thick, ythick=thick
   oplot, xhist, mhistm, psym=10, thick=thick+2, color=!dgreen
   oplot, xhist, mhistd, psym=10, thick=thick
   ;
   ; now plot class versus mag in grism image
   ; for all objects in grism image, and those that were matched
   ;
   ; -> result class does not really distinguish first orders
   keywait, 'Type anything for next plot:'
   qq     = where(magg LT 90.0, nqq)
   mrange = [0.5, -0.5] + [max(magg[qq]), min(magg[qq])]
   crange = [-0.1, 1.1]
   setplotcolors
   plot, magg, classg, xrange=mrange, yrange=crange, xstyle=1, ystyle=1, psym=sym(4), $
    xtitle='!3 mag(grism)', ytitle='!3 class(grism)', $ 
    charsize=charsize, symsize=symsize, thick=thick, xthick=thick, ythick=thick
   oplot, maggm, clgm, symsize=1.4*symsize, psym=sym(1), color=!dgreen
   oplot, magg[gg1], classg[gg1], psym=sym(4), symsize=symsize, color=!blue
   oplot, magg[gg2], classg[gg2], psym=sym(4), symsize=symsize, color=!cyan
   ;
   ; now plot semi-major versus semi-minor size
   keywait, 'Type anything for next plot:'
   srange = [0.0, 4.0]
   plot, aag, bbg, xrange=srange, yrange=srange, xstyle=1, ystyle=1, psym=sym(4), $
    xtitle='!3 a!dimage!n', ytitle='!3 b!dimage!n', /isotropic, $
    charsize=charsize, symsize=symsize, thick=thick, xthick=thick, ythick=thick
   oplot, aagm, bbgm, symsize=1.4*symsize, psym=sym(1), color=!dgreen
   oplot, aag[gg1], bbg[gg1], psym=sym(4), symsize=symsize, color=!blue
   oplot, aag[gg2], bbg[gg2], psym=sym(4), symsize=symsize, color=!cyan
   ;
   ; next plot axial ratio versus sime-minor axis size
   keywait, 'Type anything for next plot:'
   axrange = [0.9, 10.0]
   srange  = [0.0, 5.0]
   plot, bbg, axratg, xrange=srange, yrange=axrange,  xstyle=1, ystyle=1, psym=sym(4), $
    xtitle='!3 b!dimage!n', ytitle='!3 a!dimage!n/b!dimage!n ', $
    charsize=charsize, symsize=symsize, thick=thick, xthick=thick, ythick=thick
   oplot, bbgm, axrgm, symsize=1.4*symsize, psym=sym(1), color=!dgreen
   oplot, bbdm, axrgm, symsize=1.4*symsize, psym=sym(2), color=!red
   oplot, bbg[gg1], axratg[gg1], psym=sym(4), symsize=symsize, color=!blue
   oplot, bbg[gg2], axratg[gg2], psym=sym(4), symsize=symsize, color=!cyan
   ;
   ; next plot axial ratio versus mag
   keywait, 'Type anything for next plot:'
   plot, magg, axratg, xrange=mrange, yrange=axrange, xstyle=1, ystyle=1, psym=sym(4), $
    xtitle='!3 mag(grism)', ytitle='!3 a!dimage!n/b!dimage!n ', $
    charsize=charsize, symsize=symsize, thick=thick, xthick=thick, ythick=thick
   oplot, maggm, axrgm, symsize=1.4*symsize, psym=sym(1), color=!dgreen
   oplot, magg[gg1], axratg[gg1], psym=sym(4), symsize=symsize, color=!blue
   oplot, magg[gg2], axratg[gg1], psym=sym(4), symsize=symsize, color=!cyan
   ;
   ; next plot axrat versus w50
   keywait, 'Type anything for next plot:'
   plot, w50g, axratg, xrange=3.0*srange, yrange=axrange, xstyle=1, ystyle=1, psym=sym(4), $
    xtitle='!3 FWHM(grism)', ytitle='!3 a!dimage!n/b!dimage!n ', $
    charsize=charsize, symsize=symsize, thick=thick, xthick=thick, ythick=thick
   oplot, w50gm, axrgm, symsize=1.4*symsize, psym=sym(1), color=!dgreen
   oplot, w50dm, axrgm, symsize=1.4*symsize, psym=sym(2), color=!red
   oplot, w50g[gg1], axratg[gg1], psym=sym(4), symsize=symsize, color=!blue
   oplot, w50g[gg2], axratg[gg2], psym=sym(4), symsize=symsize, color=!cyan
   ; oplot size in direct image versus ax ratio in grism image
   stop
END 
