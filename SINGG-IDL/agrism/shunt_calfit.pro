PRO shunt_calfit, matchcat, dmagrange, crange;, parin, parout
   ;
   ; fit the distortion and magnitude model to matched 
   ; objects from squashed image and direct image
   ;
   ; matchcat  -> matched object catalog
   ; dmagrange -> only acceppt objects with mag difference in this range
   ; crange    -> only accept colors in this range
   ; parin     -> first guess input parameters
   ; parout    <- resultant output parameters
   ;
   ; parameters are of the pp = parin or parout
   ;   px = pp[0:3]
   ;   py = pp[4:7]
   ;   pm = pp[8:9]
   ;
   ; taking xg,yg,mg = x,y, mag in squashed grism image
   ;        xd,yd,md,cd = x,y,mag & color in direct image
   ;       
   ;
   ; xd = px[0] + px[1]*xg + px[2]*yg + px[3]*cd
   ; yd = py[0] + py[1]*xg + py[2]*yg + py[3]*cd
   ; md = pm[0] + mg + pm[1]*cd
   ;
   ; G. Meurer 2/2006
   ;
   xbin       = 25.0
   xoffset    = -70.0
   parin      = double([xoffset + 0.5*(xbin + 1.0), xbin, 0.0, 0.0, $
                        0.0, 0.0, 1.0, 0.0, 0.0, 0.0])
   ;
   maxrep     = 10
   nsigma     = 3.0
   ;
   mmin       = min(dmagrange)
   mmax       = max(dmagrange)
   cmin       = min(crange)
   cmax       = max(crange)
   mstep      = 0.25
   fmtm       = '(i,f,f,f,f,a, l,f,f,f,f,f,f,f,f)'
   ;
   readcol, matchcat, idsh, xsq, ysq, magsq, sn, sclass, $
                        iddr, xim, yim, aim, bim, thim, $
                        m775, color1, color2, format=fmtm
   mdiff      = m775 - magsq
   nm         = fix((mmax - mmin)/mstep + 1)
   mval       = mmin + mstep*(findgen(nm) + 0.5)
   mhist      = histogram(mdiff, binsize=mstep, min=mmin, max=mmax)
   yrange     = [0.0, 1.2*max(mhist)]
   xtitle     = '!3 Mag difference: GOODS - grism(instrumental)'
   ytitle     = '!3 Number'
   charsize   = 1.5
   wysize     = 400
   wxsize     = 500
   thick      = 1
   window, 0, xsize=wxsize, ysize=wysize
   plot, mval, mhist, xrange=dmagrange, yrange=yrange, xstyle=1, ystyle=1, psym=10, $
    xtitle=xtitle, ytitle=ytitle, charsize=charsize
   ;
   ; determine robust mean and rms
   m0         = biweight_mean(mdiff, sigm0)
   print, 'Robust mean mag diff & sigma: ', m0, sigm0
   parin[8]   = m0
   ;
   ; pick the color to use
   color      = color1
   ;
   ; keep only the pairs having mdiff within dmagrange
   good       = where(mdiff GE mmin AND mdiff LE mmax AND color GE cmin AND color LE cmax, ngood)
   print, ngood
   idsh       = idsh[good]
   xsq        = xsq[good]
   ysq        = ysq[good]
   magsq      = magsq[good]
   sn         = sn[good]
   sclass     = sclass[good]
   iddr       = iddr[good]
   xim        = xim[good]
   yim        = yim[good]
   aim        = aim[good]
   bim        = bim[good]
   thim       = thim[good]
   m775       = m775[good]
   color1     = color1[good]
   color2     = color2[good]
   ;
   ; pick the color to use
   color      = color1
   ;
   ; mark all points to fit going into loop
   use1       = make_array(ngood, /byte, value=1b)
   ;
   ; iterative fitting loop
   nrep       = 0
   REPEAT BEGIN 
      nrep       = nrep + 1
      use0       = use1
      ;
      ; parse input parameters
      pxi        = parin[0:3]
      pyi        = parin[4:7]
      pmi        = parin[8:9]
      ;
      ; generate independent variables and errors
      gg         = where(use0 EQ 1b, ngg)
      xx         = [xsq[gg], ysq[gg], magsq[gg], color[gg]]
      xxall      = [xsq, ysq, magsq, color]
      xyerr      = 0.1*sqrt(aim[gg]*bim[gg])
      mgerr      = 0.1 + 0.0*m775[gg]
      ;
      ; fit models
      pxo        = mpfitfun('shunt_distortmod', xx, xim[gg], xyerr[gg], pxi)
      pyo        = mpfitfun('shunt_distortmod', xx, yim[gg], xyerr[gg], pyi)
      pmo        = mpfitfun('shunt_magmod', xx, m775[gg], mgerr[gg], pmi)
      ;
      ; fit models without color term
      pxinfo          = replicate({fixed:0, limited:[0,0], limits:[0.D,0.D]},4)
      pxinfo(3).fixed = 1
      pyinfo          = pxinfo
      pxif            = pxi
      pxif[3]         = 0.0d0
      pyif            = pyi
      pyif[3]         = 0.0d0
      pxof            = mpfitfun('shunt_distortmod', xx, xim[gg], xyerr[gg], pxif, parinfo=pxinfo)
      pyof            = mpfitfun('shunt_distortmod', xx, yim[gg], xyerr[gg], pyif, parinfo=pyinfo)
      ;
      ; print results
      print, 'X model results - input,output,no-color parameters'
      forprint, pxi, pxo, pxof
      print, 'Y model results - input,output,no-color parameters'
      forprint, pyi, pyo, pyof
      print, 'mag model results - input,output parameters'
      forprint, pmi, pmo
      ;
      parout     = [pxo, pyo, pmo]
      ;
      ; get residuals
      xresid     = xim - shunt_distortmod(xxall, pxo)
      yresid     = yim - shunt_distortmod(xxall, pyo)
      mresid     = m775 - shunt_magmod(xxall, pmi)
      xresidf    = xim - shunt_distortmod(xxall, pxof)
      yresidf    = yim - shunt_distortmod(xxall, pyof)
      ;
      ; determine biweight mean residuals and sigma of just the fitted points
      mdx        = biweight_mean(xresid[gg], sigdx)
      mdy        = biweight_mean(yresid[gg], sigdy)
      mdm        = biweight_mean(mresid[gg], sigdm)
      mdxf       = biweight_mean(xresidf[gg], sigdxf)
      mdyf       = biweight_mean(yresidf[gg], sigdyf)
      ;
      print, 'Biweight X residual mean, sig, nocolor-mean, nocolor-sig : ', mdx, sigdx, mdxf, sigdxf
      print, 'Biweight Y residual mean, sig, nocolor-mean, nocolor-sig : ', mdy, sigdy, mdyf, sigdyf
      print, 'Biweight mag residual mean, sig                          : ', mdm, sigdm
      ;
      ; mark datapoints outside of +/- nsigma*(sigdx,sigdy,or sigdm) to
      ; not be used in next iteration
      use1       = make_array(ngood, /byte, value=1b)
      pp         = where(abs(xresid - mdx) GT nsigma*sigdx, npx)
      IF npx GT 0 THEN use1[pp] = 0b
      pp         = where(abs(yresid - mdy) GT nsigma*sigdy, npy)
      IF npy GT 0 THEN use1[pp] = 0b
      pp         = where(abs(mresid - mdm) GT nsigma*sigdm, npm)
      IF npm GT 0 THEN use1[pp] = 0b
      pp         = where(use1 EQ 0b, npp)
      print, 'number of rejected points by X resid, Y resid, Mag resid, total : ', npx, npy, npm, npp
      pp         = where(use1 NE use0, npp)
      print, 'number of changes in rejected points : ', npp
      ;
      ; Calculate plotting ranges
      xyrange     = [0.0, 1.05*max([xim,yim])]
      dxrange     = [-1.0, 1.0]*1.1*max(abs([xresid]))
      dyrange     = [-1.0, 1.0]*1.1*max(abs([yresid]))
      dmrange     = [-1.0, 1.0]*1.1*max(abs(mresid))
      ;
      ; plot when ready
      keywait, 'Type any key for X residuals: '
      ytitle      = '!3 X residual'
      wysize      = 900
      wxsize      = fix(float(wysize/2.25))
      thick       = 1
      window, 0, xsize=wxsize, ysize=wysize
      setplotcolors
      setbgfg,!white,!black
      erase   
      ;
      !p.noerase = 0
      !p.multi   = [3, 1, 3]                  ; top panel
      plot, xim, xresid, xrange=xyrange, yrange=dxrange, xstyle=1, ystyle=1, psym=sym(6), $
            xtitle='!3 X (direct)', ytitle=ytitle, charsize=1.5
      oplot, xim, xresidf, psym=sym(6), color=!red
      oplot, xim[gg], xresidf[gg], psym=sym(1), color=!red
      oplot, xim[gg], xresid[gg], psym=sym(1)
      !p.multi   = [2, 1, 3]
      plot, yim, xresid, xrange=xyrange, yrange=dxrange, xstyle=1, ystyle=1, psym=sym(6), $
            xtitle='!3 Y (direct)', ytitle=ytitle, charsize=1.5
      oplot, yim, xresidf, psym=sym(6), color=!red
      oplot, yim[gg], xresidf[gg], psym=sym(1), color=!red
      oplot, yim[gg], xresid[gg], psym=sym(1)
      !p.multi   = [1, 1, 3]
      plot, color, xresid, xrange=crange, yrange=dxrange, xstyle=1, ystyle=1, psym=sym(6), $
            xtitle='!3 Color (direct)', ytitle=ytitle, charsize=1.5
      oplot, color, xresidf, psym=sym(6), color=!red
      oplot, color[gg], xresidf[gg], psym=sym(1), color=!red
      oplot, color[gg], xresid[gg], psym=sym(1)
      ;
      ; plot when ready
      keywait, 'Type any key for Y residuals: '
      ytitle      = '!3 Y residual'
      erase   
      ;
      !p.multi   = [3, 1, 3]                  ; top panel
      plot, xim, yresid, xrange=xyrange, yrange=dyrange, xstyle=1, ystyle=1, psym=sym(6), $
            xtitle='!3 X (direct)', ytitle=ytitle, charsize=1.5
      oplot, xim, yresidf, psym=sym(6), color=!red
      oplot, xim[gg], yresidf[gg], psym=sym(1), color=!red
      oplot, xim[gg], yresid[gg], psym=sym(1)
      !p.multi   = [2, 1, 3]
      plot, yim, yresid, xrange=xyrange, yrange=dyrange, xstyle=1, ystyle=1, psym=sym(6), $
            xtitle='!3 Y (direct)', ytitle=ytitle, charsize=1.5
      oplot, yim, yresidf, psym=sym(6), color=!red
      oplot, yim[gg], yresidf[gg], psym=sym(1), color=!red
      oplot, yim[gg], yresid[gg], psym=sym(1)
      !p.multi   = [1, 1, 3]
      plot, color, yresid, xrange=crange, yrange=dyrange, xstyle=1, ystyle=1, psym=sym(6), $
            xtitle='!3 Color (direct)', ytitle=ytitle, charsize=1.5
      oplot, color, yresidf, psym=sym(6), color=!red
      oplot, color[gg], yresidf[gg], psym=sym(1), color=!red
      oplot, color[gg], yresid[gg], psym=sym(1)
      ;
      ; plot when ready
      keywait, 'Type any key for mag residuals: '
      ytitle      = '!3 mag residual'
      erase   
      ;
      !p.multi   = [3, 1, 3]                  ; top panel
      plot, xim, mresid, xrange=xyrange, yrange=dmrange, xstyle=1, ystyle=1, psym=sym(6), $
            xtitle='!3 X (direct)', ytitle=ytitle, charsize=1.5
      oplot, xim[gg], mresid[gg], psym=sym(1)
      !p.multi   = [2, 1, 3]
      plot, yim, mresid, xrange=xyrange, yrange=dmrange, xstyle=1, ystyle=1, psym=sym(6), $
            xtitle='!3 Y (direct)', ytitle=ytitle, charsize=1.5
      oplot, yim[gg], mresid[gg], psym=sym(1)
      !p.multi   = [1, 1, 3]
      plot, color, mresid, xrange=crange, yrange=dmrange, xstyle=1, ystyle=1, psym=sym(6), $
            xtitle='!3 Color (direct)', ytitle=ytitle, charsize=1.5
      oplot, color[gg], mresid[gg], psym=sym(1)
      ;
   ENDREP UNTIL nrep EQ maxrep OR npp EQ 0
   ;
   ; finish
   !p.multi   = 0
   !p.noerase = 0
END 
