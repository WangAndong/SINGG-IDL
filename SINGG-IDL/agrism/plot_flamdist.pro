PRO plot_flamdist
   COMMON sensmod, lams, sens, esens, hsens
   exptime = 6870.0
   fnorm = 1.0
   fmt  = '(i,f,f,f,f,f,f,f,i,f,f,a,f,f,f,a,f,f,f,f,f,f,f,f,a)'
   filsens    = '/home/meurer/acs/axe/conf/ACS.WFC.1st.sens.fits' 
   extsens    = 1
   setsensmod, filsens, extsens
   ;
   ; calculate detection limits at peak sensitivity
   snlims     = [1.0, 3.0, 5.0]
   smax       = max(sens)
   varpix     = 841.4
   aprad      = 2.5
   area       = !dpi*aprad*aprad
   errap      = sqrt(varpix*area)
   errapf     = errap / (smax*exptime)
   lsnlim    = alog10(errapf*snlims)
   print,'err cts  = ', errap
   print,'err flx  = ', errapf 
   print,'lsnlim  = ', lsnlim
   yr         = [0, 20]
   ;
   ; read catalogs
   readcol, 'emsource.cat', ida, xima, yima, maga, aa, ba, tha, fwhma, classa, raa, deca, minsna, maxsna, nord0a, lina, quala, lama, w50a, fluxa, $
    format='(i,f,f,f,f,f,f,f,f,f,f,f,f,i,i,i,f,f,f)'
   readcol,'hdfn_blem.out',idg,ximg,yimg,magb,ab,bb,thb,fwhmb,flagb,ctsb,skyb,qcutb,apeakb,ashiftb,aw50b,$
       qccb,cpeakb,cshiftb,cw50b,lamb,cprmsb,ximb,yimb,fluxb,csexid,$
       format=fmt
   ;
   ; good sources hdfn_blem
   qcutb = strtrim(qcutb,2)
   qccb  = strtrim(qccb,2)
   g = where(qcutb NE '3' AND qcutb NE '5' AND qcutb NE '6' AND qcutb NE '7' $
             AND qccb NE 'k' AND qccb NE '9' AND lamb GT 0.0, ng)
   idg = temporary(idg[g])
   ximg = temporary(ximg[g])
   yimg = temporary(yimg[g])
   magb = temporary(magb[g])
   ab = temporary(ab[g])
   bb = temporary(bb[g])
   thb = temporary(thb[g])
   fwhmb = temporary(fwhmb[g])
   flagb = temporary(flagb[g])
   ctsb = temporary(ctsb[g])
   skyb = temporary(skyb[g])
   qcutb = temporary(qcutb[g])
   apeakb = temporary(apeakb[g])
   ashiftb = temporary(ashiftb[g])
   aw50b = temporary(aw50b[g])
   qccb = temporary(qccb[g])
   cpeakb = temporary(cpeakb[g])
   cshiftb = temporary(cshiftb[g])
   cw50b = temporary(cw50b[g])
   lamb = temporary(lamb[g])
   cprmsb = temporary(cprmsb[g])
   ximb = temporary(ximb[g])
   yimb = temporary(yimb[g])
   fluxb = temporary(fluxb[g])
   csexid = temporary(csexid[g])
   setplotcolors
   ;
   ; recalculate flux of grism selected sources
   slam = interpol(sens,lams,lamb)*exptime*fnorm
   fluxb = ctsb / slam
   ;
   lfluxa = alog10(fluxa)
   lfluxb = alog10(fluxb)
   lmin   = -18.0
   lmax   = -15.0
   dl     = 0.25
   nl     = (lmax - lmin)/dl
   xl     = lmin + dl*(0.5 + findgen(nl))
   nflxa  = histogram(lfluxa, min=lmin, max=lmax, binsize=dl)
   nflxb  = histogram(lfluxb, min=lmin, max=lmax, binsize=dl)
   ;
   FOR i = 0, nl-1 DO print,i,xl[i],nflxa[i],nflxb[i]
   !P.MULTI = [0,1,2]
   window,0,xsize=400,ysize=400
   ;
   plot,xl,nflxa,xrange=[-17.5,-15],yrange=yr,xstyle=1,ystyle=1,psym=10,$
    xtitle='log(Flux [erg/cm^2/s])', ytitle='N', $
    title='Direct image selection'
   FOR i = 0, n_elements(lsnlim)-1 DO BEGIN 
      xpl = lsnlim[i] + [0.0, 0.0]
      ypl = yr
      oplot, xpl, ypl, linestyle=2, color=!pink
   ENDFOR 
   FOR i = 0, nl-1 DO BEGIN 
      xbx = xl[i] + dl*[-0.5, -0.5, 0.5, 0.5]
      ybx = nflxa[i]*[0.0, 1.0, 1.0, 0.0]
      polyfill, xbx, ybx, color=!lgreen
      oplot, xbx, ybx
   ENDFOR 
   ;
   plot,xl,nflxb,xrange=[-17.5,-15],yrange=yr,xstyle=1,ystyle=1,psym=10,$
    xtitle='log(Flux [erg/cm^2/s])', ytitle='N', $
    title='Grism image selection'
   FOR i = 0, n_elements(lsnlim)-1 DO BEGIN 
      xpl = lsnlim[i] + [0.0, 0.0]
      ypl = yr
      oplot, xpl, ypl, linestyle=2, color=!pink
   ENDFOR 
   FOR i = 0, nl-1 DO BEGIN 
      xbx = xl[i] + dl*[-0.5, -0.5, 0.5, 0.5]
      ybx = nflxb[i]*[0.0, 1.0, 1.0, 0.0]
      polyfill, xbx, ybx, color=!lgreen
      oplot, xbx, ybx
   ENDFOR 
   makepng,'flamhist.png',/color
END 

