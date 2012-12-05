PRO plot_lamdist
   COMMON sensmod, lams, sens, esens, hsens
   fmt  = '(i,f,f,f,f,f,f,f,i,f,f,a,f,f,f,a,f,f,f,f,f,f,f,f,a)'
   filsens    = '/home/meurer/acs/axe/conf/ACS.WFC.1st.sens.fits' 
   extsens    = 1
   setsensmod, filsens, extsens
   lams       = [lams[0:5498], 9999.0]
   sens       = [sens[0:5498], 0.0]
   ; stop
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
             AND qccb NE 'k' AND qccb NE '9' AND lamb Gt 0.0, ng)
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
   print,ng
   setplotcolors
   ;
   lammin = 5000.0
   lammax = 11000.0
   dlam   = 500.0
   nlam   = (lammax - lammin)/dlam
   xlam   = lammin+0.5*dlam + dlam*findgen(nlam)
   nlama  = histogram(lama, min=lammin, max=lammax, binsize=dlam)
   nlamb  = histogram(lamb, min=lammin, max=lammax, binsize=dlam)
   FOR i = 0, nlam-1 DO print,i,xlam[i],nlama[i],nlamb[i]
   ;stop
   ;
   !P.MULTI = [0,1,3]
   window,0,xsize=400,ysize=600
   ;
   plot,lams,sens,xrange=[5000.0,10000.0],xstyle=1,$
    xtitle='Wavelength [Angstroms]', ytitle='Sensitivity', $
    title='Grism sensitivity', charsize=1.8
   polyfill, lams, sens, color=!lyellow
   oplot, lams, sens
   plot,xlam,nlama,xrange=[5000.0,10000.0],yrange=[0,20],xstyle=1,ystyle=1, psym=10, $
    xtitle='Wavelength [Angstroms]', ytitle='N', title='Direct image selection', charsize=1.8
   FOR i = 0, nlam-1 DO BEGIN 
      xbx = xlam[i] + dlam*[-0.5, -0.5, 0.5, 0.5]
      ybx = nlama[i]*[0.0, 1.0, 1.0, 0.0]
      polyfill, xbx, ybx, color=!lgreen
      oplot, xbx, ybx
      ENDFOR 
   plot,xlam,nlamb,xrange=[5000.0,10000.0],yrange=[0,20],xstyle=1,ystyle=1, psym=10, $
    xtitle='Wavelength [Angstroms]', ytitle='N', title='Grism image selection', charsize=1.8
   FOR i = 0, nlam-1 DO BEGIN 
      xbx = xlam[i] + dlam*[-0.5, -0.5, 0.5, 0.5]
      ybx = nlamb[i]*[0.0, 1.0, 1.0, 0.0]
      polyfill, xbx, ybx, color=!lgreen
      oplot, xbx, ybx
   ENDFOR 
   makepng,'lamhist.png',/color
END 

