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

PRO mk_emline_directcat
   readcol,'aXeclean.cat',id, xim, yim, xw, yw, mag, a, b, th, aw, bw, fwhm, ww, cl, $
    format='(i,f,f,f,f,f,f,f,f,f,f,f,f,f,f)'
   fnda     = 0*fix(id)
   fndb     = 0*fix(id)
   fndc     = 0*fix(id)
   fndf     = 0*fix(id)
   zcc      = 0.0*fwhm - 1.0
   zff      = 0.0*fwhm - 1.0
   ;
   readcol, 'emsource.cat', ida, xima, yima, maga, aa, ba, tha, fwhma, classa, raa, deca, minsna, maxsna, nord0a, lina, quala, lama, w50a, fluxa, $
    format='(i,f,f,f,f,f,f,f,f,f,f,f,f,i,i,i,f,f,f)'
   fmt  = '(i,f,f,f,f,f,f,f,i,f,f,a,f,f,f,a,f,f,f,f,f,f,f,f,a)'
   readcol,'hdfn_blem.out',idg,ximg,yimg,magb,ab,bb,thb,fwhmb,flagb,ctsb,skyb,qcutb,apeakb,ashiftb,aw50b,$
       qccb,cpeakb,cshiftb,cw50b,lamb,cprmsb,ximb,yimb,fluxb,csexid,$
       format=fmt
   ;
   ; good sources hdfn_blem
   qcutb = strtrim(qcutb,2)
   qccb  = strtrim(qccb,2)
   g = where(qcutb NE '3' AND qcutb NE '5' AND qcutb NE '6' AND qcutb NE '7' $
             AND qccb NE 'k' AND qccb NE '9' AND lamb GT 0.0 AND strtrim(csexid,2) NE '-1', ng)
   print,'number of good blem entries : ', ng
   ;
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
   csexid = temporary(fix(csexid[g]))
   ;
   ; indicate number OF times found by each method
   FOR i = 0, n_elements(ida)-1 DO BEGIN 
      k = where(id EQ ida[i], nk)
      IF nk EQ 1 THEN fnda[k[0]] = fnda[k[0]]+1
   ENDFOR 
   FOR i = 0, n_elements(csexid)-1 DO BEGIN 
      k = where(id EQ csexid[i], nk)
      IF nk EQ 1 THEN fndb[k[0]] = fndb[k[0]]+1
   ENDFOR 
   ;
   ; now read in C00 & FLY99 matched cats
   readcol, 'c00_direct_matched.cat', idc, ximc, yimc, ac, bc, thc, fwhmc, magc, clc, id_c00, $
    rah, ram, ras, dd, dm, ds, rmagc, zc, typc, $
    format='(i,f,f,f,f,f,f,f,f,i,i,i,f,a,i,f,f,f,a)'
   ;
   ; read FLY99 matches
   readcol, 'fly_direct_matched.cat', idf, ximf, yimf, af, bf, thf, fwhmf, magf, clf, id_fly, $ 
    id_hdf, rastr, decstr, imagf, zf, typf, $
    format='(i,f,f,f,f,f,f,f,f,i,a,a,a,f,f,i)'
   FOR i = 0, n_elements(idc)-1 DO BEGIN 
      k = where(id EQ idc[i], nk)
      IF nk EQ 1 THEN BEGIN 
         fndc[k[0]] = fndc[k[0]]+1
         zcc[k[0]]  = zc[i]
      ENDIF 
   ENDFOR 
   FOR i = 0, n_elements(idf)-1 DO BEGIN 
      k = where(id EQ idf[i], nk)
      IF nk EQ 1 THEN BEGIN 
         fndf[k[0]] = fndf[k[0]]+1
         zff[k[0]]  = zf[i]
      ENDIF 
   ENDFOR 
   nt = fnda + fndb + fndc + fndf
   g  = where(nt GT 0, ng)
   ;
   ; write output
   fmt = '(i5,f8.1,f8.1,f7.2,f6.1,f6.1,f7.1,f7.2,f7.3,i4,i4,i4,i4,f6.2,f6.2)'
   IF ng GT 0 THEN BEGIN 
      openw,lu,'crossmatched.dat',/get_lun
      FOR i = 0, ng-1 DO printf,lu,id[g[i]], xim[g[i]], yim[g[i]], mag[g[i]], a[g[i]], b[g[i]], th[g[i]], fwhm[g[i]], cl[g[i]], $
       fnda[g[i]], fndb[g[i]], fndc[g[i]], fndf[g[i]], zcc[g[i]], zff[g[i]], format=fmt 
      free_lun, lu
   ENDIF 
  
END 

PRO plot_magdist
   readcol, 'crossmatched.dat', id, xim, yim, mag, a, b, th, fwhm, cl, fnda, fndb, fndc, fndf, zcc, zff, $
    format='(i,f,f,f,f,f,f,f,i,i,i,i,f,f)'
   kab = where(fnda GT 0 OR fndb GT 0, nab)
   kc  = where(fndc GT 0, nc)
   kf  = where(fndf GT 0, nf)
   print, 'Number of grism emission line sources : ', nab
   print, 'Number of Cohen et al. matches        : ', nc
   print, 'Number of FLY99 matches               : ', nf
   ;
   !P.MULTI = [0,1,3]
   window,0,xsize=400,ysize=600
   ;
   ; Some stats
   medab    = median(mag[kab])
   medc     = median(mag[kc])
   medf     = median(mag[kf])
   ;
   ; calculate histograms
   mmin     = 18.0
   mmax     = 28.0
   dm       = 0.5
   nm       = (mmax - mmin)/dm
   xm       = mmin + dm*(0.5 + findgen(nm))
   nmagab   = histogram(mag[kab], min=mmin, max=mmax, binsize=dm)
   nmagc    = histogram(mag[kc],  min=mmin, max=mmax, binsize=dm)
   nmagf    = histogram(mag[kf],  min=mmin, max=mmax, binsize=dm)
   ;
   plot,xm,nmagab,xrange=[mmin,mmax],xstyle=1, psym=10, yrange=[0,10], ystyle=1, $
    xtitle='Magauto [ABmag]', ytitle='N', title='Combined emission line detections', charsize=1.8
   FOR i = 0, nm-1 DO BEGIN 
      xbx = xm[i] + dm*[-0.5, -0.5, 0.5, 0.5]
      ybx = nmagab[i]*[0.0, 1.0, 1.0, 0.0]
      polyfill, xbx, ybx, color=!lgreen
      oplot, xbx, ybx
   ENDFOR 
   xyouts, 18.5, 9.0, 'N = '+strtrim(string(nab),2)
   xyouts, 18.5, 8.0, 'median(mag) = '+strtrim(string(medab),2)
   ;
   plot,xm,nmagc,xrange=[mmin,mmax],xstyle=1, yrange=[0,40], ystyle=1, psym=10, $
    xtitle='Magauto [ABmag]', ytitle='N', title='Cohen et al. (2000) spectro-z catalog', charsize=1.8
   FOR i = 0, nm-1 DO BEGIN 
      xbx = xm[i] + dm*[-0.5, -0.5, 0.5, 0.5]
      ybx = nmagc[i]*[0.0, 1.0, 1.0, 0.0]
      polyfill, xbx, ybx, color=!lgreen
      oplot, xbx, ybx
   ENDFOR 
   xyouts, 18.5, 36.0, 'N = '+strtrim(string(nc),2)
   xyouts, 18.5, 32.0, 'median(mag) = '+strtrim(string(medc),2)
   ;
   plot,xm,nmagf,xrange=[mmin,mmax],xstyle=1, yrange=[0,120], ystyle=1, psym=10, $
    xtitle='Magauto [ABmag]', ytitle='N', title='Fernandez-Soto et al. (1999) photo-z catalog', charsize=1.8
   FOR i = 0, nm-1 DO BEGIN 
      xbx = xm[i] + dm*[-0.5, -0.5, 0.5, 0.5]
      ybx = nmagf[i]*[0.0, 1.0, 1.0, 0.0]
      polyfill, xbx, ybx, color=!lgreen
      oplot, xbx, ybx
   ENDFOR 
   xyouts, 18.5, 108.0, 'N = '+strtrim(string(nf),2)
   xyouts, 18.5,  96.0, 'median(mag) = '+strtrim(string(medf),2)
   makepng,'maghist.png',/color
END 

PRO zmatch_ab_c
   readcol, 'crossmatched.dat', id, xim, yim, mag, a, b, th, fwhm, cl, fnda, fndb, fndc, fndf, zcc, zff, $
    format='(i,f,f,f,f,f,f,f,i,i,i,i,f,f)'
   readcol, 'emsource.cat', ida, xima, yima, maga, aa, ba, tha, fwhma, classa, raa, deca, minsna, maxsna, nord0a, lina, quala, lama, w50a, fluxa, $
    format='(i,f,f,f,f,f,f,f,f,f,f,f,f,i,i,i,f,f,f)'
   fmt  = '(i,f,f,f,f,f,f,f,i,f,f,a,f,f,f,a,f,f,f,f,f,f,f,f,a)'
   readcol,'hdfn_blem.out',idg,ximg,yimg,magb,ab,bb,thb,fwhmb,flagb,ctsb,skyb,qcutb,apeakb,ashiftb,aw50b,$
       qccb,cpeakb,cshiftb,cw50b,lamb,cprmsb,ximb,yimb,fluxb,csexid,$
       format=fmt
   ;
   ; good sources hdfn_blem
   qcutb = strtrim(qcutb,2)
   qccb  = strtrim(qccb,2)
   g = where(qcutb NE '3' AND qcutb NE '5' AND qcutb NE '6' AND qcutb NE '7' $
             AND qccb NE 'k' AND qccb NE '9' AND lamb GT 0.0 AND strtrim(csexid,2) NE '-1', ng)
   print,'number of good blem entries : ', ng
   ;
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
   csexid = temporary(fix(csexid[g]))

   kac = where(fnda GT 0 AND fndc GT 0, nac)
   kbc = where(fndb GT 0 AND fndc GT 0, nbc)

   print, 'Number of sources in both dir selcetion & c00 : ', nac
   print, 'Number of sources in both grism selcetion & c00 : ', nbc

   wl0 = [6562.817, 5006.85, 3727., 1216.]
   
   !P.MULTI = [0,1,2]
   window,0,xsize=320,ysize=640

   IF nac GT 0 THEN BEGIN 
      nlin  = total(fnda[kac])
      zc2   = make_array(nlin, /float, value=-1)
      za    = zc2
      dza   = zc2
      jj    = 0
      FOR i = 0, nac-1 DO BEGIN 
         kk = where(ida EQ id[kac[i]], nk)
         FOR k = 0, nk-1 DO BEGIN 
            wl      = lama[kk[k]]
            zguess  = (wl - wl0)/wl0
            ksrt    = sort(abs(zguess - zcc[kac[i]]))
            za[jj]  = zguess[ksrt[0]]
            zc2[jj] = zcc[kac[i]]
            dza[jj]  = zc2[jj] - za[jj]
            jj      = jj + 1
         ENDFOR 
      ENDFOR 
      print,dza[sort(dza)]
      meandz = mean(dza)
      rmsdz  = sqrt(mean(dza*dza))
      ;
      ; Now do the plot
      zr = [0.0, 1.05*max([za, zc2])]
      plot,zc2,za,xrange=zr,yrange=zr,xstyle=1,ystyle=1,psym=4,$
       xtitle='z(C00)',ytitle='z(em) [best match]',title='Direct image selection'
      oplot,zr,zr,linestyle=0
      xyouts,0.1,1.33,'mean(diff) = '+strtrim(string(meandz),2),charsize=1.2
      xyouts,0.1,1.25,'rms(diff)  = '+strtrim(string(rmsdz),2),charsize=1.2
      xyouts,0.1,1.17,'objects    = '+strtrim(string(nac),2),charsize=1.2
      xyouts,0.1,1.09,'lines      = '+strtrim(string(fix(nlin)),2),charsize=1.2
   ENDIF  

   IF nbc GT 0 THEN BEGIN 
      nlin  = total(fndb[kbc])
      zc3   = make_array(nlin, /float, value=-1)
      zb    = zc3
      dzb   = zc3
      jj    = 0
      FOR i = 0, nbc-1 DO BEGIN 
         kk = where(csexid EQ id[kbc[i]], nk)
         FOR k = 0, nk-1 DO BEGIN 
            wl      = lamb[kk[k]]
            zguess  = (wl - wl0)/wl0
            ksrt    = sort(abs(zguess - zcc[kbc[i]]))
            zb[jj]  = zguess[ksrt[0]]
            zc3[jj] = zcc[kbc[i]]
            dzb[jj] = zc3[jj] - zb[jj]
            jj      = jj + 1
         ENDFOR 
      ENDFOR 
      print,dzb[sort(dzb)]
      meandz = mean(dzb)
      rmsdz  = sqrt(mean(dzb*dzb))
      ;
      ; Now do the plot
      zr = [0.0, 1.05*max([za, zc2])]
      plot,zc3,zb,xrange=zr,yrange=zr,xstyle=1,ystyle=1,psym=4,$
       xtitle='z(C00)',ytitle='z(em) [best match]',title='Grism image selection'
      oplot,zr,zr,linestyle=0
      xyouts,0.1,1.33,'mean(diff) = '+strtrim(string(meandz),2),charsize=1.2
      xyouts,0.1,1.25,'rms(diff)  = '+strtrim(string(rmsdz),2),charsize=1.2
      xyouts,0.1,1.17,'objects    = '+strtrim(string(nbc),2),charsize=1.2
      xyouts,0.1,1.09,'lines      = '+strtrim(string(fix(nlin)),2),charsize=1.2
   ENDIF  
   makepng,'zmatch_ab_c.png',/color
END 

PRO zmatch_ab_f
   readcol, 'crossmatched.dat', id, xim, yim, mag, a, b, th, fwhm, cl, fnda, fndb, fndc, fndf, zcc, zff, $
    format='(i,f,f,f,f,f,f,f,i,i,i,i,f,f)'
   readcol, 'emsource.cat', ida, xima, yima, maga, aa, ba, tha, fwhma, classa, raa, deca, minsna, maxsna, nord0a, lina, quala, lama, w50a, fluxa, $
    format='(i,f,f,f,f,f,f,f,f,f,f,f,f,i,i,i,f,f,f)'
   fmt  = '(i,f,f,f,f,f,f,f,i,f,f,a,f,f,f,a,f,f,f,f,f,f,f,f,a)'
   readcol,'hdfn_blem.out',idg,ximg,yimg,magb,ab,bb,thb,fwhmb,flagb,ctsb,skyb,qcutb,apeakb,ashiftb,aw50b,$
       qccb,cpeakb,cshiftb,cw50b,lamb,cprmsb,ximb,yimb,fluxb,csexid,$
       format=fmt
   ;
   ; good sources hdfn_blem; use quality cut
   qcutb = strtrim(qcutb,2)
   qccb  = strtrim(qccb,2)
   g = where(qcutb NE '3' AND qcutb NE '5' AND qcutb NE '6' AND qcutb NE '7' $
             AND qccb NE 'k' AND qccb NE '9' AND lamb GT 0.0 AND strtrim(csexid,2) NE '-1', ng)
   print,'number of good blem entries : ', ng
   ;
   ; keep only the good things
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
   csexid = temporary(fix(csexid[g]))

   kaf = where(fnda GT 0 AND fndf GT 0, naf)
   kbf = where(fndb GT 0 AND fndf GT 0, nbf)

   print, 'Number of sources in both dir selcetion & c00 : ', naf
   print, 'Number of sources in both grism selcetion & c00 : ', nbf
   ;
   ; wl0 is the rest wavelength of the lines it will guess.
   wl0 = [6562.817, 5006.85, 3727., 1216.]
   
   !P.MULTI = [0,1,2]
   window,0,xsize=320,ysize=640

   IF naf GT 0 THEN BEGIN 
      nlin  = total(fnda[kaf])
      zf2   = make_array(nlin, /float, value=-1)
      za    = zf2
      dza   = zf2
      jj    = 0
      FOR i = 0, naf-1 DO BEGIN 
         kk = where(ida EQ id[kaf[i]], nk)
         ;
         ; find line that gives z closest to photo-z
         FOR k = 0, nk-1 DO BEGIN 
            wl      = lama[kk[k]]
            zguess  = (wl - wl0)/wl0
            ksrt    = sort(abs(zguess - zff[kaf[i]]))
            za[jj]  = zguess[ksrt[0]]
            zf2[jj] = zff[kaf[i]]
            dza[jj]  = zf2[jj] - za[jj]
            jj      = jj + 1
         ENDFOR 
      ENDFOR 
      print,dza[sort(dza)]
      meandz = mean(dza)
      rmsdz  = sqrt(mean(dza*dza))
      ;
      ; Now do the plot
      zr = [0.0, 1.05*max([za, zf2])]
      plot,zf2,za,xrange=zr,yrange=zr,xstyle=1,ystyle=1,psym=4,$
       xtitle='z(FLY99)',ytitle='z(em) [best match]',title='Direct image selection'
      oplot,zr,zr,linestyle=0
      xyouts,0.1,1.41,'mean(diff) = '+strtrim(string(meandz),2),charsize=1.2
      xyouts,0.1,1.33,'rms(diff)  = '+strtrim(string(rmsdz),2),charsize=1.2
      xyouts,0.1,1.25,'objects    = '+strtrim(string(nbf),2),charsize=1.2
      xyouts,0.1,1.17,'lines      = '+strtrim(string(fix(nlin)),2),charsize=1.2
   ENDIF  

   IF nbf GT 0 THEN BEGIN 
      nlin  = total(fndb[kbf])
      zf3   = make_array(nlin, /float, value=-1)
      zb    = zf3
      dzb   = zf3
      jj    = 0
      FOR i = 0, nbf-1 DO BEGIN 
         kk = where(csexid EQ id[kbf[i]], nk)
         FOR k = 0, nk-1 DO BEGIN 
            wl      = lamb[kk[k]]
            zguess  = (wl - wl0)/wl0
            ksrt    = sort(abs(zguess - zff[kbf[i]]))
            zb[jj]  = zguess[ksrt[0]]
            zf3[jj] = zff[kbf[i]]
            dzb[jj] = zf3[jj] - zb[jj]
            jj      = jj + 1
         ENDFOR 
      ENDFOR 
      print,dzb[sort(dzb)]
      meandz = mean(dzb)
      rmsdz  = sqrt(mean(dzb*dzb))
      ;
      ; Now do the plot
      zr = [0.0, 1.05*max([za, zf2])]
      plot,zf3,zb,xrange=zr,yrange=zr,xstyle=1,ystyle=1,psym=4,$
       xtitle='z(FLY99)',ytitle='z(em) [best match]',title='Grism image selection'
      oplot,zr,zr,linestyle=0
      xyouts,0.1,1.41,'mean(diff) = '+strtrim(string(meandz),2),charsize=1.2
      xyouts,0.1,1.33,'rms(diff)  = '+strtrim(string(rmsdz),2),charsize=1.2
      xyouts,0.1,1.25,'objects    = '+strtrim(string(nbf),2),charsize=1.2
      xyouts,0.1,1.17,'lines      = '+strtrim(string(fix(nlin)),2),charsize=1.2
   ENDIF  
   makepng,'zmatch_ab_f.png',/color
END 
