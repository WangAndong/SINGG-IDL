PRO sfmue, hardfile=hardfile
   ddb       = 'singg_derived'
   csfr      = 7.9d-42             ; Kennicutt et al 1994, salpeter IMF
   snlimit   = 2.0                 ; H-alpha flux detection limit
   srange    = [-4.5,1]            ; range in log surface brightness
   trange    = [8,12.]             ; range in log tgas
   erange    = [0.5, 3.0]          ; range in log EW
   rrange    = [2.2, 4.5]          ; range in log Re
   hrange    = [7.3, 11.0]         ; range in log M_HI
   mrange    = [-13.0, -24.0]      ; range in log M_R
   murange   = [25.0, 18.25]
   xmargin   = [7.0, 3.0]
   ymargin   = [4.0, 1.0]
   lsesblim  = -1.80
   etitle    = '!6 log(EW [!3'+angstsym()+'!6])'
   stitle    = '!6 log(!7R!6!DSFR!N [M!Dsun!N yr!U-1!N kpc!U-2!N])'
   ttitle    = '!6 log(t!Dgas!N [yr])'
   htitle    = '!6 log(M!dHI!n [M!Dsun!N])'
   mtitle    = '!6 M!dR,0!n [ABmag]'
   mutitle   = '!7 l!6!de!n(R) [ABmag arcsec!u-2!n]'
   charsize  = 2.5
   symsize   = 1.5
   nsigma    = 2.5
   fmt2      = '(a14, a14, f7.3, i5, f8.2, " +/- ", f4.2, f8.2, " +/- ", f4.2, f6.2, f6.2)' 
   ;
   c_se      = 4.0d0*!pi*(0.206265*3.085678d27)^2*csfr
   lc_se     = alog10(c_se)
   ;
   elfha     = alog10(1.0 + 1.0/snlimit)
   cmd       = 'err_logf_ha_t < '+strtrim(string(elfha),2)
   dbopen, ddb
   list      = good_derived(exclopt=2)
   lista     = dbfind(cmd+', frac_ha_t > 1.0',list)
   listb     = dbfind(cmd+', frac_ha_t < 0.999',list)
   dbext, lista, 'name,frac_ha_t,tgas0_t,logse_ha0_t,ew50_0_t,logmhi,mabs_r0_t,re_r_t,re_ha_t,distance,mu_e_r0_t', $
           namea,frachaa,tgasa0,lsehaa0,ew50ac,lmhia,mabsra0,rera,rehaa,dista,muera
   dbext, listb, 'name,frac_ha_t,tgas0_t,logse_ha0_t,ew50_0_t,logmhi,mabs_r0_t,re_r_t,re_ha_t,distance,mu_e_r0_t', $
           nameb,frachab,tgasb0,lsehab0,ew50bc,lmhib,mabsrb0,rerb,rehab,distb,muerb
   dbclose
   ;
   ltgasa0   = 9.0 + alog10(tgasa0)
   lsesfra0  = lc_se + lsehaa0
   ltgasb0   = 9.0 + alog10(tgasb0)
   lsesfrb0  = lc_se + lsehab0
   lew50ac   = alog10(ew50ac)
   lew50bc   = alog10(ew50bc)
   ;
   ; calculate correlation coefficients
   rxy1 = rxy(muera, lsesfra0)
   rxy2 = rxy(muera, ltgasa0)
   rxy3 = rxy(muera, lew50ac)
   print, 'Correlation coefficients: '
   print, 'mu_e(R) vs. log(S_SFR)', rxy1
   print, 'mu_e(R) vs. log(t_gas)', rxy2
   print, 'mu_e(R) vs. log(EW_50)', rxy3
   ;
   indused1 = 1b
   indused2 = 1b
   print,' Fitting ... '
   iter_sixlin, muera, lsesfra0, 2, nsigma, aa1, sigaa1, bb1, sigbb1, $
                sigx1, sigy1, nused1, niter=niter1, indused=indused1
   iter_sixlin, muera, ltgasa0, 2, nsigma, aa2, sigaa2, bb2, sigbb2, $
                sigx2, sigy2, nused2, niter=niter2, indused=indused2
   iter_sixlin, muera, lew50ac, 2, nsigma, aa3, sigaa3, bb3, sigbb3, $
                sigx3, sigy3, nused3, niter=niter3, indused=indused3
   printf, -1, ljust('mu_e(R)',14), ljust('log(S_SFR)',14), rxy1, nused1, aa1[0], sigaa1[0], $
                           bb1[0], sigbb1[0], sigx1[0], sigy1[0], format=fmt2
   printf, -1, ljust('mu_e(R)',14), ljust('log(t_gas)',14), rxy2, nused2, aa2[0], sigaa2[0], $
                           bb2[0], sigbb2[0], sigx2[0], sigy2[0], format=fmt2
   printf, -1, ljust('mu_e(R)',14), ljust('log(EW_50)',14), rxy3, nused3, aa3[0], sigaa3[0], $
                           bb3[0], sigbb3[0], sigx3[0], sigy3[0], format=fmt2
   ;
   ; set plot parameters
   IF keyword_set(hardfile) THEN BEGIN 
      xs = 9.0
      ys = xs/3.25
      yoff=1.0
      xoff=1.0
      thick=2
      set_plot,'ps',/copy, /interpolate
      IF strpos(strlowcase(hardfile), '.eps') GT 0 THEN $
         device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color,/encapsulated ELSE $
         device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color
      charsize = 0.5*charsize
      symsize  = 0.6*symsize
   ENDIF ELSE BEGIN 
      wxsize   = 900
      wysize   = fix(float(wxsize)/3.25)
      thick    = 1
      window, 0, xsize=wxsize, ysize=wysize
   ENDELSE 
   ;
   setplotcolors
   setbgfg,!white,!black
   erase   
   ;
   !p.noerase = 0
   !p.multi   = [3, 3, 1]
   plot, muera, lsesfra0, xrange=murange, yrange=srange, xstyle=1, ystyle=1, $
         psym=sym(1), xtitle=mutitle, ytitle=stitle, charsize=charsize, symsize=symsize, $
         xmargin=xmargin, ymargin=ymargin, thick=thick, $
         xthick=xthick, ythick=thick, charthick=thick
   ;
   ; show fit
   xp         = murange
   yp         = aa1[0] + xp*bb1[0]
   oplot, xp, yp, thick=thick+1
   yp         = yp + sigy1[0]
   oplot, xp, yp, thick=thick, linestyle=2
   yp         = yp - 2.0*sigy1[0]
   oplot, xp, yp, thick=thick, linestyle=2
   oplot, muerb, lsesfrb0, psym=sym(6), color=!red, symsize=0.8*symsize
   oplot, muera, lsesfra0, psym=sym(1), color=!blue, symsize=symsize
   ; ----------------------------------------------------------------------
   !p.multi   = [2, 3, 1]
   plot, muera, ltgasa0, xrange=murange, yrange=trange, xstyle=1, ystyle=1, $
         psym=sym(1), xtitle=mutitle, ytitle=ttitle, charsize=charsize, symsize=symsize, $
         xmargin=xmargin, ymargin=ymargin, thick=thick, $
         xthick=xthick, ythick=thick, charthick=thick
   ;
   ; show fit
   xp         = murange
   yp         = aa2[0] + xp*bb2[0]
   oplot, xp, yp, thick=thick+1
   yp         = yp + sigy2[0]
   oplot, xp, yp, thick=thick, linestyle=2
   yp         = yp - 2.0*sigy2[0]
   oplot, xp, yp, thick=thick, linestyle=2
   oplot, muera, ltgasa0, psym=sym(1), color=!blue, symsize=symsize
   ;oplot, muerb, ltgasb0, psym=sym(1), color=!red, symsize=symsize
   ; ----------------------------------------------------------------------
   !p.multi   = [1, 3, 1]
   plot, muera, lew50ac, xrange=murange, yrange=erange, xstyle=1, ystyle=1, $
         psym=sym(1), xtitle=mutitle, ytitle=etitle, charsize=charsize, symsize=symsize, $
         xmargin=xmargin, ymargin=ymargin, thick=thick, $
         xthick=xthick, ythick=thick, charthick=thick
   ;
   ; show fit
   xp         = murange
   yp         = aa3[0] + xp*bb3[0]
   oplot, xp, yp, thick=thick+1
   yp         = yp + sigy3[0]
   oplot, xp, yp, thick=thick, linestyle=2
   yp         = yp - 2.0*sigy3[0]
   oplot, xp, yp, thick=thick, linestyle=2
   oplot, muerb, lew50bc, psym=sym(6), color=!red, symsize=0.8*symsize
   oplot, muera, lew50ac, psym=sym(1), color=!blue, symsize=symsize
   ;
   ; finish
   !p.multi   = 0
   !p.noerase = 0
   IF keyword_set(hardfile) THEN BEGIN 
      psend, hardfile, /noprint, /clobber
   ENDIF 
   ;
   mom         = moment(lew50ac)
   lew50ac_avg = mom[0]
   lew50ac_sig = sqrt(mom[1])
   print, 'Mean log(EW){single} = ',lew50ac_avg,' sigma = ',lew50ac_sig
   print, 'corresponding EW = ', 10.0^lew50ac_avg
END 
