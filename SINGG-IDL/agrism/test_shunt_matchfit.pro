PRO test_shunt_matchfit
   ;
   ; play around with fitting the results from match for 
   ; for use with shunt
   ;
   ; G. Meurer 10/2005
   ;
   fila = 'udf095_grism_squash_shunt_ed.cat'
   filb = 'notfaint_grismframe.cat'
   nsig = 2.5
   nitermax = 50
   ;
   aa = -67.105740430   
   bb = 25.047279189    
   cc = -0.003323654    
   dd = 0.445403038     
   ee = -0.016664412    
   ff = 0.999501064 
   ;
   ; read data to fit
   readcol, fila, ida, xa, ya, maga, format='(l,f,f,f)'
   readcol, filb, xb, yb, idb, magb, format='(f,f,l,x,x,x,x,x,x,x,x,x,x,f)'
   ;
   ; look at match results to get IDs to fit
   readcol, 'matched.mtA', useida, format='(l)'
   readcol, 'matched.mtB', useidb, format='(l)'
   nm   = n_elements(useida)
   ppa  = make_array(nm, /long, value=-1)
   ppb  = ppa
   ;
   FOR ii = 0, nm-1 DO BEGIN 
      kk = where(ida EQ useida[ii], nkk)
      IF nkk EQ 1 THEN ppa[ii] = kk[0]
      kk = where(idb EQ useidb[ii], nkk)
      IF nkk EQ 1 THEN ppb[ii] = kk[0]
   ENDFOR 
   qq   = where(ppa GE 0 AND ppb GE 0, nqq)
   IF nqq LT 1 THEN stop
   ;
   ; load arrays to fit.
   iida_all = ida[ppa[qq]]
   xxa_all  = double(xa[ppa[qq]])
   yya_all  = double(ya[ppa[qq]])
   mma_all  = double(maga[ppa[qq]])
   iidb_all = idb[ppa[qq]]
   xxb_all  = double(xb[ppb[qq]])
   yyb_all  = double(yb[ppb[qq]])
   mmb_all  = double(magb[ppb[qq]])
   exxb_all = make_array(nqq, value=1.0d0)
   eyyb_all = make_array(nqq, value=1.0d0)
   emmb_all = make_array(nqq, value=0.1d0)
   nall     = nqq
   xin_all  = [[xxa_all], [yya_all]]
   ;
   ; initialize good array
   goodnew  = indgen(nall)
   ngood    = nall
   ; 
   ; load first guess fit parameters
   poutx    = double([aa, bb, cc])
   pouty    = double([dd, ee, ff])
   poutm    = double([mean(mmb_all - mma_all), 1.0])
   ;
   ; start iteration loop
   niter    = 0
   REPEAT BEGIN 
      niter  = niter + 1
      print, ' '
      print, 'Beginning iteration # :', niter
      ;
      startx  = poutx
      starty  = pouty
      startm  = poutm
      good    = goodnew
      ngood   = n_elements(good)
      ;
      ; load arrays
      iida   = iida_all[good]
      xxa    = xxa_all[good]
      yya    = yya_all[good]
      mma    = mma_all[good]
      iidb   = iidb_all[good]
      xxb    = xxb_all[good]
      yyb    = yyb_all[good]
      mmb    = mmb_all[good]
      exxb   = exxb_all[good]
      eyyb   = eyyb_all[good]
      emmb   = emmb_all[good]
      xin    = [[xxa], [yya]]
      ;
      ; get new parameters
      poutx   = mpfitfun('myfun_lin2d', xin, xxb, exxb, startx)
      pouty   = mpfitfun('myfun_lin2d', xin, yyb, eyyb, starty)
      poutm   = mpfitfun('myfun_lin1d', mma, mmb, emmb, startm)
      ;
      ; get results and residuals
      xfit    = myfun_lin2d(xin_all, poutx)
      yfit    = myfun_lin2d(xin_all, pouty)
      mfit    = myfun_lin1d(mma_all, poutm)
      xres    = xxb_all - xfit
      yres    = yyb_all - yfit
      mres    = mmb_all - mfit
      grm_avsigclip, xres[good], nsig, 50, mean_xres, sig_xres, nusex, nrejx, nitx
      grm_avsigclip, yres[good], nsig, 50, mean_yres, sig_yres, nusey, nrejy, nity
      grm_avsigclip, mres[good], nsig, 50, mean_mres, sig_mres, nusem, nrejm, nitm
      ;
      ; print some results
      print, ' Summary for X fit - mean of residuals : ', mean_xres, ' Dispersion : ', sig_xres
      print, ' Summary for Y fit - mean of residuals : ', mean_yres, ' Dispersion : ', sig_yres
      print, ' Summary for M fit - mean of residuals : ', mean_mres, ' Dispersion : ', sig_mres
      ;
      goodnew = where(abs(xres - mean_xres) LE nsig*sig_xres AND $
                      abs(yres - mean_yres) LE nsig*sig_yres AND $
                      abs(mres - mean_mres) LE nsig*sig_mres, ngoodnew)
      print, ' Number of good points - this fit : ', ngood, ' next fit : ', ngoodnew
      ;
      IF ngoodnew EQ ngood THEN test = max(abs(good - goodnew)) ELSE test = 1
      ;
      ; finish loop
      ENDREP UNTIL niter EQ nitermax OR test EQ 0
   ;
   stop
END 
