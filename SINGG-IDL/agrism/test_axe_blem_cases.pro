PRO test_axe_blem_cases
   ;
   ; test perverse cases in matching axe results at blem positions.
   ;
   ; G. Meurer 11/2004
   ;
   fila  = '/home/meurer/ACS/Grism/HDFN_jun04/Apsis/Work/AXE_blem/Plots/hdfn_blem_axe_emsource.cat'
   filb  = '/home/meurer/ACS/Grism/HDFN_jun04/Apsis/Work/BLEM_aug04a/hdfn_blem_emsource.dat'
   sigclip = 3.0
   dx    = 50.0
   xmin  = -1.5*dx
   ; xmin  = -2500.0 + 0.5*dx
   xmax  = 2500.0 - 0.5*dx
   nx    = fix((xmax - xmin)/dx)
   xhist = xmin + dx*(0.5 + findgen(nx))
   ;
   readcol, fila, ida, xima, yima, maga, aa, ba, thetaa, w50a, classa, raa, deca, minsna, maxsna, nord0a, lina, quala, cena, widtha, fluxa, conta, ewa, $
    format='(i,f,f,f,f,f,f,f,f,f,f,f,f,i,i,i,f,f,f,f,f)'
   readcol, filb, idb, xgb, ygb, agb, bgb, fwhmgb, thetagb, apeakb, aw50b, cpeakb, cshiftb, cxpb, cypb, cwlb, cw50b, cflxb, sexidb, $
    format='(i,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,i)'
   ;
   na       = n_elements(ida)
   nb       = n_elements(idb)
   ;
   ; find number of matches for each blem id.
   ; store waveleength differences
   foundina = make_array(nb,/int,value=0)
   dlam     = make_array(na,/float,value=0.0)
   FOR ii = 0, nb-1 DO BEGIN 
      jj           = where(ida EQ idb[ii], njj)
      foundina[ii] = njj
      IF njj GT 0 THEN dlam[jj]     = cena[jj] - cwlb[ii]
   ENDFOR 
   ;
   j1       = where(foundina EQ 0, notina)
   j2       = where(foundina GT 1, nmulti)
   j3       = where(foundina GT 2, nmany)
   grm_avsigclip, dlam, sigclip, 50.0, meandlam, sigdlam, nuse, nrej, nit, /verbose
   ;mdlam    = moment(dlam)
   ;meandlam = mdlam[0]
   ;sigdlam  = sqrt(mdlam[1])
   ;
   ; print results
   forprint, ida, dlam
   print, 'Number of lines measured with AXE     : ', na
   print, 'Number of lines found by BLEM         : ', nb
   print, 'Number of lines not found by AXE      : ', notina, idb[j1]
   print, 'Number of multiple lines found by AXE : ', nmulti, idb[j2]
   print, 'Number of multiple>2 found by AXE     : ', nmany
   print, 'Mean wavelength difference            : ', meandlam
   print, 'dispersion wavelength difference      : ', sigdlam
   print, 'Min, max dlam                         : ', min(dlam), max(dlam)
   print, 'nuse, nrej, nit                       : ', nuse, nrej, nit
   ;
   ; plot histogram
   hist  = histogram(abs(dlam), binsize=dx, min=xmin, max=xmax)
   plot, xhist, hist, xrange=[xmin-0.5*dx,xmax+0.5*dx], yrange=[0,50], xstyle=1, ystyle=1, $
    xtitle='Wavelength difference ['+angstsym()+']', ytitle='N', psym=10
END 
