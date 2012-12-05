PRO test_match_rjbcats, xsh, ysh, rmatch, magmatch
   ; 
   ; xsh,ysh -> additive offset to apply to cat1 to get cat2 coords
   wd = '/home/meurer/ACS/Grism/HDFN_jun04/Apsis/Work/Match'
   cat1 = 'hdf1.cat'
   cat2 = 'hdf2.cat'
   cat3 = 'detim_radec.dat'
   out1 = 'hdf1_overlap.cat'
   out2 = 'hdf2_overlap.cat'
   xoff = -189.
   yoff = -62.
   ;
   cd, wd, current=cwd
   readcol, cat1, id1, ra1, dec1, dum, mag1, format='(l,f,f,i,f)'
   readcol, cat2, id2, ra2, dec2, dum, mag2, format='(l,f,f,i,f)'
   readcol, cat3, id3, ra3, dec3, mag3, format='(l,f,f,f)'
   x1   = ra1 + xoff
   x2   = ra2 + xoff - xsh
   x3   = ra3 + xoff
   y1   = dec1 + yoff
   y2   = dec2 + yoff - ysh
   y3   = dec3 + yoff
   xr   = [min([x1, x2]),max([x1, x2])]
   yr   = [min([y1, y2]),max([y1, y2])]
   setplotcolors
   plot, x1, y1, xrange=xr, yrange=yr, xstyle=1, ystyle=1, psym=1
   oplot, x1, y1, psym=1, color=!purple
   oplot, x2, y2, psym=1, color=!dgreen
   oplot, x3, y3, psym=sym(4), color=!dred, symsize=0.8
   ;
   ; define polygon for overlap check
   xc = make_array(5,/float)
   yc = make_array(5,/float)
   FOR i = 0, 3 DO BEGIN 
      cursor, g,h, /data, /down
      xc[i] = g
      yc[i] = h
      gg = make_array(1,value=g)
      hh = make_array(1,value=h)
      oplot,gg,hh,psym=sym(1),color=!red
   ENDFOR
   xc[4] = xc[0]
   yc[4] = yc[0]
   oplot,xc,yc,psym=0,color=!red
   ;
   ; now find points within polygon in both arrays
   xc = xc[0:3]
   yc = yc[0:3]
   in1 = inside(x1,y1,xc,yc,/index)
   in2 = inside(x2,y2,xc,yc,/index)
   nin1 = n_elements(in1)
   nin2 = n_elements(in2)
   print,nin1,nin2
   ;
   ; make arrays to test
   idt1 = id1[in1]
   xt1  = x1[in1]
   yt1  = y1[in1]
   mt1  = mag1[in1]
   idt2 = id2[in2]
   xt2  = x2[in2]
   yt2  = y2[in2]
   mt2  = mag2[in2]
   ptr  = make_array(nin1,/long,value=-1)
   ;
   ; loop through & test
   rmatchsq = rmatch*rmatch
   FOR i = 0,nin1-1 DO BEGIN 
      rsq  = (xt2 - xt1[i])^2 + (yt2 - yt1[i])^2
      dm   = abs(mt2 - mt1[i])
      k    = where(rsq LE rmatchsq AND dm LE magmatch, nk)
      IF nk GT 0 THEN BEGIN 
         IF nk EQ 1 THEN BEGIN 
            j = k[0]
         ENDIF ELSE BEGIN 
            k = sort(rsq)
            j = k[0]
         ENDELSE 
         ptr[i] = j
      ENDIF 
   ENDFOR 
   ;
   ; save positions for measuring
   k     = where(ptr GE 0,nk)
   IF nk GT 0 THEN BEGIN 
      idm1  = idt1[k]
      xm1   = xt1[k]
      ym1   = yt1[k]
      mm1   = mt1[k]
      idm2  = id2[ptr[k]]
      xm2   = xt2[ptr[k]] + xsh  ; original coord sys
      ym2   = yt2[ptr[k]] + ysh  ; original coord sys
      mm2   = mt2[ptr[k]]
      dx    = xm2 - xm1
      dy    = ym2 - ym1
      dm    = mm2 - mm1
      grm_avsigclip, dx, 3.0, 30, meandx, sigmadx, nusedx, nrejdx, nitdx, /verbose
      grm_avsigclip, dy, 3.0, 30, meandy, sigmady, nusedy, nrejdy, nitdy, /verbose
      grm_avsigclip, dm, 3.0, 30, meandm, sigmadm, nusedm, nrejdm, nitdm, /verbose
      dxr    = meandx + 3.0*max([sigmadx,sigmady])*[-1.0,1.0]
      dyr    = meandy + 3.0*max([sigmadx,sigmady])*[-1.0,1.0]
      plot, dx,dy, xrange=dxr, yrange=dyr, xstyle=1, ystyle=1, psym=1, $
       xtitle='dx',ytitle='dy'
      keywait, 'type something for next plot'
      plot, xm1, dx, xrange=xr, yrange=dxr, xstyle=1, ystyle=1, psym=sym(1), $
       xtitle='x',ytitle='dx,dy'
      oplot, xm1, dx, psym=sym(1), color=!red
      oplot, xm1, dy, psym=sym(4), color=!blue
      keywait, 'type something for next plot'
      plot, ym1, xrange=yr, yrange=dxr, xstyle=1, ystyle=1, psym=sym(1), $
       xtitle='y',ytitle='dx,dy'
      oplot, ym1, dx, psym=sym(1), color=!red
      oplot, ym1, dy, psym=sym(4), color=!blue
      ;
      FOR i = 0,nk-1 DO print, idm1[i],xm1[i],ym1[i],mm1[i],idm2[i],xm2[i],ym2[i],mm2[i],dx[i],dy[i],dm[i],$
       format='(i6,f11.6,f11.6,f8.4," | ",i6,f11.6,f11.6,f8.4," | ",f10.6,f10.6,f8.4)'
      print, ' '
      print, 'Number of matches = ', nk
      print, 'dx (mean, sigma, nuse, nrej, nit) : ', meandx, sigmadx, nusedx, nrejdx, nitdx
      print, 'dy (mean, sigma, nuse, nrej, nit) : ', meandy, sigmady, nusedy, nrejdy, nitdy
      print, 'dm (mean, sigma, nuse, nrej, nit) : ', meandm, sigmadm, nusedm, nrejdm, nitdm
      ;
   ENDIF ELSE BEGIN 
      print, 'HUH NK le 0????'
   ENDELSE 
   ;
   cd, cwd

END 
