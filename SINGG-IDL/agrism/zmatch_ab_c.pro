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

