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
