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

