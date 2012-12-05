PRO compile_goodsn_bpz_phot
   ;
   ; compile goods photometry for use with BPZ
   ;
   catb    = 'goodsn11_idt_b.cat'
   catv    = 'goodsn11_idt_v.cat'
   cati    = 'goodsn11_idt_i.cat'
   catz    = 'goodsn11_idt_z.cat'
   fmti    = '(a,f,f,f,i,f,f,f,f,f,f,f,f,f,f,f,f,l)'
   fmto    = '(i5,2x,a22,f12.7,f12.7,f7.3,f7.3,f7.3,f7.3,f7.3,f7.3,f7.3,f7.3)'
   catph   = 'goodsn_idt_phot.cat'
   ethresh =  0.6
   dmag    = 0.1
   mrange  = [15.0, 31.0]
   xmag    = mrange[0] + dmag*(0.5 + findgen((mrange[1] - mrange[0])/dmag))
   ; 
   ; approx photometry limit in each band, set errors on
   ; undetected objects to these limits.
   limb   = 27.4
   limv   = 26.8
   limi   = 26.3
   limz   = 26.7
   ;
   readcol, catb, namb, rab, decb, sectb, xsb, ysb, areab, pab, kradb, $
    fradb, fwhmb, classb, flagb, magb, emagb, ab, bb, idmb, format=fmti
   readcol, catv, namv, rav, decv, sectv, xsv, ysv, areav, pav, kradv, $
    fradv, fwhmv, classv, flagv, magv, emagv, av, bv, idmv, format=fmti
   readcol, cati, nami, rai, deci, secti, xsi, ysi, areai, pai, kradi, $
    fradi, fwhmi, classi, flagi, magi, emagi, ai, bi, idmi, format=fmti
   readcol, catz, namz, raz, decz, sectz, xsz, ysz, areaz, paz, kradz, $
    fradz, fwhmz, classz, flagz, magz, emagz, az, bz, idmz, format=fmti
   hmagb  = histogram(magb, binsize=dmag, min=mrange[0], max=mrange[1])
   hmagv  = histogram(magv, binsize=dmag, min=mrange[0], max=mrange[1])
   hmagi  = histogram(magi, binsize=dmag, min=mrange[0], max=mrange[1])
   hmagz  = histogram(magz, binsize=dmag, min=mrange[0], max=mrange[1])
   ;
   k      = where(magb GT 90.0 OR emagb GT ethresh, nk)
   IF nk GT 0 THEN BEGIN 
      magb[k]  = 99.0
      emagb[k] = limb
   ENDIF 
   k      = where(magv GT 90.0 OR emagv GT ethresh, nk)
   IF nk GT 0 THEN BEGIN 
      magv[k]  = 99.0
      emagv[k] = limv
   ENDIF 
   k      = where(magi GT 90.0 OR emagi GT ethresh, nk)
   IF nk GT 0 THEN BEGIN 
      magi[k]  = 99.0
      emagi[k] = limi
   ENDIF 
   k      = where(magz GT 90.0 OR emagz GT ethresh, nk)
   IF nk GT 0 THEN BEGIN 
      magz[k]  = 99.0
      emagz[k] = limz
   ENDIF 
   ;
   ; quick check if names match
   checkb = where(namb NE namz, nb)
   checkv = where(namv NE namz, nv)
   checki = where(nami NE namz, ni)
   print, nb, nv, ni
   ;
   ;plot, xmag, hmagb, xrange=mrange, xstyle=1, psym=10
   ;keywait, 'type something'
   ;plot, xmag, hmagv, xrange=mrange, xstyle=1, psym=10
   ;keywait, 'type something'
   ;plot, xmag, hmagi, xrange=mrange, xstyle=1, psym=10
   ;keywait, 'type something'
   ;plot, xmag, hmagz, xrange=mrange, xstyle=1, psym=10
   ;keywait, 'type something'
   ;forprint, xmag, hmagb, hmagv, hmagi, hmagz
   ;
   ; output
   n = n_elements(namz)
   ind = lindgen(n)
   openw, lu, catph, /get_lun
   printf, lu, '#1     INDEX '
   printf, lu, '#2     ID_IAU '
   printf, lu, '#3     ALPHA_J2000'
   printf, lu, '#4     DELTA_J2000'
   printf, lu, '#5     MAG_AUTO_F435W'
   printf, lu, '#6     MAGERR_AUTO_F435W'
   printf, lu, '#7     MAG_AUTO_F606W'
   printf, lu, '#8     MAGERR_AUTO_F606W'
   printf, lu, '#9     MAG_AUTO_F775W'
   printf, lu, '#10    MAGERR_AUTO_F775W'
   printf, lu, '#11    MAG_AUTO_F850LP'
   printf, lu, '#12    MAGERR_AUTO_F850LP'
   FOR i = 0, n-1 DO printf, lu, ind[i], ljust(namz[i],22), raz[i], decz[i], magb[i], $
    emagb[i], magv[i], emagv[i], magi[i], emagi[i], magz[i], emagz[i], $
    format=fmto
   free_lun, lu
END 
