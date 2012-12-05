PRO compile_goodsn_idt_master
   ;
   ; Compile relevant goods parameters and place goods objects 
   ; on the ACS IDT pixel frame
   detim   = 'detectionImage.fits'
   catb    = 'goodsn11_idt_b.cat'
   catv    = 'goodsn11_idt_v.cat'
   cati    = 'goodsn11_idt_i.cat'
   catz    = 'goodsn11_idt_z.cat'
   filr    = 'goodsn11.reg'
   film    = 'goods_matchin.dat'
   fmti    = '(a,f,f,f,i,f,f,f,f,f,f,f,f,f,f,f,f,l)'
   fmtm    = '(i5,2x,f10.2,f10.2,f8.3,f7.3,f8.3,f7.3,f8.3,f7.3,f8.3,f7.3,2x,a22,f12.7,f12.7,i5,i8,f8.2,f7.1,f7.1,f7.1,f7.3,f7.1,f7.1,i6)'
   ;fmto    = '(i5,2x,a22,f12.7,f12.7,f7.3,f7.3,f7.3,f7.3,f7.3,f7.3,f7.3,f7.3)'
   catmast = 'goodsn_idt_mast.cat'
   fwhmmax = 10.0
   imax    = 25.0
   ethresh =  0.6
   szpixg  = 0.03
   szpixi  = 0.05
   ;
   ; trans strcture from mrmatch:
   a       = -6.494578686    
   b       =  1.000268634     
   c       =  0.001649286     
   d       =  9.207672369     
   e       = -0.001638957    
   f       =  1.000303173 
   ; 
   ; approx photometry limit in each band, set errors on
   ; undetected objects to these limits.
   limb   = 27.4
   limv   = 26.8
   limi   = 26.3
   limz   = 26.7
   szrat  = szpixg / szpixi
   arat   = szrat^2
   ;
   readcol, catb, namb, rab, decb, sectb, xsb, ysb, areab, pab, kradb, $
    fradb, fwhmb, classb, flagb, magb, emagb, ab, bb, idmb, format=fmti
   readcol, catv, namv, rav, decv, sectv, xsv, ysv, areav, pav, kradv, $
    fradv, fwhmv, classv, flagv, magv, emagv, av, bv, idmv, format=fmti
   readcol, cati, nami, rai, deci, secti, xsi, ysi, areai, pai, kradi, $
    fradi, fwhmi, classi, flagi, magi, emagi, ai, bi, idmi, format=fmti
   readcol, catz, namz, raz, decz, sectz, xsz, ysz, areaz, paz, kradz, $
    fradz, fwhmz, classz, flagz, magz, emagz, az, bz, idmz, format=fmti
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
   ; read header of detection Image
   fits_read, detim, data, hdr, /header_only
   ; 
   ; determine pixel positions in detection Image
   adxy, hdr, raz, decz, xidt, yidt
   xidt    = xidt + 1.0
   yidt    = yidt + 1.0
   ; 
   ; transform
   xidtt   = a + b*xidt + c*yidt
   yidtt   = d + e*xidt + f*yidt
   ;
   ; convert things to pixel size in IDT image
   areaz   = arat*areaz
   kradz   = szrat*kradz
   fradz   = szrat*fradz
   fwhmz   = szrat*fwhmz
   az      = szrat*az
   bz      = szrat*bz
   ;
   ; write master catalog
   n = n_elements(namz)
   ind = lindgen(n)
   openw, lu, catmast, /get_lun
   printf, lu, '# 1     INDEX '
   printf, lu, '# 2     X_IDT '
   printf, lu, '# 3     Y_IDT '
   printf, lu, '# 4     MAG_AUTO_F850LP'
   printf, lu, '# 5     MAGERR_AUTO_F850LP'
   printf, lu, '# 6     MAG_AUTO_F435W'
   printf, lu, '# 7     MAGERR_AUTO_F435W'
   printf, lu, '# 8     MAG_AUTO_F606W'
   printf, lu, '# 9     MAGERR_AUTO_F606W'
   printf, lu, '# 10    MAG_AUTO_F775W'
   printf, lu, '# 11    MAGERR_AUTO_F775W'
   printf, lu, '# 12    ID_IAU '
   printf, lu, '# 13    ALPHA_J2000'
   printf, lu, '# 14    DELTA_J2000'
   printf, lu, '# 15    SECT_REFNUM'
   printf, lu, '# 16    ISOAREA_IMAGE'
   printf, lu, '# 17    THETA_IMAGE'
   printf, lu, '# 18    KRON_RADIUS'
   printf, lu, '# 19    FLUX_RADIUS'
   printf, lu, '# 20    FWHM_IMAGE'
   printf, lu, '# 21    CLASS_STAR'
   printf, lu, '# 22    A_IMAGE'
   printf, lu, '# 23    B_IMAGE'
   printf, lu, '# 24    ID_MOSAIC'
   FOR i = 0, n-1 DO printf, lu, ind[i], xidtt[i], yidtt[i], magz[i], emagz[i], magb[i], $
    emagb[i], magv[i], emagv[i], magi[i], emagi[i], $
    ljust(namz[i],22), raz[i], decz[i], sectz[i], areaz[i], paz[i], kradz[i], fradz[i], $
    fwhmz[i], classz[i], az[i], bz[i], idmz[i], format=fmtm
   free_lun, lu
   ;
   ; write regions file
   openw, lu, filr, /get_lun
   printf, lu, 'global color=green '
   FOR i = 0, n-1 DO BEGIN 
      str = 'circle('+strtrim(string(fix(xidtt[i]+0.5)),2)+','+strtrim(string(fix(yidtt[i]+0.5)),2)+','+strtrim(string(fix(fwhmz[i]*0.5+0.5)),2)+')'
      printf, lu, str
   ENDFOR 
   free_lun, lu
   ;
   ; save small objects that are fairly bright for matching
   k = where(magi LE imax AND fwhmz LE fwhmmax, nk)
   openw, lu, film, /get_lun
   FOR i = 0, nk-1 DO printf, lu, ind[k[i]], xidt[k[i]], yidt[k[i]], magi[k[i]]
   free_lun, lu
END 
