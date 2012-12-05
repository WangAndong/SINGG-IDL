PRO rd_flycat, file, idf, xf, yf, rastr, decstr, idhdf, imag, zspec, zphot, type
   ;
   ; Read Table 4 from Fernandez-Soto, Lanzetta, & Yahil (1999) 
   ; file   -> name of file containing cohen et al catalog
   ; idf    <- index in table 4
   ; xf     <- column position in HDF
   ; yf     <- row position in HDF
   ; rastr  <- RA
   ; decstr <- Dec
   ; imag   <- I (F814W) band magnitude
   ; zspec  <- spectroscopic redshifts
   ; zphot  <- Photometric redshift
   ; type   <- spectral template type
   ;
   ; G. Meurer 12/2002
   fmt = '(i4, f7.1, f7.1, 1x, a12, 1x, a12, 1x, a14, f6.2, a5, 1x, f6.3, i2)'
   readfmt, file, fmt, idf, xf, yf, rastr, decstr, idhdf, imag, zstr, zphot, type
   zspec  = 0.0*zphot - 9.999
   ;
   ; Fix RA & Dec strings
   strput,rastr,':',2
   strput,rastr,':',5
   neg = strpos(decstr, '-')
   j   = where(neg EQ -1, nj)
   IF nj GT 0 THEN BEGIN 
      temp = decstr[j]
      strput,temp,'+',0
      decstr[j] = temp
   ENDIF 
   j   = where(strmid(decstr,1,1) EQ ' ',nj)
   IF nj GT 0 THEN BEGIN 
      temp = decstr[j]
      strput,temp,'0',1
      decstr[j] = temp
   ENDIF 
   strput,decstr,':',3
   strput,decstr,':',6
   ;
   ; set zphot where zstr is not blank
   j = where(strlen(strtrim(zstr,2)) GT 0, nj)
   IF nj GT 0 THEN zspec[j] = float(zstr[j])
END 

PRO fly_thead, unit, title
   printf,unit,'<P><TABLE border=1 cellpadding=3>'

   printf,unit,'<th colspan=8 bgcolor=a8a8a8><center>'
   printf,unit,title+'<br>'

   printf,unit,'</center></th>'

   printf,unit,'<tr bgcolor="#a8a8a8">'
   printf,unit,'<td>ID</td>'
   printf,unit,'<td>X<sub>image</sub><br>Y<sub>image</sub></td>'
   printf,unit,'<td>A<sub>image</sub><br>B<sub>image</sub></td>'
   printf,unit,'<td>theta<sub>image</sub><br>FWHM<sub>image</sub></td>'
   printf,unit,'<td>MAG<sub>AUTO</sub><br>Class</td>'
   printf,unit,'<td rowspan=2>Stamp<br>Ribbon</td>'
   printf,unit,'<td rowspan=2>Spectrum</td>'
   printf,unit,'<td rowspan=2>data</td>'
   printf,unit,'</tr>'
   printf,unit,'<tr bgcolor="#b8b8b8">'
   printf,unit,'<td>ID: FLY99<br>ID: HDFN</td>' 
   printf,unit,'<td>RA<br>Dec</td>'
   printf,unit,'<td>type</td>'
   printf,unit,'<td>z<sub>phot</sub><br>(z<sub>spec</sub>)</td>'
   printf,unit,'<td>I mag</td>'
   printf,unit,'</tr>'
END 

PRO fly_trow, unit, id, posim, aim, bim, thetaim, fwhm, magauto, class, $
              idf, idhdf, rastr, decstr, type, zphot, zspec, imag, $
              fstmp, fribn, fspec, fdat=fdat, ffit=ffit
   printf,unit,'<tr bgcolor="#c0eec0">'
   printf,unit,'<td>',id,'</td>'
   printf,unit,'<td>',posim[0],'<br>',posim[1],'</td>'
   printf,unit,'<td>',aim,'<br>',bim,'</td>'
   printf,unit,'<td>',thetaim,'<br>',fwhm,'</td>'
   printf,unit,'<td>',magauto,'<br>',class,'</td>'
   printf,unit,'<td rowspan=2><img src="'+fstmp+'">','<br>&nbsp;<br>','<img src="'+fribn+'"></td>'
   printf,unit,'<td rowspan=2><img src="'+fspec+'"></td>'
   IF keyword_set(fdat) THEN BEGIN 
      IF strlen(strtrim(fdat)) GT 0 THEN entry = '<a href="'+fdat+'">data</a>' ELSE entry = ''
   ENDIF ELSE BEGIN 
      entry = ''
   ENDELSE 
   IF keyword_set(ffit) THEN BEGIN 
      IF strlen(strtrim(ffit)) GT 0 THEN BEGIN 
         IF strlen(entry) GT 0 THEN entry = entry + '<br>'
         entry = entry + '<a href="'+ffit+'">Gfit</a>' 
      ENDIF 
   ENDIF 
   IF keyword_set(fdat) OR keyword_set(ffit) THEN BEGIN 
      IF strlen(entry) EQ 0 THEN entry = '&nbsp;'
      printf,unit,'<td rowspan=2>'+entry+'</td>' 
   ENDIF 
   printf,unit,'</tr>'
   printf,unit,'<tr bgcolor="#d0eed0">'
   printf,unit,'<td>',idf,'<br>', idhdf, '</td>'
   printf,unit,'<td>',rastr,'<br>',decstr,'</td>'
   printf,unit,'<td>',type,'</td>'
   entry = '<td>'+strtrim(string(zphot),2)
   IF zspec GE 0.0 THEN entry = entry+'<br>'+strtrim(string(zspec),2)
   entry = entry + '</td>'
   printf,unit,entry
   printf,unit,'<td>',imag,'</td>'
   printf,unit,'</tr>'

END 

PRO fly_grism_page, filhtml, title, elm, id, pos, aim, bim, thetaim, $
                w50, magauto, class, elmc, idf, idhdf, rastr, decstr, imag, zphot, zspec, type, $
                pstmp, pspec, pribn, pscii=pscii, pgfit=pgfit
   ;
   ; make a page of grism data in tabular form
   ;
   IF keyword_set(pscii) THEN BEGIN
      pdat  = pscii
      dodat = 1b
   ENDIF ELSE BEGIN 
      pdat  = 'spec_'
      dodat = 0b
   ENDELSE 
   ;
   showfit = keyword_set(pgfit)
   ;
   openw, unitp, filhtml, /get_lun, /more
   html_pagetop, unitp, title, bgcolor="#c0c0c0"
   fly_thead, unitp, title
   ;
   FOR i = 0, n_elements(elm)-1 DO BEGIN
      ide = id[elm[i]]
      IF dodat THEN fdat = namdat(pscii,ide) ELSE fdat = ' '
      IF showfit THEN BEGIN 
         ffit = nampng(pgfit,ide)
         test = findfile(ffit, count=count)
         print,ffit,count
         IF count EQ 0 THEN ffit = ' '
      ENDIF ELSE BEGIN 
         ffit = ' ' 
      ENDELSE 
      IF keyword_set(pscii) THEN BEGIN
         fdat = namdat(pscii,ide)
         fly_trow, unitp, id[elm[i]], [pos[elm[i],0],pos[elm[i],1]], aim[elm[i]], bim[elm[i]], $
          thetaim[elm[i]], w50[elm[i]], magauto[elm[i]], class[elm[i]], $
          idf[elmc[i]], idhdf[elmc[i]], rastr[elmc[i]], decstr[elmc[i]], type[elmc[i]], $
          zphot[elmc[i]], zspec[elmc[i]], imag[elmc[i]], $
          nampng(pstmp,ide), nampng(pribn,ide), nampng(pspec,ide), fdat=fdat, ffit=ffit
      ENDIF ELSE BEGIN
         fly_trow, unitp, id[elm[i]], [pos[elm[i],0],pos[elm[i],1]], aim[elm[i]], bim[elm[i]], $
          thetaim[elm[i]], w50[elm[i]], magauto[elm[i]], class[elm[i]], $
          idf[elmc[i]], idhdf[elmc[i]], rrastr[elmc[i]], decstr[elmc[i]], type[elmc[i]], $
          zphot[elmc[i]], zspec[elmc[i]], imag[elmc[i]], $
          nampng(pstmp,ide), nampng(pribn,ide), nampng(pspec,ide)
      ENDELSE
   ENDFOR
   ;
   html_tfoot, unitp
   html_pagebot, unitp, 'FLY_GRISM_PAGE', 'G.R. Meurer', email='meurer@pha.jhu.edu'
   free_lun, unitp
   
END

PRO fly_matchcat, file, id, posim, aim, bim, thetaim, w50, magauto, class,  $
       idf, idhdf, rastr, decstr, imag, zphot, type
   ;
   ; make ascii catalog of matched cohen et al list
   fmt = '(i4,f8.1,f8.1,f6.1,f6.1,f7.1,f6.1,f7.2,f7.3,i6,1x,a14,2x,a11,1x,a11,f6.2,f7.3,i4)'
   openw,luno,file,/get_lun
   printf,luno,'# id  x_im    y_im      a     b   theta  fwhm magauto class fly99   HDF-id         <-- RA ---> <-- Dec --> Imag    z    typ'
   for i = 0, n_elements(id)-1 DO $
    printf,luno,id[i],posim[i,0],posim[i,1],aim[i],bim[i],thetaim[i],w50[i],magauto[i],class[i], $
                idf[i],idhdf[i],rastr[i],decstr[i],imag[i],zphot[i],type[i],format=fmt
   close,luno
END 

PRO hdfn_fly99_dec02a
   ;
   ; Make pages of grism plots for HDFN galaxies in the
   ; photometric redshift catalog of Fernandex-Soto et
   ; al. 1999 (FLY99, or fly for short). 
   ;
   workdir  = '/data3/acs27/meurer/grism/hdfn_new/axe_all/plots/'
   matchdir = '/data3/acs27/meurer/grism/hdfn_new/axe_all/fly99match/'
   flycat   = matchdir + 'table4.dat'
   dircat   = matchdir + 'j8eb01g800l_drz_sci_1.cat'
   matcha   = matchdir + 'matched.mtA'  ; this should be fly cat
   matchb   = matchdir + 'matched.mtB'  ; this should be direct cat
   pgpfx    = 'fly_'
   titlepfx = 'Grism spectra of Fernandez-Soto et al. (1999) matches - page : '
   matchcat = workdir + 'fly_direct_matched.cat'
   pngmag   = workdir + 'fly_magmatch.png'
   psmag    = workdir + 'fly_magmatch.ps'
   filgrim  = '/data3/acs27/meurer/grism/hdfn/axe/j8eb01g800l_drz_sci_1.SPC.fits'
   filsens  = '/data3/acs27/meurer/grism/sensG800Lwfc_gd153_ave.fits'
   pscii    = 'spec_fly_'
   pgfit    = 'spec_gfit_'
   lrange   = [5800.0, 9800.0]
   cd,workdir
   ;
   ; read exptime, seterrmod, grism sensitivity
   fits_read,filgrim,dum,hgrim,/header_only
   exptime = sxpar(hgrim, 'EXPTIME')
   seterrmod2, hgrim, dark=dark, /verbose, covar=1.0
   setsensmod, filsens, extsens, fudge=sfudge
   ;
   ; Read catalogs
   rd_grdirct_cat, dircat, id, xim, yim, magauto, aim, bim, thetaim, w50, class
   rd_flycat, flycat, idf, xf, yf, rastr, decstr, idhdf, imag, zspec, zphot, type
   ;
   ; Read output from mrmatch - make sure that ida and idb are the same size
   rd_mrmatch, matcha, ida, xa, ya, maga
   rd_mrmatch, matchb, idb, xb, yb, magb
   IF (n_elements(ida) NE n_elements(idb)) THEN BEGIN 
      print,'*** ida and idb do not have the same # of elements ***'
      stop
   ENDIF 
   ; 
   ; Get indecis of matches
   match, id, idb, sub, subb
   match, idf, ida, subc, suba
   ;
   ; make sure all elements of suba & subb are matched, and then put vectors in the 
   ; right order.
   IF (n_elements(subb) NE n_elements(idb)) THEN BEGIN 
      print,'*** Elements of idb are not all matched ***'
      stop
   ENDIF 
   IF (n_elements(suba) NE n_elements(ida)) THEN BEGIN 
      print,'*** Elements of ida are not all matched ***'
      stop
   ENDIF 
   ;
   ; these steps insure that the matched output still corresponds.
   k           = sort(subb)
   subb        = temporary(subb[k])
   sub         = temporary(sub[k])
   k           = sort(suba)
   suba        = temporary(suba[k])
   subc        = temporary(subc[k])
   ;
   ; now do a photometric redshift sort
   k           = sort(zphot[subc])
   sub         = temporary(sub[k])
   subb        = temporary(subb[k])
   suba        = temporary(suba[k])
   subc        = temporary(subc[k])
   ;
   ; Make data files for spectra
   print, 'Making output ascii files ...'
   lun         = fxposit(filgrim,0)
   old         = 0
   FOR i = 0, n_elements(idb)-1 DO BEGIN
      skip      = idb[i] - old - 1
      print,old,idb[i],skip
      IF skip LT 0 THEN BEGIN 
         free_lun, lun
         lun    = fxposit(filgrim, idb[i])
         bintab = kludge_mrdfits(lun,0)
      ENDIF ELSE BEGIN 
         bintab = kludge_mrdfits(lun,skip)
      ENDELSE 
      old    = idb[i]
      fdat   = pscii + trim(string(idb[i]),2) + '.dat'
      grism_updflux, bintab, exptime=exptime
      grism_outascii, fdat, bintab, magauto[sub[i]], zphot[subc[i]], xim[sub[i]], yim[sub[i]], lamlim=lrange
      print,fdat
   ENDFOR 
   free_lun,lun
   ;
   ; Make web pages of matched spectra
   posim       = [[xim], [yim]] 
   pstmp       = 'stmp_'
   pribn       = 'ribn_'
   pspec       = 'spcf_'
   rowsperpage = 50
   npage       = 1
   i0          = 0
   WHILE i0 LE n_elements(sub)-1 DO  BEGIN 
      i1       = min([i0 + rowsperpage,n_elements(ida)])-1
      j        = i0 + indgen(i1-i0+1)
      elm      = sub[j]
      elmc     = subc[j]
      filhtml  = pgpfx + trim(string(npage),2) + '.html'
      title    = titlepfx + trim(string(npage),2) 
      fly_grism_page, filhtml, title, elm, id, posim, aim, bim, thetaim, $
       w50, magauto, class,  elmc, idf, idhdf, rastr, decstr, imag, zphot, zspec, type, $
       pstmp, pspec, pribn, pscii=pscii, pgfit=pgfit
      npage    = npage + 1
      i0       = i1 + 1
      print, ' '
      print, ' completed page : ', filhtml
      print, ' '
   ENDWHILE 
   ;
   ; save catalog
   fly_matchcat, matchcat, id[sub], posim[sub,*], aim[sub], bim[sub], thetaim[sub], $
       w50[sub], magauto[sub], class[sub], idf[subc], idhdf[subc], rastr[subc], decstr[subc], $
       imag[subc], zphot[subc], type[subc]   
   ; Make some plots ?
   window,0,xsize=400,ysize=400
   plot,imag[subc],magauto[sub],psym=1,xtitle='Imag (Fly99)',ytitle='Magauto (direct image)',$
    xrange=[16,28],yrange=[16,28],xstyle=1,ystyle=1
   makepng,pngmag,/color
   psl
   plot,imag[subc],magauto[sub],psym=1,xtitle='Imag (Fly99)',ytitle='Magauto (direct image)',$
    xrange=[16,28],yrange=[16,28],xstyle=1,ystyle=1
   psend,psmag
   setbgfg,!white,!black
   ; keywait,'press any key to continue'
   ; plot,xa,ya,psym=1
   ; oplot,xb,yb,psym=6
END

PRO hdfn_fly99_dec02b
   ;
   ; Make pages of grism plots for HDFN galaxies in the
   ; photometric redshift catalog of Fernandex-Soto et
   ; al. 1999 (FLY99, or fly for short). 
   ;
   workdir  = '/data2/acs27/meurer/grism/hdfn_dec02/Axe/output/plots/'
   matchdir = '/data2/acs27/meurer/grism/hdfn_dec02/Axe/FLY99match/'
   flycat   = matchdir + 'table4.dat'
   dircat   = matchdir + 'hdfn_g800l_2.cat'
   matcha   = matchdir + 'matched.mtA'  ; this should be fly cat
   matchb   = matchdir + 'matched.mtB'  ; this should be direct cat
   pgpfx    = 'fly_'
   titlepfx = 'Grism spectra of Fernandez-Soto et al. (1999) matches - page : '
   matchcat = workdir + 'fly_direct_matched.cat'
   pngmag   = workdir + 'fly_magmatch.png'
   psmag    = workdir + 'fly_magmatch.ps'
   filgrim  = '/data2/acs27/meurer/grism/hdfn_dec02/Axe/output/hdfn_g800l_2.SPC.fits'
   filsens  = '/home/meurer/acs/axe/conf/ACS.WFC.1st.sens.fits' 
   extsens  = 1
   pscii    = 'spec_fly_'
   pgfit    = 'spec_gfit_'
   lrange   = [5800.0, 9800.0]
   cd,workdir
   ;
   ; read exptime, seterrmod, grism sensitivity
   fits_read,filgrim,dum,hgrim,/header_only
   exptime = sxpar(hgrim, 'EXPTIME')
   seterrmod2, hgrim, dark=dark, /verbose, covar=1.0
   setsensmod, filsens, extsens, fudge=sfudge
   ;
   ; Read catalogs
   rd_grdirct_cat, dircat, id, xim, yim, magauto, aim, bim, thetaim, w50, class
   rd_flycat, flycat, idf, xf, yf, rastr, decstr, idhdf, imag, zspec, zphot, type
   ;
   ; Read output from mrmatch - make sure that ida and idb are the same size
   rd_mrmatch, matcha, ida, xa, ya, maga
   rd_mrmatch, matchb, idb, xb, yb, magb
   IF (n_elements(ida) NE n_elements(idb)) THEN BEGIN 
      print,'*** ida and idb do not have the same # of elements ***'
      stop
   ENDIF 
   ; 
   ; Get indecis of matches
   match, id, idb, sub, subb
   match, idf, ida, subc, suba
   ;
   ; make sure all elements of suba & subb are matched, and then put vectors in the 
   ; right order.
   IF (n_elements(subb) NE n_elements(idb)) THEN BEGIN 
      print,'*** Elements of idb are not all matched ***'
      stop
   ENDIF 
   IF (n_elements(suba) NE n_elements(ida)) THEN BEGIN 
      print,'*** Elements of ida are not all matched ***'
      stop
   ENDIF 
   ;
   ; these steps insure that the matched output still corresponds.
   k           = sort(subb)
   subb        = temporary(subb[k])
   sub         = temporary(sub[k])
   k           = sort(suba)
   suba        = temporary(suba[k])
   subc        = temporary(subc[k])
   ;
   ; now do a photometric redshift sort
   k           = sort(zphot[subc])
   sub         = temporary(sub[k])
   subb        = temporary(subb[k])
   suba        = temporary(suba[k])
   subc        = temporary(subc[k])
   ;
   ; Make data files for spectra
   print, 'Making output ascii files ...'
   lun         = fxposit(filgrim,0)
   old         = 0
   FOR i = 0, n_elements(idb)-1 DO BEGIN
      jj        = where(id EQ idb[i], njj)
      IF njj NE 0 THEN print, njj
      skip      = jj[0] - old - 1
      print,jj[0],old,idb[i],skip
      IF skip LT 0 THEN BEGIN 
         free_lun, lun
         lun    = fxposit(filgrim, jj[0])
         bintab = kludge_mrdfits(lun,0)
      ENDIF ELSE BEGIN 
         bintab = kludge_mrdfits(lun,skip)
      ENDELSE 
      old    = jj[0]
      fdat   = pscii + trim(string(idb[i]),2) + '.dat'
      grism_updflux, bintab, exptime=exptime
      grism_outascii, fdat, bintab, magauto[sub[i]], zphot[subc[i]], xim[sub[i]], yim[sub[i]], lamlim=lrange
      print,fdat
   ENDFOR 
   free_lun,lun
   ;
   ; Make web pages of matched spectra
   posim       = [[xim], [yim]] 
   pstmp       = 'stmp_'
   pribn       = 'ribn_'
   pspec       = 'spcf_'
   rowsperpage = 50
   npage       = 1
   i0          = 0
   WHILE i0 LE n_elements(sub)-1 DO  BEGIN 
      i1       = min([i0 + rowsperpage,n_elements(ida)])-1
      j        = i0 + indgen(i1-i0+1)
      elm      = sub[j]
      elmc     = subc[j]
      filhtml  = pgpfx + trim(string(npage),2) + '.html'
      title    = titlepfx + trim(string(npage),2) 
      fly_grism_page, filhtml, title, elm, id, posim, aim, bim, thetaim, $
       w50, magauto, class,  elmc, idf, idhdf, rastr, decstr, imag, zphot, zspec, type, $
       pstmp, pspec, pribn, pscii=pscii, pgfit=pgfit
      npage    = npage + 1
      i0       = i1 + 1
      print, ' '
      print, ' completed page : ', filhtml
      print, ' '
   ENDWHILE 
   ;
   ; save catalog
   fly_matchcat, matchcat, id[sub], posim[sub,*], aim[sub], bim[sub], thetaim[sub], $
       w50[sub], magauto[sub], class[sub], idf[subc], idhdf[subc], rastr[subc], decstr[subc], $
       imag[subc], zphot[subc], type[subc]   
   ; Make some plots ?
   window,0,xsize=400,ysize=400
   plot,imag[subc],magauto[sub],psym=1,xtitle='Imag (Fly99)',ytitle='Magauto (direct image)',$
    xrange=[16,28],yrange=[16,28],xstyle=1,ystyle=1
   makepng,pngmag,/color
   psl
   plot,imag[subc],magauto[sub],psym=1,xtitle='Imag (Fly99)',ytitle='Magauto (direct image)',$
    xrange=[16,28],yrange=[16,28],xstyle=1,ystyle=1
   psend,psmag
   setbgfg,!white,!black
   ; keywait,'press any key to continue'
   ; plot,xa,ya,psym=1
   ; oplot,xb,yb,psym=6
END
