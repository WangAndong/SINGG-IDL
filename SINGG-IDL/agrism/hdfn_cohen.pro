PRO rd_c00cat, file, idc, rastr, decstr, rmag, zspec, qual, typstr, source 
   ; file   -> name of file containing cohen et al catalog
   ; idc    <- index in cat
   ; rastr  <- RA
   ; decstr <- Dec
   ; rmag   <- R band magnitude
   ; zspec  <- spectroscopic redshifts
   ; qual   <- quality code
   ; typstr <- Type 
   ; source <- source of redshift
   readcol, file, idc, ra, dec, rmag, zspec, qual, typstr, source, $
    format='(i,f,f,f,f,a,a,a)'
   rastr  = make_array(n_elements(idc),/string,value=' ')
   decstr = rastr
   ;
   ; remove curly braces from typstr, turn ra & dec into strings
   FOR i = 0, n_elements(idc)-1 DO BEGIN
      typstr[i] = strjoin(strsplit(typstr[i],'{}',/extract))
      radec     = adstring(ra[i],dec[i],1)
      rastr[i]  = strmid(radec,1,11)
      decstr[i] = strmid(radec,14,11)
   ENDFOR 
END 

PRO rd_c00rad, file, idc, rarad, decrad, rmag, zspec, qual, typstr, source 
   ; file   -> name of file containing cohen et al catalog
   ; idc    <- index in cat
   ; rastr  <- RA
   ; decstr <- Dec
   ; rmag   <- R band magnitude
   ; zspec  <- spectroscopic redshifts
   ; qual   <- quality code
   ; typstr <- Type 
   ; source <- source of redshift
   readcol, file, idc, rarad, decrad, rmag, zspec, qual, typstr, source, $
    format='(i,f,f,f,f,a,a,a)'
   FOR i = 0, n_elements(idc)-1 DO BEGIN
      typstr[i] = strjoin(strsplit(typstr[i],'{}',/extract))
   ENDFOR 
END 

PRO c00grp_thead, unit, title
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
   printf,unit,'<td>ID_C00</td>' 
   printf,unit,'<td>RA<br>Dec</td>'
   printf,unit,'<td>type</td>'
   printf,unit,'<td>z</td>'
   printf,unit,'<td>R mag</td>'
   printf,unit,'</tr>'
END 

PRO c00grp_trow, unit, id, posim, aim, bim, thetaim, fwhm, magauto, class, $
                 idc, rastr, decstr, typstr, zspec, rmag, $
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
   printf,unit,'<td>',idc,'</td>'
   printf,unit,'<td>',rastr,'<br>',decstr,'</td>'
   printf,unit,'<td>',typstr,'</td>'
   printf,unit,'<td>',zspec,'</td>'
   printf,unit,'<td>',rmag,'</td>'
   printf,unit,'</tr>'

END 

PRO c00_grism_page, filhtml, title, elm, id, pos, aim, bim, thetaim, $
                w50, magauto, class, elmc, idc, rastr, decstr, rmag, zspec, typstr, $
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
   grp_pagetop, unitp, title
   c00grp_thead, unitp, title
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
         c00grp_trow, unitp, id[elm[i]], [pos[elm[i],0],pos[elm[i],1]], aim[elm[i]], bim[elm[i]], $
          thetaim[elm[i]], w50[elm[i]], magauto[elm[i]], class[elm[i]], $
          idc[elmc[i]], rastr[elmc[i]], decstr[elmc[i]], typstr[elmc[i]], zspec[elmc[i]], rmag[elmc[i]], $
          nampng(pstmp,ide), nampng(pribn,ide), nampng(pspec,ide), fdat=fdat, ffit=ffit
      ENDIF ELSE BEGIN
         c00grp_trow, unitp, id[elm[i]], [pos[elm[i],0],pos[elm[i],1]], aim[elm[i]], bim[elm[i]], $
          thetaim[elm[i]], w50[elm[i]], magauto[elm[i]], class[elm[i]], $
          idc[elmc[i]], rastr[elmc[i]], decstr[elmc[i]], typstr[elmc[i]], zspec[elmc[i]], rmag[elmc[i]], $
          nampng(pstmp,ide), nampng(pribn,ide), nampng(pspec,ide)
      ENDELSE
   ENDFOR
   ;
   grp_tfoot, unitp
   c00grp_pagebot, unitp
   free_lun, unitp
   
END

PRO c00_matchcat, file, id, posim, aim, bim, thetaim, w50, magauto, class,  $
       idc, rastr, decstr, rmag, zspec, typstr
   ;
   ; make ascii catalog of matched cohen et al list
   fmt = '(i4,f8.1,f8.1,f6.1,f6.1,f7.1,f6.1,f7.2,f7.3,i6,2x,a11,1x,a11,f6.2,f7.3,a4)'
   openw,luno,file,/get_lun
   printf,luno,'# id  x_im    y_im      a     b   theta  fwhm magauto class  c00   <-- RA ---> <-- Dec --> Rmag    z    typ'
   for i = 0, n_elements(id)-1 DO $
    printf,luno,id[i],posim[i,0],posim[i,1],aim[i],bim[i],thetaim[i],w50[i],magauto[i],class[i], $
                idc[i],rastr[i],decstr[i],rmag[i],zspec[i],typstr[i],format=fmt
   close,luno
END 

PRO c00grp_pagebot, unit
   date=systime()
   printf,unit,'<p><small>Page generated automatically with ' $
    +'hdfn_cohen.pro on '$
    +date+'<br>'
   printf,unit,'Page maintanined by <a href=mailto:meurer@pha.jhu.edu>Gerhardt Meurer</a></small>'
   printf,unit,'</BODY>'
   printf,unit,'</HTML>'
END 


PRO hdfn_cohen
   ;
   ; Make pages of grism plots for HDFN galaxies in the
   ; spectroscpic list of Cohen et al (2000)
   ;
   workdir  = '/data3/acs27/meurer/grism/hdfn/axe/plots/'
   matchdir = '/data3/acs27/meurer/grism/hdfn/axe/c00match/'
   c00cat   = matchdir + 'c00_all.dat'
   c00rad   = matchdir + 'c00_all_radians.dat'
   dircat   = matchdir + 'direct_0209.cat'
   matcha   = matchdir + 'matched_0209a.mtA'  ; this should be c00 cat
   matchb   = matchdir + 'matched_0209a.mtB'  ; this should be direct cat
   pgpfx    = 'c00_'
   titlepfx = 'Grism spectra of Cohen et al. (2000) matches - page : '
   matchcat = workdir + 'c00_direct_matched.cat'
   pngmag   = workdir + 'c00_magmatch.png'
   cd,workdir
   ;
   setplotcolors
   ;
   ; Read catalogs
   rd_grdirct_cat, dircat, id, xim, yim, magauto, aim, bim, thetaim, w50, class
   rd_c00cat, c00cat, idc, rastr, decstr, rmag, zspec, qual, typstr, source 
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
   match, idc, ida, subc, suba
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
   k           = sort(subb)
   subb        = temporary(subb[k])
   sub         = temporary(sub[k])
   k           = sort(suba)
   suba        = temporary(suba[k])
   subc        = temporary(subc[k])
   ;
   ; Make web pages of matched spectra
   posim       = [[xim], [yim]] 
   pstmp       = 'stmp_'
   pribn       = 'ribn_'
   pspec       = 'spec_'
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
      c00_grism_page, filhtml, title, elm, id, posim, aim, bim, thetaim, $
       w50, magauto, class,  elmc, idc, rastr, decstr, rmag, zspec, typstr, $
       pstmp, pspec, pribn
      npage    = npage + 1
      i0       = i1 + 1
      print, ' '
      print, ' completed page : ', filhtml
      print, ' '
   ENDWHILE 
   ;
   ; save catalog
   c00_matchcat, matchcat, id[sub], posim[sub,*], aim[sub], bim[sub], thetaim[sub], $
       w50[sub], magauto[sub], class[sub], idc[subc], rastr[subc], decstr[subc], $
       rmag[subc], zspec[subc], typstr[subc]   
   ; Make some plots ?
   window,0,xsize=400,ysize=400
   plot,rmag[subc],magauto[sub],psym=1,xtitle='Rmag (Cohen)',ytitle='Magauto (direct image)',$
    xrange=[16,28],yrange=[16,28],xstyle=1,ystyle=1
   makepng,pngmag,/color
   keywait,'press any key to continue'
   plot,xa,ya,psym=1
   oplot,xb,yb,psym=6
END

PRO hdfn_cohen_sep02b
   ;
   ; Make pages of grism plots for HDFN galaxies in the
   ; spectroscpic list of Cohen et al (2000)
   ;
   workdir  = '/data3/acs27/meurer/grism/hdfn/axe/plots/'
   matchdir = '/data3/acs27/meurer/grism/hdfn/axe/c00match/'
   c00cat   = matchdir + 'c00_all.dat'
   c00rad   = matchdir + 'c00_all_radians.dat'
   dircat   = matchdir + 'j8eb01g800l_drz_sci_1.cat'
   matcha   = matchdir + 'matched_0209b.mtA'  ; this should be c00 cat
   matchb   = matchdir + 'matched_0209b.mtB'  ; this should be direct cat
   pgpfx    = 'c00_'
   titlepfx = 'Grism spectra of Cohen et al. (2000) matches - page : '
   matchcat = workdir + 'c00_direct_matched.cat'
   pngmag   = workdir + 'c00_magmatch.png'
   psmag    = workdir + 'c00_magmatch.ps'
   filgrim  = '/data3/acs27/meurer/grism/hdfn/axe/j8eb01g800l_drz_sci_1.SPC.fits'
   filsens  = '/data3/acs27/meurer/grism/sensG800Lwfc_gd153_ave.fits'
   pscii    = 'spec_c00_'
   lrange   = [5800.0, 9400.0]
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
   rd_c00cat, c00cat, idc, rastr, decstr, rmag, zspec, qual, typstr, source 
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
   match, idc, ida, subc, suba
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
   ; now do a spectroscopic redshift sort
   k           = sort(zspec[subc])
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
      grism_outascii, fdat, bintab, magauto[sub[i]], zspec[subc[i]], xim[sub[i]], yim[sub[i]], lamlim=lrange
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
      c00_grism_page, filhtml, title, elm, id, posim, aim, bim, thetaim, $
       w50, magauto, class,  elmc, idc, rastr, decstr, rmag, zspec, typstr, $
       pstmp, pspec, pribn, pscii=pscii
      npage    = npage + 1
      i0       = i1 + 1
      print, ' '
      print, ' completed page : ', filhtml
      print, ' '
   ENDWHILE 
   ;
   ; save catalog
   c00_matchcat, matchcat, id[sub], posim[sub,*], aim[sub], bim[sub], thetaim[sub], $
       w50[sub], magauto[sub], class[sub], idc[subc], rastr[subc], decstr[subc], $
       rmag[subc], zspec[subc], typstr[subc]   
   ; Make some plots ?
   window,0,xsize=400,ysize=400
   plot,rmag[subc],magauto[sub],psym=1,xtitle='Rmag (Cohen)',ytitle='Magauto (direct image)',$
    xrange=[16,28],yrange=[16,28],xstyle=1,ystyle=1
   makepng,pngmag,/color
   psl
   plot,rmag[subc],magauto[sub],psym=1,xtitle='Rmag (Cohen)',ytitle='Magauto (direct image)',$
    xrange=[16,28],yrange=[16,28],xstyle=1,ystyle=1
   psend,psmag
   setbgfg,!white,!black
   ; keywait,'press any key to continue'
   ; plot,xa,ya,psym=1
   ; oplot,xb,yb,psym=6
END

PRO hdfn_cohen_dec02b
   ;
   ; Make pages of grism plots for HDFN galaxies in the
   ; spectroscpic list of Cohen et al (2000)
   ;
   ;workdir  = '/data3/acs27/meurer/grism/hdfn_new/axe_all/plots/'
   ;matchdir = '/data3/acs27/meurer/grism/hdfn_new/axe_all/c00match/'
   ;c00cat   = matchdir + 'c00_all.dat'
   ;c00rad   = matchdir + 'c00_all_radians.dat'
   ;dircat   = matchdir + 'j8eb01g800l_drz_sci_1.cat'
   ;matcha   = matchdir + 'matched_0212a.mtA'  ; this should be c00 cat
   ;matchb   = matchdir + 'matched_0212a.mtB'  ; this should be direct cat
   ;pgpfx    = 'c00_'
   ;titlepfx = 'Grism spectra of Cohen et al. (2000) matches - page : '
   ;matchcat = workdir + 'c00_direct_matched.cat'
   ;pngmag   = workdir + 'c00_magmatch.png'
   ;psmag    = workdir + 'c00_magmatch.ps'
   ;filgrim  = '/data3/acs27/meurer/grism/hdfn/axe/j8eb01g800l_drz_sci_1.SPC.fits'
   ;filsens  = '/data3/acs27/meurer/grism/sensG800Lwfc_gd153_ave.fits'
   ;pscii    = 'spec_c00_'
   ;pgfit    = 'spec_gfit_'
   ;lrange   = [5800.0, 9800.0]
   ;
   workdir  = '/data2/acs27/meurer/grism/hdfn_dec02/Axe/output/plots/'
   matchdir = '/data2/acs27/meurer/grism/hdfn_dec02/Axe/C00match/'
   c00cat   = matchdir + 'c00_all.dat'
   c00rad   = matchdir + 'c00_all_radians.dat'
   dircat   = matchdir + 'hdfn_g800l_2.cat'
   matcha   = matchdir + 'matched.mtA'  ; this should be c00 cat
   matchb   = matchdir + 'matched.mtB'  ; this should be direct cat
   pgpfx    = 'c00_'
   titlepfx = 'Grism spectra of Cohen et al. (2000) matches - page : '
   matchcat = workdir + 'c00_direct_matched.cat'
   pngmag   = workdir + 'c00_magmatch.png'
   psmag    = workdir + 'c00_magmatch.ps'
   filgrim  = '/data2/acs27/meurer/grism/hdfn_dec02/Axe/output/hdfn_g800l_2.SPC.fits'
   filsens  = '/home/meurer/acs/axe/conf/ACS.WFC.1st.sens.fits' 
   extsens  = 1
   pscii    = 'spec_c00_'
   pgfit    = 'spec_gfit_'
   lrange   = [5800.0, 9800.0]
   ;
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
   rd_c00cat, c00cat, idc, rastr, decstr, rmag, zspec, qual, typstr, source 
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
   match, idc, ida, subc, suba
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
   ; now do a spectroscopic redshift sort
   k           = sort(zspec[subc])
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
      skip      = i - old - 1
      print,i,old,idb[i],skip
      IF skip LT 0 THEN BEGIN 
         free_lun, lun
         lun    = fxposit(filgrim, idb[i])
         bintab = kludge_mrdfits(lun,0)
      ENDIF ELSE BEGIN 
         bintab = kludge_mrdfits(lun,skip)
      ENDELSE 
      old    = i
      fdat   = pscii + trim(string(idb[i]),2) + '.dat'
      grism_updflux, bintab, exptime=exptime
      grism_outascii, fdat, bintab, magauto[sub[i]], zspec[subc[i]], xim[sub[i]], yim[sub[i]], lamlim=lrange
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
      c00_grism_page, filhtml, title, elm, id, posim, aim, bim, thetaim, $
       w50, magauto, class,  elmc, idc, rastr, decstr, rmag, zspec, typstr, $
       pstmp, pspec, pribn, pscii=pscii, pgfit=pgfit
      npage    = npage + 1
      i0       = i1 + 1
      print, ' '
      print, ' completed page : ', filhtml
      print, ' '
   ENDWHILE 
   ;
   ; save catalog
   c00_matchcat, matchcat, id[sub], posim[sub,*], aim[sub], bim[sub], thetaim[sub], $
       w50[sub], magauto[sub], class[sub], idc[subc], rastr[subc], decstr[subc], $
       rmag[subc], zspec[subc], typstr[subc]   
   ; Make some plots ?
   window,0,xsize=400,ysize=400
   plot,rmag[subc],magauto[sub],psym=1,xtitle='Rmag (Cohen)',ytitle='Magauto (direct image)',$
    xrange=[16,28],yrange=[16,28],xstyle=1,ystyle=1
   makepng,pngmag,/color
   ; psl
   plot,rmag[subc],magauto[sub],psym=1,xtitle='Rmag (Cohen)',ytitle='Magauto (direct image)',$
    xrange=[16,28],yrange=[16,28],xstyle=1,ystyle=1
   ; psend,psmag
   setbgfg,!white,!black
   ; keywait,'press any key to continue'
   ; plot,xa,ya,psym=1
   ; oplot,xb,yb,psym=6
END
