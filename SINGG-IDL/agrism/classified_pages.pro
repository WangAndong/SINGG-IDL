PRO read_emcat, cat, id, xim, yim, mag, a, b, theta, w50, class, radeg, decdeg, minsn, maxsn, nord0, lin, qual, cen, width, flux
   readcol, cat, id, xim, yim, mag, a, b, theta, w50, class, radeg, decdeg, minsn, maxsn, nord0, lin, qual, cen, width, flux, $
   format = '(i5,f8.1,f8.1,f7.2,f6.1,f6.1,f7.1,f7.2,f7.3,f11.5,f11.5,f7.1,f7.1,i6,i4,i4,f8.1,f7.1,f10.2,)' 
END 

PRO emcat_thead, unit, title
   hdcol = '"#a8a8a8"'
   printf,unit,'<P><TABLE border=1 cellpadding=3>'

   printf,unit,'<th colspan=10 bgcolor='+hdcol+'><center>'+title+'</center></th>'

   printf,unit,'<tr bgcolor='+hdcol+'>'
   printf,unit,'<td>ID</td>'
   printf,unit,'<td>X<sub>image</sub><br>Y<sub>image</sub><br>RA<br>Dec<br>Mag<sub>auto</sub><br>W<sub>50</sub><br>Class</td>'
   printf,unit,'<td>no.</td>'
   printf,unit,'<td>center</td>'
   printf,unit,'<td>FWHM</td>'
   printf,unit,'<td>Flux</td>'
   printf,unit,'<td>Flag</td>'
   printf,unit,'<td>Stamp<br>Ribbon</td>'
   printf,unit,'<td>Spectrum</td>'
   printf,unit,'<td>Data</td>'
   printf,unit,'</tr>'
END 

PRO emcat_trow, unit, id, xim, yim, rastr, decstr, mag, w50, class, $
                lin, cen, width, flux, nord0, qual, $
                fstmp, fribn, ffit, fdat
   bgcol = "#c0c0e0"
   td0   = '<td>'
   td1   = '</td>'
   br    = '<br>'
   nl    = n_elements(qual)
   qstr  = make_array(nl, /string)
   FOR i = 0, nl-1 DO BEGIN 
      IF qual[i] NE 1 THEN qstr[i] = '* ' ELSE qstr[i] = ''
      IF nord0[i] NE 0 THEN qstr[i] = qstr[i]+'C0 ' 
      IF strtrim(qstr[i],2) EQ '' THEN qstr[i] = '&nbsp;'
   ENDFOR 
   printf,unit,'<tr bgcolor='+bgcol+'>'
   printf,unit,td0,id,td1
   printf,unit,td0,xim,br,yim,br,rastr,br,decstr,br,mag,br,w50,class,td1
   printf,unit,html_cell_arr(lin)
   printf,unit,html_cell_arr(cen)
   printf,unit,html_cell_arr(width)
   printf,unit,html_cell_arr(flux)
   printf,unit,html_cell_arr(qstr)
   printf,unit,td0,'<img src="'+fstmp+'">','<br>&nbsp;<br>','<img src="'+fribn+'">',td1
   printf,unit,td0,'<img src="'+ffit+'">',td1
   printf,unit,td0,'<a href="',+fdat+'">Data',td1
END

PRO emcat_html, emcat, fhtml, title
   ;
   ; Mark up a grism emission line catalog into html
   pstmp    = 'stmp_'
   pgfit    = 'spec_gfit_'
   pribn    = 'ribn_'
   pscii    = 'spec_'
   program  = 'emcat_html.pro'
   author   = 'Gerhardt R. Meurer'
   email    = 'meurer@pha.jhu.edu'
   ;
   ; read catalog, get unique id numbers
   read_emcat, emcat, id, xim, yim, mag, a, b, theta, w50, class, radeg, decdeg, minsn, maxsn, nord0, lin, qual, cen, width, flux
   uniqid = id(uniq(id, sort(id)))
   nuniq  = n_elements(uniqid)
   ;
   ; open html file, write header, table top
   openw, lh, fhtml, /get_lun
   html_pagetop, lh, title, bgcolor='"#e0e0ff"'
   printf, lh, '<p><a href='+emcat+'>ASCII catalog</a></p>'
   emcat_thead, lh, title
   ;
   ; fill table object by object
   FOR i = 0, nuniq-1 DO BEGIN 
      l      = where(id EQ uniqid[i],nl)
      id_    = id[l[0]]
      xim_   = xim[l[0]]
      yim_   = yim[l[0]]
      mag_   = mag[l[0]]
      w50_   = w50[l[0]]
      class_ = class[l[0]]
      rdstr  = adstring(radeg[l[0]], decdeg[l[0]], 2)
      rastr  = strtrim(strmid(rdstr,0,12),2)
      decstr = strtrim(strmid(rdstr,13),2)
      lin_   = lin[l]
      cen_   = cen[l]
      width_ = width[l]
      flux_  = flux[l]
      nord0_ = nord0[l]
      qual_  = qual[l]
      emcat_trow, lh, id_, xim_, yim_, rastr, decstr, mag_, w50_, class_, $
       lin_, cen_, width_, flux_, nord0_, qual_, $
       nampng(pstmp,id_), nampng(pribn,id_), nampng(pgfit,id_), namdat(pscii,id_)
   ENDFOR 
   ;
   ; finish table
   html_tfoot, lh
   ;
   ; put in a link to the acsii table
   printf, lh, '<p><a href='+emcat+'>ASCII catalog</a></p>'
   ;
   html_pagebot, lh, program, author, email=email
   ;
   free_lun, lh
END 

PRO read_msnspucat, cat, id, xim, yim, mag, a, b, theta, w50, class, radeg, decdeg, minsn, maxsn, qual
   readcol, cat, id, xim, yim, mag, a, b, theta, w50, class, radeg, decdeg, minsn, maxsn, qual, $
    format='(i5,f8.1,f8.1,f7.2,f6.1,f6.1,f7.1,f7.2,f7.3,f11.5,f11.5,f7.1,f7.1,i6)'
END 
 
PRO msnspucat_thead, unit, title
   hdcol = '"#a8a8a8"'
   printf,unit,'<P><TABLE border=1 cellpadding=3>'

   printf,unit,'<th colspan=10 bgcolor='+hdcol+'><center>'+title+'</center></th>'

   printf,unit,'<tr bgcolor='+hdcol+'>'
   printf,unit,'<td>ID</td>'
   printf,unit,'<td>X<sub>image</sub><br>Y<sub>image</sub><br>RA<br>Dec</td>'
   printf,unit,'<td>a<sub>image</sub><br>b<sub>image</sub><br>W<sub>50</sub><br>Theta</td>'
   printf,unit,'<td>Mag<sub>auto</sub><br>Class</td>'
   printf,unit,'<td>Stamp<br>Ribbon</td>'
   printf,unit,'<td>Spectrum</td>'
   printf,unit,'<td>Data</td>'
   printf,unit,'</tr>'
END 
 
PRO msnspucat_trow, unit, id, xim, yim, rastr, decstr, a, b, w50, theta, mag, class, $
                    fstmp, fribn, ffit, fdat
   bgcol = "#c0c0e0"
   td0   = '<td>'
   td1   = '</td>'
   printf,unit,'<tr bgcolor='+bgcol+'>'
   printf,unit,td0,id,td1
   printf,unit,html_cell_arr([strtrim(string(xim),2), strtrim(string(yim),2), rastr, decstr])
   printf,unit,html_cell_arr([a, b, w50, theta])
   printf,unit,html_cell_arr([mag, class])
   printf,unit,td0,'<img src="'+fstmp+'">','<br>&nbsp;<br>','<img src="'+fribn+'">',td1
   printf,unit,td0,'<img src="'+ffit+'">',td1
   printf,unit,td0,'<a href="',+fdat+'">Data',td1
END

PRO msnspucat_html, cat, fhtml, title
   ;
   ; Mark up non emission line catalog output from grism_classify
   pstmp    = 'stmp_'
   pspec    = 'spcf_'
   pribn    = 'ribn_'
   pscii    = 'spec_'
   program  = 'msnspucat_html.pro'
   author   = 'Gerhardt R. Meurer'
   email    = 'meurer@pha.jhu.edu'
   ;
   ; read catalog
   read_msnspucat, cat, id, xim, yim, mag, a, b, theta, w50, class, radeg, decdeg, minsn, maxsn, qual
   nid = n_elements(id)
   ;
   ; open html file, write header, table top
   openw, lh, fhtml, /get_lun
   html_pagetop, lh, title, bgcolor='"#e0e0ff"'
   printf, lh, '<p><a href='+cat+'>ASCII catalog</a></p>'
   msnspucat_thead, lh, title
   ;
   ; fill table object by object
   FOR i = 0, nid-1 DO BEGIN 
      id_    = id[i]
      xim_   = xim[i]
      yim_   = yim[i]
      a_     = a[i]
      b_     = b[i]
      w50_   = w50[i]
      theta_ = theta[i]
      mag_   = mag[i]
      class_ = class[i]
      rdstr  = adstring(radeg[i], decdeg[i], 2)
      rastr  = strtrim(strmid(rdstr,0,12),2)
      decstr = strtrim(strmid(rdstr,13),2)
      msnspucat_trow, lh, id_, xim_, yim_, rastr, decstr, a_, b_, w50_, theta_, mag_, class_, $
       nampng(pstmp,id_), nampng(pribn,id_), nampng(pspec,id_), namdat(pscii,id_)
   ENDFOR 
   ;
   ; finish table
   html_tfoot, lh
   ;
   ; put in a link to the acsii table
   printf, lh, '<p><a href='+cat+'>ASCII catalog</a></p>'
   ;
   html_pagebot, lh, program, author, email=email
   ;
   free_lun, lh
END 

