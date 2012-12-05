PRO singg_prods_html, inlist, fra, frb, frc, comfile, outpage, title
   ;
   ; Marks up SINGG products for delivery via the web.
   ;
   ; inlist  -> text file giving inputs
   ;  col1   :  long name
   ;  col2   :  shortname 
   ;  col3   :  Net image name
   ;  col4   :  R image name
   ;  col5   :  NB image nam
   ; fra     -> rebin factors used when making linear png files.
   ; frb     -> rebin factors used when making sqrt png files.
   ; frc     -> rebin factors used when making log png files.
   ; title   -> title for output page
   ; outpage -> name of output webpage
   ;
   ; G. Meurer, 04/2003
   ;
   subtitle = 'Data products'
   ;
   readcol, inlist, naml, nams, filnet, filcnt, filnb, format='(a,a,a,a,a)'
   nr       = n_elements(naml)
   ;
   nfra1    = n_elements(fra)-1
   nfrb1    = n_elements(frb)-1
   nfrc1    = n_elements(frc)-1
   expstra  = strtrim(string(max(fra)),2)
   expstrb  = strtrim(string(max(frb)),2)
   expstrc  = strtrim(string(max(frc)),2)
   ;
   openw, lp, outpage, /get_lun
   singg_pagetop, lp, title, subtitle
   ;
   printf,lp,'<H3>Preview images</h3>'
   printf,lp,'<p>'
   printf,lp,'The three color preview images show net line emission in red, the narrow'
   printf,lp,'band image in green, and the continuum (R) image in blue.  Three'
   printf,lp,'stretched are shown - linear, sqrt, and log reaching progressively'
   printf,lp,'higher maximum display limits.  The limits are the same in terms of flux'
   printf,lp,'or flux density for all sources.  The numbered links under the inline'
   printf,lp,'images are to the preview images at five different rebinning factors'
   printf,lp,'(the largest rebinning corresponds to the inline image). The number'
   printf,lp,'refers to the rebinning factor.'
   printf,lp,'</p>'
   ;
   printf,lp,'<p><a href="'+comfile+'">Comments on individual sources</a></p>'
   ;
   ; Table header
   ncol=5
   printf,lp,'<p><Table border=1 cellpadding=3>'
   printf,lp,'<tr bgcolor="#a8a8a8">'
   printf,lp,'  <th colspan='+strtrim(string(ncol),2)+' bgcolor=#a8a8a8><center>'
   printf,lp,title+'<br>'+subtitle+'</center>'
   printf,lp,'</tr>'
   printf,lp,'<tr bgcolor="#b0b0ff">'
   printf,lp,'  <th>HIPASS+</th>'
   printf,lp,'  <th>Images</th>'
   printf,lp,'  <th colspan=3>Previews</th>'
   printf,lp,'</tr>'
   printf,lp,'<tr bgcolor="#b0b0ff">'
   printf,lp,'  <th rowspan=2>long</th>'
   printf,lp,'  <th>Net</th>'
   printf,lp,'  <th rowspan=3>Linear</th>'
   printf,lp,'  <th rowspan=3>SQRT</th>'
   printf,lp,'  <th rowspan=3>Log</th>'
   printf,lp,'</tr>'
   printf,lp,'<tr bgcolor="#b0b0ff">'
   printf,lp,'  <th>R or continuum</th>'
   printf,lp,'</tr>'
   printf,lp,'<tr bgcolor="#b0b0ff">'
   printf,lp,'  <th>short</th>'
   printf,lp,'  <th>Narrow band</th>'
   printf,lp,'</tr>'
   printf,lp,'<tr bgcolor="#b0b0ff">'
   ;
   FOR i = 0, nr-1 DO BEGIN 
      imlin  = nams[i]+'_lin'+expstra+'.png'
      imsqrt = nams[i]+'_sqrt'+expstrb+'.png'
      imlog  = nams[i]+'_log'+expstrc+'.png'
      navstrlin  = '<br>'
      navstrsqrt = '<br>'
      navstrlog  = '<br>' 
      FOR j = 0, nfra1 DO BEGIN 
         estr = strtrim(string(fra[j]),2)
         IF strlen(navstrlin)  GT 4 THEN navstrlin  = navstrlin  + ' , '
         navstrlin  = navstrlin+'<a href="'+nams[i]+'_lin'+estr+'.png">'+estr+'</a>'
      ENDFOR 
      FOR j = 0, nfrb1 DO BEGIN 
         estr = strtrim(string(frb[j]),2)
         IF strlen(navstrsqrt) GT 4 THEN navstrsqrt = navstrsqrt + ' , '
         navstrsqrt = navstrsqrt+'<a href="'+nams[i]+'_sqrt'+estr+'.png">'+estr+'</a>'
      ENDFOR 
      FOR j = 0, nfrc1 DO BEGIN 
         estr = strtrim(string(frc[j]),2)
         IF strlen(navstrlog)  GT 4 THEN navstrlog  = navstrlog  + ' , '
         navstrlog  = navstrlog+'<a href="'+nams[i]+'_log'+estr+'.png">'+estr+'</a>'
      ENDFOR 
      printf,lp,'<tr>'
      printf,lp,'  <td bgcolor=#d0d0ff rowspan=2>'+naml[i]+'</td>'
      printf,lp,'  <td bgcolor=#d0d0ff><a href="'+strtrim(filnet[i],2)+'">'+filnet[i]+'</a></td>'
      printf,lp,'  <td bgcolor=#d0d0ff rowspan=3><img src="'+imlin+'">'+navstrlin+'</td>'
      printf,lp,'  <td bgcolor=#d0d0ff rowspan=3><img src="'+imsqrt+'">'+navstrsqrt+'</td>'
      printf,lp,'  <td bgcolor=#d0d0ff rowspan=3><img src="'+imlog+'">'+navstrlog+'</td>'
      printf,lp,'</tr>'
      printf,lp,'<tr>'
      printf,lp,'  <td bgcolor=#d0d0ff><a href="'+strtrim(filcnt[i],2)+'">'+filcnt[i]+'</a></td>'
      printf,lp,'</tr>'
      printf,lp,'<tr>'
      printf,lp,'  <td bgcolor=#d0d0ff>'+nams[i]+'</td>'
      printf,lp,'  <td bgcolor=#d0d0ff><a href="'+strtrim(filnb[i],2)+'">'+filnb[i]+'</a></td>'
      printf,lp,'</tr>'
   ENDFOR 
   ; 
   singg_tfoot, lp
   date=systime()
   printf,lp,'<p><small>Page generated automatically with ' $
    +'singg_prods_html.pro</a> on '$
    +date+'<br>'
   printf,lp,'Page maintanined by <a href=mailto:meurer@pha.jhu.edu>Gerhardt Meurer</a></small>'
   printf,lp,'</BODY>'
   printf,lp,'</HTML>'
   free_lun, lp
END 
