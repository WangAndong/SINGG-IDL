PRO shunt_html_row, unit, id, xsq, ysq, spec_class, mag, ra, dec, pfx_plots, pfx_dstamp=pfx_dstamp
   ;
   ; write a table row of html output
   ;
   ; G. Meurer 09/2005
   ;
   CASE spec_class OF 
      'S' : bgcolor  = '#c0eec0'
      'A' : bgcolor  = '#c0c0ee'
      'M' : bgcolor  = '#eec0c0'
      'K' : bgcolor  = '#eec0c0'
      'B' : bgcolor  = '#eec0ee'
      'E' : bgcolor  = '#c0eeee'
      'U' : bgcolor  = '#eeeec0'
      'D' : bgcolor  = '#ffffd0'
      'O' : bgcolor  = '#eeeeee'
      ELSE : bgcolor = '#eeeeee'
   ENDCASE 
   printf,unit,'<tr bgcolor="'+bgcolor+'" valign=middle>'
   printf,unit,'<td>',id,'</td>'
   printf,unit,'<td>x<sub>sq</sub>,y<sub>sq</sub> = '+string(xsq,ysq,format='(f9.2,f9.2)')+'<br>'
   printf,unit,'    class = '+spec_class+' <br>'
   printf,unit,'    mag = '+string(mag,format='(f6.2)')+'<br>'
   printf,unit,'    &alpha;,&delta; = '+string(ra,dec,format='(f11.6,f11.6)')+'</td>'
   printf,unit,'<td><a href="'+pfx_plots+'_'+strtrim(string(id),2)+'.jpg"><img src="'+pfx_plots+'_'+strtrim(string(id),2)+'.jpg"></a> <br> <a href="'+pfx_plots+'_'+strtrim(string(id),2)+'.ps">(postscript)</a></td>'
   IF keyword_set(pfx_dstamp) THEN printf,unit,'<td><a href="'+pfx_dstamp+'_'+strtrim(string(id),2)+'.jpg"><img src="'+pfx_dstamp+'_'+strtrim(string(id),2)+'.jpg"></a> <br> <a href="'+pfx_dstamp+'_'+strtrim(string(id),2)+'.ps">(postscript)</a></td>'
END 
