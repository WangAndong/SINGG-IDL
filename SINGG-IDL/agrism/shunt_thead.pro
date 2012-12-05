PRO shunt_thead, unit, title, dodstamp=dodstamp
   ;
   ; table header for shunt html output
   ; unit     -> unit for output
   ; title    -> title of page
   ; dodtsamp -> if set, amke a column for direct image stampsi
   ;
   ; G. Meurer 09/2005
   IF keyword_set(dodstamp) THEN ncol = 4 ELSE ncol = 3
   printf,unit,'<P><TABLE border=1 cellpadding=3>'
   printf,unit,'<th colspan='+strtrim(string(ncol),2)+' bgcolor=a8a8a8><center>'
   printf,unit,title+'<br>'
   printf,unit,'</center></th>'
   printf,unit,'<tr bgcolor="#a8a8a8" valign=middle>'
   printf,unit,'<th>ID</th>'
   printf,unit,'<th>squashed im. position<br>classification<br>Instr. mag<br>Est. wcs position</th>'
   printf,unit,'<th>Grism images and plots</th>'
   IF keyword_set(dodstamp) THEN printf,unit,'<th>direct image stamp</th>'
   printf,unit,'</tr>'
END 

