PRO singg_indhead, unit, nminicol, title, sourcefile
   dbtar = 'singg_sample_db.tar'
   test = file_search(dbtar, count=nc)
   printf,unit,'<P><TABLE border=1 cellpadding=5>'
   printf,unit,'<th colspan='+strn(nminicol+1)+' bgcolor=a8a8a8><center>'
   printf,unit,title+'<br><a href="'+sourcefile+'">Text file (all sources)</a><br>'
   IF nc EQ 1 THEN printf,unit,'<a href="'+dbtar+'">IDL database files (tar archive)</a><br>'
   printf,unit,"<small>Click on the object Name for a 20'x20' DSS finder chart.<br>"
   printf,unit,"Use link in first column for summary table.</small>"
   printf,unit,'</center></th>'
   printf,unit,'<tr bgcolor="#a8a8a8">'
   printf,unit,'<td><center><b>Summary<br>Table</b></center></td>'
   printf,unit,'<td colspan='+strn(nminicol)+'><center><b>Objects</b><center></td>'
   printf,unit,'</tr>'
END 

