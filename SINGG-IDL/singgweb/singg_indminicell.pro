PRO singg_indminicell, unit, cellno, nminicol, object, color, link
   mcolno = cellno MOD nminicol
   IF (cellno GT 0 AND mcolno EQ 0) THEN printf,unit,'<tr>' 
   IF (link) THEN printf,unit,'<TD BGCOLOR='+color+'><a href="'+object+'.html">'+object+'</a></TD>' $
    ELSE printf,unit,'<TD BGCOLOR='+color+'>'+object+'</TD>' 
   IF (mcolno EQ nminicol-1) THEN printf,unit,'</tr>' 
END 

