PRO singg_histpl_links, unit, samppl, pfile=pfile
   tablnk = ' ' 
   IF keyword_set(pfile) THEN tablnk = ' , <a href="'+pfile+'">Table</a>'
   printf,unit,'<p>HI mass histogram: <a href="'+samppl+'">Plot</a>'+tablnk+' </p>'
END 

