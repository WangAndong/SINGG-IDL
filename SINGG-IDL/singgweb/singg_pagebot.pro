PRO singg_pagebot, unit, prog=prog
   ;
   ; G. Meurer, written, a long, long, time ago
   ; G. Meurer, 12/2006 updated to allow calling prog name to be passed. 
   ;                    only obliquely give my email adxdress.
   ; G. Meurer, 08/2007 inserted missing "return" statement
   ;
   date=systime()
   pname = 'singg_genweb.pro'
   IF keyword_set(prog) THEN pname = strtrim(prog,2)
   printf,unit,'<p><small>Page generated automatically with ' $
    +'<a href="'+pname+'">'+pname+'</a> on '$
    +date+'<br>'
   printf,unit,'Page maintanined by Gerhardt Meurer (meurer -at- icrar.org)</small>'
   printf,unit,'</BODY>'
   printf,unit,'</HTML>'
   return
END 

