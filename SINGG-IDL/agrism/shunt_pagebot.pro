PRO shunt_pagebot, unit
   date=systime()
   printf,unit,'<p><small>Page generated automatically with ' $
    +'shunt.pro on '$
    +date+'<br>'
   printf,unit,'Page maintanined by Gerhardt Meurer (meurer -at- pha.jhu.edu)</small>'
   printf,unit,'</BODY>'
   printf,unit,'</HTML>'
END 

