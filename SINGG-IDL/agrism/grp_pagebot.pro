PRO grp_pagebot, unit
   date=systime()
   printf,unit,'<p><small>Page generated automatically with ' $
    +'grism_page.pro on '$
    +date+'<br>'
   printf,unit,'Page maintanined by <a href=mailto:meurer@pha.jhu.edu>Gerhardt Meurer</a></small>'
   printf,unit,'</BODY>'
   printf,unit,'</HTML>'
END 

