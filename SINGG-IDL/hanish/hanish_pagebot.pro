PRO hanish_pagebot, unit
   ;
   ; 10/2005 D. Hanish copy from Meurer ?
   ; 09/2007 G. Meurer renamed from singg_pagebot
   ;                   changed some OF the hardwiring...
   date=systime()
   printf,unit,'<p><small>Page generated automatically with ' $
    +'<a href="singg_qa.pro">singg_qa.pro</a> on '$
    +date+'<br>'
   printf,unit,'Page maintanined by Gerhardt Meurer (meurer -at- pha.jhu.edu)</small>'
   printf,unit,'</BODY>'
   printf,unit,'</HTML>'
END 

