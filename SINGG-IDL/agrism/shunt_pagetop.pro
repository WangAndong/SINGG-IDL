PRO shunt_pagetop, unit, title
   ;
   ; makes top of html page.  This is a nearly verbatim copy of 
   ; grp_pagetop.pro
   ;
   printf,unit,'<HTML>'
   printf,unit,'<HEAD>'
   printf,unit,'<TITLE>'+title+'</TITLE>'
   printf,unit,'</HEAD>'
   printf,unit,'<BODY bgcolor="#c0c0c0" text="#000000" link="#400080" vlink="#C40000" alink="#00A000">'
   printf,unit,'<H1>'
   printf,unit,title
   printf,unit,'</H1>'
END 

