PRO singg_tnavbar, unit, tableno, maxtabs, sourcefile
   navbar = '<p>Pages: '
   IF tableno GT 1 THEN BEGIN 
      fname = 'sample'+strn(tableno-1,length=3,padchar='0')+'.html'
      navbar = navbar+'[<a href="'+fname+'">&lt;prev ('+strn(tableno-1)+')</a>] '
   ENDIF 
   navbar = navbar+'[<a href="index.html">Index</a>] '
   IF tableno LT maxtabs THEN BEGIN 
      fname = 'sample'+strn(tableno+1,length=3,padchar='0')+'.html'
      navbar = navbar+'[<a href="'+fname+'">next ('+strn(tableno+1)+')&gt;</a>] '
   ENDIF 
   FOR i = 1, maxtabs DO BEGIN 
      fname = 'sample'+strn(i,length=3,padchar='0')+'.html'
      navbar = navbar+'[<a href="'+fname+'">'+strn(i)+'</a>] '
      ENDFOR 
   navbar = navbar+'[<a href="'+sourcefile+'">Text file (all sources)</a>]</p>'
   printf,unit,navbar
END 

