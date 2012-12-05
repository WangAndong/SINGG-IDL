PRO shunt_fcat_link, unit, fcat
   ;
   ; Make a link to the ascii catalog
   ;
   printf, unit, '<p>SHUNT catalog: <a href="'+strtrim(fcat,2)+'">ascii</a></p>'
END 
