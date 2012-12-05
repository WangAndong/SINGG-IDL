function hipass_ascii_spec_url, cube, rastr, decstr
   ;
   ; return the URL to retrieve an ascii spectrum from the
   ; HIPASS database
   ;
   ; cube   -> number of HIPASS cube, an integer from 0 to 999
   ; rastr  -> right ascension in sexigessimal with colon (:)
   ;           separators
   ; decstr -> declination in sexigessimal...
   ;
   ; G. Meurer 7/2007
   s1   = 'http://www.atnf.csiro.au/cgi-bin/multi/release/download.cgi?cubename=/DATA/MULTI_3/HIDE/PUBLIC/H'
   s2   = '_abcde_luther.FELO.imbin.vrd&hann=1&coord='
   s3   = '%2C'
   s4   = '&xrange=-1281%2C12726&xaxis=optical&datasource=hipass&type=ascii'
   cln  = '%3A'
   ;
   nn   = n_elements(cube)
   nc   = alog10(float(cube))
   cstr = strtrim(string(cube),2)
   jj   = where(nc LT 2.0, njj)
   IF njj GT 0 THEN cstr[jj] = '0'+cstr[jj]
   jj   = where(nc LT 1.0, njj)
   IF njj GT 0 THEN cstr[jj] = '0'+cstr[jj]
   rah  = strmid(rastr,0,2)
   ram  = strmid(rastr,3,2)
   ras  = strmid(rastr,6)
   dcd  = strmid(decstr,0,3)
   dcm  = strmid(decstr,4,2)
   dcs  = strmid(decstr,7)
   jj   = where(strmid(dcd,0,1) EQ '+', njj)
   IF njj GT 0 THEN dcd[jj] = strmid(dcd[jj],1,2)
   ;
   url  = s1+cstr+s2+rah+cln+ram+cln+ras+s3+dcd+cln+dcm+cln+dcs+s4
   return, url
END 
