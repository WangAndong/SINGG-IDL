PRO sr2web_apmatch2html, lu, fapmatch
   ;
   ; markup apmatch file into an html table
   ;
   ; G. Meurer 12/2006
   ;
   ; setup stuff
   prog  = 'sr2web_apmatch2html.pro'
   prog2 = 'SR2WEB_APMATCH2HTML: '
   fmta  = '(a,l,a,f,f,a,a,f,f,f,x,a,l,a,f,f,a,a)'
   ;
   ; check if file exists
   finfo = file_info(fapmatch)
   IF NOT finfo.exists THEN BEGIN 
      print,prog2,'WARNING file does not exist : '+fapmatch
      print,prog2,'making note in HTML'
      printf,lu,'<P>No apmatch file to markup...</P>'
      return
   ENDIF 
   ; 
   ; read fapmatch
   print,prog2+'reading file : '+fapmatch
   readcol, fapmatch, db1, ent1, nam1, xpix1, ypix1, rastr1, decstr1, sma1, pa1, ab1, $
            db2, ent2, nam2, xpix2, ypix2, rastr2, decstr2, format=fmta
   nobj    = n_elements(nam1)
   ;
   ; convert ra,dec strings to decimal degrees
   print,prog2+'calculating...'
   ra1     = 15.0*sexideg(rastr1)
   dec1    = sexideg(decstr1)
   ra2     = 15.0*sexideg(rastr2)
   dec2    = sexideg(decstr2)
   ;
   ; get URL for NED search to position rastr1, decstr1
   url     = make_array(nobj, /string, value='')
   FOR ii = 0, nobj-1 DO url[ii] = nedpos_search(rastr1[ii], decstr1[ii])
   ;
   ; calculate separation in pixels and degrees
   seppix  = sqrt((xpix2 - xpix1)^2 + (ypix2 - ypix1)^2)
   gcircd, 2, ra1, dec1, ra2, dec2, sepas
   sepas   = 3600.0*sepas
   ;
   ; write table header
   print,prog2+'writing html table'
   printf,lu,'<P><TABLE border=1 cellpadding=3>'
   printf,lu,'<TR bgcolor=#707070>'
   printf,lu,'   <TH align="center" colspan=13>Results of aperture matching</TH>'
   printf,lu,'</TR>'
   printf,lu,'<TR>'
   printf,lu,'   <TH bgcolor=#7070ff>Database</TH>'
   printf,lu,'   <TH bgcolor=#7070ff>Entry</TH>'
   printf,lu,'   <TH bgcolor=#7070ff>Name</TH>'
   printf,lu,'   <TH bgcolor=#7070ff>X<sub>pix</sub><br>Y<sub>pix</sub></TH>'
   printf,lu,'   <TH bgcolor=#7070ff>RA<br>Dec</TH>'
   printf,lu,'   <TH bgcolor=#7070ff>a (arcsec)</TH>'
   printf,lu,'   <TH bgcolor=#7070ff>a/b<br>PA</TH>'
   printf,lu,'   <TH bgcolor=#70ff70>Database</TH>'
   printf,lu,'   <TH bgcolor=#70ff70>Entry</TH>'
   printf,lu,'   <TH bgcolor=#70ff70>Name</TH>'
   printf,lu,'   <TH bgcolor=#70ff70>X<sub>pix</sub><br>Y<sub>pix</sub></TH>'
   printf,lu,'   <TH bgcolor=#70ff70>RA<br>Dec</TH>'
   printf,lu,'   <TH bgcolor=#ffff70>sep (pixels)<br>sep (arcsec)</TH>'
   printf,lu,'</TR>'
   ;
   ; loop through objects 
   FOR kk = 0, nobj-1 DO BEGIN 
      ;
      ; write a row of output
      printf,lu,'<TR>'
      printf,lu,'   <TD valign="middle" align="center" bgcolor=#aaaaff>'+strtrim(db1[kk],2)+'</TD>'
      printf,lu,'   <TD valign="middle" align="center" bgcolor=#aaaaff>'+strtrim(string(ent1[kk]),2)+'</TD>'
      printf,lu,'   <TD valign="middle" align="center" bgcolor=#aaaaff>'+strtrim(nam1[kk],2)+'</TD>'
      printf,lu,'   <TD align="center" bgcolor=#aaaaff>'+strtrim(string(xpix1[kk]),2)+'<br>'+strtrim(string(ypix1[kk]),2)+'</TD>'
      printf,lu,'   <TD align="center" bgcolor=#aaaaff><a href="'+url[kk]+'">'+strtrim(rastr1[kk],2)+'<br>'+strtrim(decstr1[kk],2)+'</a></TD>'
      printf,lu,'   <TD valign="middle" align="center" bgcolor=#aaaaff>'+strtrim(string(sma1[kk]),2)+'</TD>'
      printf,lu,'   <TD align="center" bgcolor=#aaaaff>'+strtrim(string(ab1[kk]),2)+'<br>'+strtrim(string(pa1[kk]),2)+'</TD>'
      printf,lu,'   <TD valign="middle" align="center" bgcolor=#aaffaa>'+strtrim(db2[kk],2)+'</TD>'
      printf,lu,'   <TD valign="middle" align="center" bgcolor=#aaffaa>'+strtrim(string(ent2[kk]),2)+'</TD>'
      printf,lu,'   <TD valign="middle" align="center" bgcolor=#aaffaa>'+strtrim(nam2[kk],2)+'</TD>'
      printf,lu,'   <TD align="center" bgcolor=#aaffaa>'+strtrim(string(xpix2[kk]),2)+'<br>'+strtrim(string(ypix2[kk]),2)+'</TD>'
      printf,lu,'   <TD align="center" bgcolor=#aaffaa>'+strtrim(rastr2[kk],2)+'<br>'+strtrim(decstr2[kk],2)+'</TD>'
      printf,lu,'   <TD align="center" bgcolor=#ffffaa>'+strtrim(string(seppix[kk]),2)+'<br>'+strtrim(string(sepas[kk]),2)+'</TD>'
      printf,lu,'</TR>'
   ENDFOR 
   ;
   ; finish up
   printf,lu,'</P></TABLE>'
   print,prog2+'finished.'
END 
