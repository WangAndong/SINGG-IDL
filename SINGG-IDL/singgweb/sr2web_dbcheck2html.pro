PRO sr2web_dbcheck2html, lu, fdbcheck
   ;
   ; Markup a _dbcheck.out file into an html table
   ;
   ; G. Meurer 12/2006
   ;
   ; setup stuff
   prog  = 'sr2web_dbcheck2html.pro'
   prog2 = 'SR2WEB_DBCHECK2HTML: '
   fmtd  = '(i,a,l,a,f,f,a,a,f,f,f,f,i)'
   ;
   ; check if file exists
   finfo = file_info(fdbcheck)
   IF NOT finfo.exists THEN BEGIN 
      print,prog2,'WARNING file does not exist : '+fdbcheck
      print,prog2,'making note in HTML'
      printf,lu,'<P>No dbcheck output file to markup...</P>'
      return
   ENDIF 
   ;
   ; read fdbcheck
   print,prog2+'reading file : '+fdbcheck
   readcol, fdbcheck, num, dbnam, ent, source, xpix, ypix, rastr, decstr, $
            sma, pa, ab, farea, vprob, format=fmtd, comment='#'
   nd    = n_elements(num)
   ;
   ; write top of table
   print,prog2+'writing html table'
   printf,lu,'<P><TABLE border=1 cellpadding=3>'
   printf,lu,'<TR bgcolor=#7070ff>'
   printf,lu,'   <TH align="center" colspan=10>Results of database checks</TH>'
   printf,lu,'</TR>'
   printf,lu,'<TR bgcolor=#7070ff>'
   printf,lu,'   <TH>#</TH>'
   printf,lu,'   <TH>database</TH>'
   printf,lu,'   <TH>entry</TH>'
   printf,lu,'   <TH>source</TH>'
   printf,lu,'   <TH>X<sub>pix</sub><br>Y<sub>pix</sub></TH>'
   printf,lu,'   <TH>RA<br>Dec</TH>'
   printf,lu,'   <TH>a (arcsec)</TH>'
   printf,lu,'   <TH>a/b<br>PA</TH>'
   printf,lu,'   <TH>f<sub>area</sub></TH>'
   printf,lu,'   <TH>V-test</TH>'
   printf,lu,'</TR>'
   FOR kk = 0, nd-1 DO BEGIN 
      printf,lu,'<TR bgcolor=#c0c0ff>'
      printf,lu,'   <TD valign="middle" align="center">'+strtrim(string(num[kk]),2)+'</TD>'
      printf,lu,'   <TD valign="middle" align="center">'+strtrim(dbnam[kk],2)+'</TD>'
      printf,lu,'   <TD valign="middle" align="center">'+strtrim(string(ent[kk]),2)+'</TD>'
      printf,lu,'   <TD valign="middle" align="center">'+strtrim(source[kk],2)+'</TD>'
      printf,lu,'   <TD align="center">'+strtrim(string(xpix[kk]),2)+'<br>'+strtrim(string(ypix[kk]),2)+'</TD>'
      printf,lu,'   <TD align="center">'+strtrim(rastr[kk],2)+'<br>'+strtrim(decstr[kk],2)+'</TD>'
      printf,lu,'   <TD valign="middle" align="center">'+strtrim(string(sma[kk]),2)+'</TD>'
      printf,lu,'   <TD align="center">'+strtrim(string(ab[kk]),2)+'<br>'+strtrim(string(pa[kk]),2)+'</TD>'
      printf,lu,'   <TD valign="middle" align="center">'+strtrim(string(farea[kk]),2)+'</TD>'
      printf,lu,'   <TD valign="middle" align="center">'+strtrim(string(vprob[kk]),2)+'</TD>'
      printf,lu,'</TR>'
   ENDFOR 
   printf,lu,'</P></TABLE>'
   print,prog2+'finished.'
END 
