PRO singg_objpage, unit, colrow, objname, rastr, decstr, l, b, vhel, w50, peak, sdv, $
                   d, logmhi, filter, obs, cat, nednam, imgfound, $
                   jpgflag=jpgflag, imgdir=imgdir
   ;
   ; Write a web page for a SINGG sample object.
   IF keyword_set(jpgflag) THEN usejpg = jpgflag ELSE usejpg = 0b 
   IF usejpg THEN imgnam = objname+'.jpg' ELSE imgnam = objname+'.gif' 
   ;
   ; IF imgdir is set copy image from directory
   IF keyword_set(imgdir) THEN BEGIN 
      IF strmid(imgdir,strlen(imgdir)-1,1) NE '/' THEN dir = imgdir+'/' ELSE dir = imgdir
      found = findfile(dir+imgnam,count=nfound)
      IF nfound EQ 1 THEN BEGIN
         cmd = '/bin/cp '+dir+imgnam+' '+imgnam
         ; print,'spawning command : ',cmd
         spawn,cmd,result
      ENDIF ELSE BEGIN
         print,'**** SINGG_OBJPAGE.  Warning keyword IMGDIR set but nfound = ', nfound
      ENDELSE 
   ENDIF ELSE BEGIN 
      found = findfile(imgnam,count=nfound)
   ENDELSE 
   IF (nfound EQ 1) THEN BEGIN 
      imgfound = 1b
   ENDIF ELSE BEGIN
      imgfound = 0b
      print,'**** SINGG_OBJPAGE.  Could not find : ',imgnam
   ENDELSE 
   printf,unit,'<HTML>'
   printf,unit,'<HEAD>'
   printf,unit,'<TITLE>Finder Chart: '+objname+'</TITLE>'
   printf,unit,'</HEAD>'
   printf,unit,'<BODY BGCOLOR="#FFFFFF">'
   printf,unit,'<P>'
   printf,unit,'<img src='+imgnam+'></a>'
   printf,unit,'</P>'
   printf,unit,'<br>'
   printf,unit,'<P><TABLE border=1 cellpadding=3 bgcolor='+colrow+'>'
   printf,unit,'<tr><td><b>Property</b></td><td><b>Value</b></td><td><b>Property</b></td><td><b>Value</b></td></tr>' 
   printf,unit,'<tr><td>Name</td>               <td>'+objname+'</td>     '
   printf,unit,'    <td>NED id</td>             <td>'+nednam+'</td></tr>'  
   printf,unit,'<tr><td>Cat. source</td>        <td>'+cat+'</td>     '
   printf,unit,'    <td>Gal (l,b)</td>          <td>'+strn(fix(l+0.5))+','+strn(fix(b+0.5))+'</td></tr>'
   printf,unit,'<tr><td>RA(2000)</td>           <td>'+rastr+'</td>     '
   printf,unit,'    <td>DEC(2000)</td>          <td>'+decstr+'</td></tr>'
   printf,unit,'<tr><td>V<sub>hel</sub></td>    <td>', vhel, ' km/s </td>     ',format="(A36,F7.1,A16)"
   printf,unit,'    <td>W<sub>50</sub></td>     <td>', w50, ' km/s </td></tr>',format="(A36,F5.1,A16)"
   printf,unit,'<tr><td>S<sub>peak</sub></td>   <td>', peak, ' mJy </td>     ',format="(A36,F7.1,A15)"
   printf,unit,'    <td>Int(Sdv)</td>           <td>', sdv, ' Jy km/s </td></tr>',format="(A36,F7.1,A19)"
   printf,unit,'<tr><td>Dist</td>               <td>', d, ' Mpc </td>     ',format="(A36,F5.1,A15)"
   printf,unit,'    <td>log(M<sub>HI</sub>)</td><td>', logmhi, ' (solar units) </td></tr>',format="(A36,F6.2,A25)"
   printf,unit,'<tr><td>Best filter</td>        <td>'+filter+'</td>     '
   printf,unit,'    <td>Observed?</td>          <td>'+obs+'</td></tr>'
   printf,unit,'</TABLE></P>'
   printf,unit,''
   ;printf,fndunit,'</pre>'

end


